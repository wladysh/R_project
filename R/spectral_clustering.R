## Spectral clustering
## Pipeline:
## 1) build affinity matrix W
## 2) build graph Laplacian L
## 3) take k eigenvectors -> embedding U
## 4) row-normalize U
## 5) run k-means on U

# --- helpers (internal) ---

sc_sqeuclid <- function(X) {
  # squared Euclidean distance matrix using ||a-b||^2 = ||a||^2 + ||b||^2 - 2 a^T b
  s <- rowSums(X^2)
  D2 <- outer(s, s, "+") - 2 * tcrossprod(X)
  D2[D2 < 0] <- 0
  D2
}

sc_choose_sigma <- function(D2) {
  vals <- sqrt(D2[upper.tri(D2)])
  sig <- stats::median(vals[is.finite(vals) & vals > 0])
  if (!is.finite(sig) || sig <= 0) sig <- 1
  sig
}

sc_affinity_rbf <- function(D2, sigma) {
  W <- exp(-D2 / (2 * sigma^2))
  diag(W) <- 0
  W
}

sc_affinity_knn <- function(D2, knn_k, sigma, weighted = TRUE, mutual = FALSE) {
  n <- nrow(D2)
  W <- matrix(0, n, n)
  
  for (i in seq_len(n)) {
    ord <- order(D2[i, ])
    nn <- ord[2:min(n, knn_k + 1)]  # skip self
    if (length(nn) > 0) {
      if (weighted) {
        W[i, nn] <- exp(-D2[i, nn] / (2 * sigma^2))
      } else {
        W[i, nn] <- 1
      }
    }
  }
  
  if (mutual) {
    # keep only mutual edges
    W <- pmin(W, t(W))
  } else {
    # union
    W <- pmax(W, t(W))
  }
  
  diag(W) <- 0
  W
}

sc_affinity_epsilon <- function(D2, epsilon, sigma = NULL, weighted = FALSE) {
  n <- nrow(D2)
  W <- matrix(0, n, n)
  
  mask <- (D2 > 0) & (D2 <= epsilon^2)
  if (weighted) {
    if (is.null(sigma)) sigma <- sc_choose_sigma(D2)
    W[mask] <- exp(-D2[mask] / (2 * sigma^2))
  } else {
    W[mask] <- 1
  }
  
  W <- pmax(W, t(W))
  diag(W) <- 0
  W
}

sc_laplacian <- function(W, type = c("sym", "unnormalized", "rw")) {
  type <- match.arg(type)
  n <- nrow(W)
  deg <- rowSums(W)
  
  if (type == "unnormalized") {
    L <- diag(deg, n, n) - W
    return(list(L = L, symmetric = TRUE))
  }
  
  if (type == "sym") {
    inv_sqrt <- 1 / sqrt(deg)
    inv_sqrt[!is.finite(inv_sqrt)] <- 0
    Dm12 <- diag(inv_sqrt, n, n)
    L <- diag(n) - Dm12 %*% W %*% Dm12
    return(list(L = L, symmetric = TRUE))
  }
  
  # random-walk: L = I - D^{-1} W (generally not symmetric)
  inv <- 1 / deg
  inv[!is.finite(inv)] <- 0
  Dm1 <- diag(inv, n, n)
  L <- diag(n) - Dm1 %*% W
  list(L = L, symmetric = FALSE)
}

sc_row_normalize <- function(U) {
  norms <- sqrt(rowSums(U^2))
  norms[norms == 0] <- 1
  U / norms
}

# --- public function ---

spectral_clustering <- function(
    X, k,
    affinity = c("rbf", "knn", "epsilon"),
    sigma = NULL,
    knn_k = 10,
    mutual = FALSE,
    epsilon = NULL,
    laplacian = c("sym", "unnormalized", "rw"),
    normalize_rows = TRUE,
    kmeans_max_iter = 100,
    kmeans_tol = 1e-6,
    seed = NULL,
    verbose = FALSE,
    return_affinity = FALSE
) {
  if (is.data.frame(X)) X <- as.matrix(X)
  stopifnot(is.matrix(X))
  stopifnot(is.numeric(X))
  stopifnot(!anyNA(X))
  
  n <- nrow(X)
  stopifnot(k >= 2, k <= n)
  
  affinity <- match.arg(affinity)
  laplacian <- match.arg(laplacian)
  
  # distances
  D2 <- sc_sqeuclid(X)
  
  # sigma heuristic (if needed)
  if (is.null(sigma) && (affinity %in% c("rbf", "knn"))) {
    sigma <- sc_choose_sigma(D2)
    if (isTRUE(verbose)) message("spectral_clustering: sigma chosen as ", signif(sigma, 4))
  }
  
  # affinity matrix
  W <- switch(
    affinity,
    rbf = sc_affinity_rbf(D2, sigma),
    knn = sc_affinity_knn(D2, knn_k = knn_k, sigma = sigma, weighted = TRUE, mutual = mutual),
    epsilon = {
      stopifnot(!is.null(epsilon), epsilon > 0)
      sc_affinity_epsilon(D2, epsilon = epsilon, sigma = sigma, weighted = FALSE)
    }
  )
  
  # Laplacian
  lap <- sc_laplacian(W, type = laplacian)
  L <- lap$L
  
  # eigen decomposition
  eig <- eigen(L, symmetric = lap$symmetric)
  vals <- Re(eig$values)
  vecs <- Re(eig$vectors)
  
  # take k smallest eigenvalues -> k eigenvectors
  idx <- order(vals, decreasing = FALSE)[seq_len(k)]
  U <- vecs[, idx, drop = FALSE]
  eigenvalues <- vals[idx]
  
  if (isTRUE(normalize_rows)) {
    U <- sc_row_normalize(U)
  }
  
  ### final clustering on embedding
  ### km <- k_means(U, K = k, max_iter = kmeans_max_iter, tol = kmeans_tol, seed = seed, verbose = FALSE)
  
  # reproducibility even if k_means has no seed arg
  if (!is.null(seed)) set.seed(seed)
  
  km_args <- list(U, K = k, max_iter = kmeans_max_iter)
  
  # pass optional args only if k_means supports them
  km_formals <- names(formals(k_means))
  if ("tol" %in% km_formals)     km_args$tol     <- kmeans_tol
  if ("seed" %in% km_formals)    km_args$seed    <- seed
  if ("verbose" %in% km_formals) km_args$verbose <- FALSE
  
  km <- do.call(k_means, km_args)
  
  
  out <- list(
    clusters = km$clusters,
    embedding = U,
    eigenvalues = eigenvalues
  )
  
  if (isTRUE(return_affinity)) out$affinity <- W
  out
}

#TO-DO:
# - andere Funktionen in NAMESPACE exportieren
# - Andere Projektteilnehmer bitten, den Namenskonflikt zu klären, 
#   und anschließend den eigenen Code bearbeiten. 
