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

#' Spectral clustering (Ng–Jordan–Weiss style embedding + k-means)
#'
#' Performs spectral clustering by constructing a graph from the input data,
#' computing a graph Laplacian, taking the eigenvectors corresponding to the
#' \eqn{k} smallest eigenvalues as an embedding, and running k-means in that
#' embedded space.
#'
#' The pipeline is:
#' \enumerate{
#'   \item Compute squared Euclidean distances \eqn{D^2}.
#'   \item Build an affinity matrix \eqn{W} (RBF / kNN / epsilon graph).
#'   \item Build a Laplacian \eqn{L} (symmetric normalized / unnormalized / random-walk).
#'   \item Compute the eigenvectors of \eqn{L} and take \eqn{k} smallest eigenpairs.
#'   \item (Optional) Row-normalize the embedding.
#'   \item Run \code{k_means()} on the embedding.
#' }
#'
#' @param X Numeric matrix or data.frame of shape \eqn{n \times p} (rows = observations).
#'   Must not contain missing values.
#' @param k Integer number of clusters. Must satisfy \code{2 <= k <= nrow(X)}.
#' @param affinity Character string specifying how to build the affinity matrix \eqn{W}.
#'   One of \code{"rbf"}, \code{"knn"}, \code{"epsilon"}.
#'   \itemize{
#'     \item \code{"rbf"}: fully-connected RBF graph \eqn{W_{ij} = exp(-||x_i-x_j||^2 / (2 sigma^2))}.
#'     \item \code{"knn"}: k-nearest-neighbor graph (union by default, or mutual if \code{mutual = TRUE}),
#'       edges weighted with the same RBF kernel.
#'     \item \code{"epsilon"}: epsilon-neighborhood graph with binary weights (1 if within \code{epsilon}).
#'   }
#' @param sigma Numeric bandwidth parameter for the RBF kernel (used for \code{"rbf"} and \code{"knn"}).
#'   If \code{NULL}, it is chosen by a median-distance heuristic based on pairwise distances.
#' @param knn_k Integer number of neighbors for \code{affinity = "knn"}.
#' @param mutual Logical. If \code{TRUE} and \code{affinity = "knn"}, keep only mutual kNN edges
#'   (intersection). If \code{FALSE}, use the union graph.
#' @param epsilon Numeric radius for \code{affinity = "epsilon"} (required in that mode).
#' @param laplacian Character string specifying the Laplacian type. One of
#'   \code{"sym"}, \code{"unnormalized"}, \code{"rw"}.
#'   \itemize{
#'     \item \code{"unnormalized"}: \eqn{L = D - W}
#'     \item \code{"sym"}: symmetric normalized Laplacian \eqn{L = I - D^{-1/2} W D^{-1/2}}
#'     \item \code{"rw"}: random-walk Laplacian \eqn{L = I - D^{-1} W}
#'   }
#' @param normalize_rows Logical. If \code{TRUE}, row-normalize the eigenvector embedding before k-means
#'   (common in Ng–Jordan–Weiss spectral clustering).
#' @param kmeans_max_iter Integer maximum iterations for the internal \code{k_means()} call.
#' @param kmeans_tol Numeric tolerance for \code{k_means()} if that implementation supports a \code{tol} argument.
#' @param seed Optional integer random seed for reproducibility. If provided, \code{set.seed(seed)} is called
#'   before k-means; additionally passed to \code{k_means()} if it supports a \code{seed} argument.
#' @param verbose Logical. If \code{TRUE}, prints informational messages (e.g., chosen \code{sigma}).
#' @param return_affinity Logical. If \code{TRUE}, also return the affinity matrix \eqn{W}.
#'
#' @return A list with components:
#' \describe{
#'   \item{clusters}{Integer vector of length \code{nrow(X)} with cluster assignments.}
#'   \item{embedding}{Numeric matrix \eqn{n \times k} of spectral embedding (eigenvectors).}
#'   \item{eigenvalues}{Numeric vector of length \code{k} with the selected eigenvalues of the Laplacian.}
#'   \item{affinity}{(Optional) The affinity matrix \eqn{W} if \code{return_affinity = TRUE}.}
#' }
#'
#' @examples
#' set.seed(1)
#' n <- 80
#' X <- rbind(
#'   matrix(rnorm(n * 2, mean = 0, sd = 0.4), ncol = 2),
#'   matrix(rnorm(n * 2, mean = 3, sd = 0.4), ncol = 2)
#' )
#'
#' res <- spectral_clustering(
#'   X, k = 2,
#'   affinity = "rbf",
#'   laplacian = "sym",
#'   seed = 123
#' )
#' table(res$clusters)
#'
#' @export
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
  ...
}


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
