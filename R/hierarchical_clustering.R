## Hierarchical clustering (agglomerative, from scratch)
##
## Pipeline:
##  1) validate input (numeric matrix/data.frame, no NA)
##  2) compute distance matrix (currently Euclidean only)
##  3) start with n singleton clusters
##  4) repeatedly merge the two closest clusters (according to linkage)
##  5) optionally cut the resulting tree into k clusters


hc_validate_input <- function(x) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("x must be a matrix or data.frame")
  }
  x <- as.matrix(x)
  if (!is.numeric(x)) {
    stop("x must contain only numeric values")
  }
  if (anyNA(x)) {
    stop("x must not contain NA values")
  }
  if (nrow(x) < 2) {
    stop("x must have at least 2 rows (observations)")
  }
  x
}

hc_euclidean_distance_matrix <- function(x) {
  # compute full Euclidean distance matrix using the identity:
  # ||a-b||^2 = ||a||^2 + ||b||^2 - 2 a^T b
  s <- rowSums(x^2)
  d2 <- outer(s, s, "+") - 2 * tcrossprod(x)
  d2[d2 < 0] <- 0
  sqrt(d2)
}

hc_linkage_distance <- function(dist_mat, x, members_a, members_b, linkage) {
  if (linkage == "single") {
    return(min(dist_mat[members_a, members_b, drop = FALSE]))
  }
  if (linkage == "complete") {
    return(max(dist_mat[members_a, members_b, drop = FALSE]))
  }
  if (linkage == "average") {
    return(mean(dist_mat[members_a, members_b, drop = FALSE]))
  }
  if (linkage == "ward") {
    # Ward (Euclidean): increase in within-cluster SSE when merging A and B.
    # delta = (|A|*|B|)/( |A|+|B| ) * ||mu_A - mu_B||^2
    size_a <- length(members_a)
    size_b <- length(members_b)
    mu_a <- colMeans(x[members_a, , drop = FALSE])
    mu_b <- colMeans(x[members_b, , drop = FALSE])
    delta <- (size_a * size_b) / (size_a + size_b) * sum((mu_a - mu_b)^2)
    return(sqrt(delta))
  }
  stop("unsupported linkage: ", linkage)
}

hc_leaf_order <- function(merge) {
  # return leaf order for plotting (similar to hclust$order)
  n <- nrow(merge) + 1
  recurse <- function(node_id) {
    if (node_id < 0) {
      return(-node_id)
    }
    left <- merge[node_id, 1]
    right <- merge[node_id, 2]
    c(recurse(left), recurse(right))
  }
  recurse(n - 1)
}

hc_cut_tree <- function(merge, k) {
  n <- nrow(merge) + 1
  if (!(is.numeric(k) && length(k) == 1 && k >= 1 && k <= n && (k %% 1 == 0))) {
    stop("k must be an integer with 1 <= k <= nrow(x)")
  }
  if (k == n) {
    return(seq_len(n))
  }

  node_members <- vector("list", length = n - 1)
  for (step in seq_len(n - 1)) {
    left <- merge[step, 1]
    right <- merge[step, 2]
    members_left <- if (left < 0) -left else node_members[[left]]
    members_right <- if (right < 0) -right else node_members[[right]]
    node_members[[step]] <- c(members_left, members_right)
  }

  active_ids <- -seq_len(n)
  n_merges_to_apply <- n - k
  for (step in seq_len(n_merges_to_apply)) {
    left <- merge[step, 1]
    right <- merge[step, 2]
    active_ids <- active_ids[!(active_ids %in% c(left, right))]
    active_ids <- c(active_ids, step)
  }

  clusters <- integer(n)
  for (cluster_idx in seq_along(active_ids)) {
    node_id <- active_ids[cluster_idx]
    members <- if (node_id < 0) -node_id else node_members[[node_id]]
    clusters[members] <- cluster_idx
  }
  clusters
}

# --- main function ---

#' Agglomerative hierarchical clustering (from scratch)
#'
#' Builds a hierarchical (agglomerative) clustering tree by repeatedly merging the
#' two closest clusters under a chosen linkage criterion. Optionally, the tree can
#' be cut into \code{k} clusters.
#'
#' @param x Numeric matrix or data frame with observations in rows and features in columns.
#' @param k Optional integer. If provided, the function also returns a vector of cluster
#'   assignments obtained by cutting the tree into \code{k} clusters.
#' @param linkage Character string specifying the linkage rule. One of
#'   \code{"complete"}, \code{"single"}, \code{"average"}, \code{"ward"}.
#' @param metric Character string specifying the distance metric. Currently only
#'   \code{"euclidean"} is supported.
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{merge}: \code{(n-1) x 2} integer matrix describing the merges (like \code{stats::hclust}).
#'   \item \code{height}: numeric vector of length \code{n-1} with merge heights (distances).
#'   \item \code{order}: integer vector giving a leaf order for plotting.
#'   \item \code{clusters}: integer vector of length \code{n} (only if \code{k} is provided).
#'   \item \code{linkage}, \code{metric}: the chosen settings.
#' }
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(40), ncol = 2)
#' res <- hc_hierarchical_clustering(x, k = 3, linkage = "average")
#' table(res$clusters)
#'
#' @export
hc_hierarchical_clustering <- function(
  x,
  k = NULL,
  linkage = c("complete", "single", "average", "ward"),
  metric = c("euclidean")
) {
  x <- hc_validate_input(x)

  linkage <- match.arg(linkage)
  metric <- match.arg(metric)

  if (!(metric == "euclidean")) {
    stop("unsupported metric: ", metric)
  }

  n <- nrow(x)

  if (!is.null(k) && !(is.numeric(k) && length(k) == 1 && k >= 1 && k <= n && (k %% 1 == 0))) {
    stop("k must be an integer with 1 <= k <= nrow(x)")
  }

  dist_mat <- hc_euclidean_distance_matrix(x)

  # each cluster is a vector of member indices
  clusters <- lapply(seq_len(n), function(i) i)
  # ids for merge matrix: leaves are negative (-1..-n), new nodes are positive (1..n-1)
  cluster_ids <- -seq_len(n)

  merge <- matrix(0L, nrow = n - 1, ncol = 2)
  height <- numeric(n - 1)

  for (step in seq_len(n - 1)) {
    m <- length(clusters)
    best_dist <- Inf
    best_pair <- c(NA_integer_, NA_integer_)

    for (i in seq_len(m - 1)) {
      members_i <- clusters[[i]]
      for (j in (i + 1):m) {
        members_j <- clusters[[j]]
        d_ij <- hc_linkage_distance(dist_mat, x, members_i, members_j, linkage)
        if (d_ij < best_dist) {
          best_dist <- d_ij
          best_pair <- c(i, j)
        }
      }
    }

    i <- best_pair[1]
    j <- best_pair[2]

    merge[step, ] <- c(cluster_ids[i], cluster_ids[j])
    height[step] <- best_dist

    new_members <- c(clusters[[i]], clusters[[j]])

    # remove j then i (so indices stay valid)
    if (j > i) {
      clusters <- clusters[-j]
      cluster_ids <- cluster_ids[-j]
      clusters <- clusters[-i]
      cluster_ids <- cluster_ids[-i]
    } else {
      clusters <- clusters[-i]
      cluster_ids <- cluster_ids[-i]
      clusters <- clusters[-j]
      cluster_ids <- cluster_ids[-j]
    }

    clusters[[length(clusters) + 1]] <- new_members
    cluster_ids[length(cluster_ids) + 1] <- step
  }

  order <- hc_leaf_order(merge)

  labels <- rownames(x)
  if (is.null(labels)) {
    labels <- as.character(seq_len(n))
  }

  result <- list(
    merge = merge,
    height = height,
    order = order,
    labels = labels,
    method = linkage,
    dist.method = metric,
    call = match.call()
  )
  class(result) <- "hclust"

  if (!is.null(k)) {
    result$clusters <- hc_cut_tree(merge, k)
  }

  result
}
