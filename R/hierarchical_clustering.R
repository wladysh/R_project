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
    stop("k must be an integer with 1 <= k <= number of observations")
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

#' Agglomerative hierarchical clustering (from scratch)
#'
#' Builds a hierarchical clustering tree by repeatedly merging the two closest
#' clusters under a chosen linkage criterion. Optionally, the tree can also be
#' cut into \code{k} clusters.
#'
#' @param x Numeric matrix or data frame with observations in rows and features in columns.
#' @param k Optional integer. If provided, the function also returns cluster
#'   assignments obtained by cutting the tree into \code{k} clusters.
#' @param linkage Character string specifying the linkage rule. One of
#'   \code{"complete"}, \code{"single"}, \code{"average"}, \code{"ward"}.
#' @param metric Character string specifying the distance metric. Currently only
#'   \code{"euclidean"} is supported.
#'
#' @details
#' The returned object has class \code{"hclust"}, so it can be used with
#' \code{plot()} and \code{stats::cutree()}. If \code{k} is supplied, the function
#' also adds a \code{clusters} element with the corresponding flat clustering.
#'
#' @return A list of class \code{"hclust"} with components:
#' \itemize{
#'   \item \code{merge}: integer matrix of size \code{(n - 1) x 2} describing the merges.
#'   \item \code{height}: numeric vector of merge heights.
#'   \item \code{order}: integer vector giving a leaf order for plotting.
#'   \item \code{labels}: character vector of observation labels.
#'   \item \code{method}: the selected linkage method.
#'   \item \code{dist.method}: the selected distance metric.
#'   \item \code{call}: the matched function call.
#'   \item \code{clusters}: integer vector of length \code{n} if \code{k} is provided.
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

  if (metric != "euclidean") {
    stop("unsupported metric: ", metric)
  }

  n <- nrow(x)

  if (!is.null(k) && !(is.numeric(k) && length(k) == 1 && k >= 1 && k <= n && (k %% 1 == 0))) {
    stop("k must be an integer with 1 <= k <= number of observations")
  }

  dist_mat <- hc_euclidean_distance_matrix(x)

  clusters <- lapply(seq_len(n), function(i) i)
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
