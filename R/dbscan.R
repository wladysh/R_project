## DBSCAN (Density-Based Spatial Clustering of Applications with Noise)
## Pipeline:
##	1) validate input (numeric matrix/data.frame, no NA, eps > 0, minPts >= 1)
##	2) compute distances (current version: full distance matrix)
##	3) iterate points and skip already visited
##	4) get eps-neighborhood of point i
##	5) if neighborhood < minPts: mark as noise for now (may change later)
##	6) else: start new cluster, expand it with a queue:
##        - when a neighbor is a core point, add its neighbors too
##        - assign border points to the current cluster
##	7) return labels + optional metadata (core flags, params)
##	8) OPTIONAL?
##        - S3 print, call, plot
##        - vignette for DBSCAN and OPTICS
##        - example for DBSCAN and OPTICS
##        - the possibility of refusing surgery in case of high input
##        - README.md


## Functions:

#' DBSCAN clustering
#'
#' Density-based clustering that finds dense groups of points and marks outliers
#' as noise. The number of clusters is not fixed in advance.
#'
#' @param x Numeric matrix or data frame (n*m). Rows are points.
#' @param eps Single numeric value, that > 0. Neighborhood radius.
#' @param minPts Single integer >= 1. Minimum number of points in the eps neighborhood
#' for a core point, including the point itself.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{clusters}: Integer vector of length n. 0 = noise, 1..K = cluster id.
#'   \item \code{core}: Logical vector of length n. TRUE for core points.
#'   \item \code{eps}: The eps used.
#'   \item \code{minPts}: The minPts used.
#'   \item \code{clusters_count}: Number of clusters found.
#'   \item \code{noise_count}: Number of noise points.
#'   \item \code{cluster_sizes}: Named integer vector: sizes of clusters.
#'   \item \code{core_count}: Number of core points.
#'   \item \code{border_count}: Number of border points.
#'   \item \code{n}: Number of input points.
#' }
#' 
#' @details
#' For large \code{n}, the function may warn because it computes a full distance matrix (O(n^2)).
#' 
#' Use \code{print(res)} to show a compact summary of the clustering result.
#' 
#' The returned object can be visualized with \code{plot(res, x)}, where
#' \code{res} is the object returned by \code{dbscan()} and \code{x} is the
#' original input data. The plot uses only the first two columns of \code{x}.
#' Filled points indicate core points, open circles indicate border points, and
#' crosses indicate noise points.
#' 
#' @examples
#' x <- matrix(c(
#'   0, 0,
#'   0.1, 0,
#'   0, 0.1,
#'   5, 5
#' ), ncol = 2, byrow = TRUE)
#' 
#' res <- dbscan(x, eps = 0.2, minPts = 3)
#' table(res$clusters)
#' plot(res, x)
#'
#' @export

dbscan <- function(x, eps, minPts){
  # checks
  if (!is.numeric(eps) || length(eps) != 1 || is.na(eps) || eps <= 0){
    stop("eps must be a single numeric value > 0")
  }
  if (!is.numeric(minPts) || length(minPts) != 1 || is.na(minPts) || minPts < 1){
    stop("minPts must be a single numeric value >= 1")
  }
  minPts <- as.integer(minPts) # if minPts was double

  n <- if (is.matrix(x) || is.data.frame(x)) nrow(x) else NA_integer_
  db_warn_large_n(n)
  dist_mat <- db_dist_matrix(x)
  n <- nrow(dist_mat)

  visited <- rep(FALSE, n)
  clusters <- rep(0L, n)	# 0 = noise, 1...n = which cluster
  core <- rep(FALSE, n)

  cluster_id <- 0L

  for (i in seq_len(n)){
    if (visited[i] == TRUE){
      next
    }
    visited[i] <- TRUE

    neigh <- db_neighbors_eps(dist_mat, i, eps)

    if (length(neigh) < minPts){
      # not a core point -> noise/board (for now)
      clusters[i] <- 0L
      next
    }

    # start new cluster
    cluster_id <- cluster_id + 1L
    clusters[i] <- cluster_id
    core[i] <- TRUE

    # cluster expansion
    queue <- neigh

    while (length(queue) > 0){
      j <- queue[1]
      queue <- queue[-1]

      if (!visited[j] == TRUE){
        visited[j] <- TRUE
        neigh_j <- db_neighbors_eps(dist_mat, j, eps)

        if (length(neigh_j) >= minPts){
          core[j] <- TRUE
          # add new neighbors and avoid infinite growth
          queue <- unique(c(queue, neigh_j))
        }
      }

      # assign to cluster if it wasn't or was noise
      if (clusters[j] == 0L){
        clusters[j] <- cluster_id
      }
    }
  }

  noise_count <- sum(clusters == 0L)

  tab <- table(clusters[clusters > 0L])
  cluster_sizes <- structure(as.integer(tab), names = attr(tab, "names"))

  core_count <- sum(core)
  border_count <- sum(clusters != 0L & !core == TRUE)

  res <- list(
    clusters = clusters,
    core = core,
    eps = eps,
    minPts = minPts,
    clusters_count = as.integer(cluster_id),
    noise_count = as.integer(noise_count),
    cluster_sizes = cluster_sizes,
    core_count = as.integer(core_count),
    border_count = as.integer(border_count),
    n = as.integer(n)
  )

  class(res) <- "haufenR_dbscan"
  res
}
