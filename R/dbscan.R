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
##        - using in dist (method = "euclidean")? (похуй, оставляем?)
##        - return extra stats (n_clusters, noise_count, cluster_sizes)
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
#' \describe{
#'   \item{clusters}{Integer vector of length n. 0 = noise, 1..K = cluster id.}
#'   \item{core}{Logical vector of length n. TRUE for core points.}
#'   \item{eps}{The eps used.}
#'   \item{minPts}{The minPts used.}
#' }
#'
#' @details
#' For large \code{n}, the function may warn because it computes a full distance matrix (O(n^2)).
#' 
#' @examples
#' x <- matrix(c(
#'   0, 0,
#'   0.1, 0,
#'   0, 0.1,
#'   5, 5
#' ), ncol = 2, byrow = TRUE)
#' res <- dbscan(x, eps = 0.2, minPts = 3)
#' table(res$clusters)
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

  list(
    clusters = clusters,
    core = core,
    eps = eps,
    minPts = minPts
  )
}
