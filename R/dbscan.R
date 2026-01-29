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
##        - using in dist (method = "euclidean")?
##				- alternative distance access (compute distances on demand)?
##				- speed up neighbor search (avoid O(n^2) for large n)?
##        - return extra stats (n_clusters, noise_count, cluster_sizes)?
##        - documentation for dbscan (roxygen2 / man/*.Rd); helpers?
##        - vignette for DBSCAN and OPTICS
##        - example for DBSCAN and OPTICS
##        - OPTICS
##        - README.md?

## Helpers:

db_dist_matrix <- function(x){
  # compute distance between all points and check validate matrix on input
  stopifnot(
    "x must be a matrix or data.frame" = is.matrix(x) || is.data.frame(x),
    "x must not contain NA values" = !anyNA(x)
  )

  df <- as.data.frame(x)
  is_num <- sapply(df, is.numeric)
  stopifnot("all columns (cords) of x must be numeric" = all(is_num))

  x <- as.matrix(df)
  n <- nrow(x)
  d_mat <- matrix(0, n, n)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      d_mat[i, j] <- sqrt(sum((x[i, ] - x[j, ])^2))
    }
  }

  d_mat
}

db_neighbors_eps <- function(d_mat, i, eps){
  # indices of all points within eps (including i itself)
  which(d_mat[i, ] <= eps)
}


## Functions:

dbscan <- function(x, eps, minPts){
  # checks

  if (!is.numeric(eps) || length(eps) != 1 || is.na(eps) || eps <= 0){
    stop("eps must be a single numeric value > 0")
  }
  if (!is.numeric(minPts) || length(minPts) != 1 || is.na(minPts) || minPts < 1){
    stop("minPts must be a single numeric value >= 1")
  }
  minPts <- as.integer(minPts) # if minPts was double

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

      if (!visited[j]){
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
