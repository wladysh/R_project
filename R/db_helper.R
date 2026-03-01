## Helpers for density-based clustering (DBSCAN / OPTICS)

db_dist_matrix <- function(x){
  # compute distance between all points and validate input

  if (!(is.matrix(x) == TRUE || is.data.frame(x) == TRUE)){
    stop("x must be a matrix or data.frame.")
  }

  if (anyNA(x) == TRUE){
    stop("x must not contain NA values.")
  }

  df <- as.data.frame(x)

  is_num <- sapply(df, is.numeric)
  if (!all(is_num) == TRUE){
    stop("All columns of x must be numeric.")
  }

  x <- as.matrix(df)
  n <- nrow(x)
  d_mat <- matrix(0, n, n)

  for (i in seq_len(n)){
    for (j in i:n){
      dij <- sqrt(sum((x[i, ] - x[j, ])^2))
      d_mat[i, j] <- dij
      d_mat[j, i] <- dij
    }
  }

  d_mat
}

db_neighbors_eps <- function(d_mat, i, eps){
  # indices of all points within eps (including i)
  which(d_mat[i, ] <= eps)
}

db_core_distance <- function(d_mat, i, eps, minPts){
  # distance to the minPts-th nearest neighbor, within eps
  neigh <- db_neighbors_eps(d_mat, i, eps)

  if (length(neigh) < minPts){
    return(Inf)
  }

  sort(d_mat[i, neigh])[minPts]
}

db_warn_large_n <- function(
  n,
  limit = 1000L
){
  # warn for large n (full distance matrix is O(n^2))
  if (is.na(n) == TRUE || n <= limit){
    return(invisible(TRUE))
  }

  warning(
    sprintf(
      "Large dataset (n=%d): computing a full distance matrix is O(n^2) and may be slow / memory heavy.",
      n
    )
  )

  invisible(TRUE)
}

print.haufenR_dbscan <- function(x, ...){
  # console output for DBSCAN results
  cat("DBSCAN result\n")

  # basic settings + counts
  cat("n:       ", x$n, "\n", sep = "")
  cat("eps:     ", x$eps, "\n", sep = "")
  cat("minPts:  ", x$minPts, "\n", sep = "")
  cat("clusters:", x$clusters_count, "\n", sep = "")
  cat("noise:   ", x$noise_count, "\n", sep = "")
  cat("core:    ", x$core_count, "\n", sep = "")
  cat("border:  ", x$border_count, "\n", sep = "")

  # cluster sizes or empty
  if (length(x$cluster_sizes) > 0){
    nm <- attr(x$cluster_sizes, "names")
    pairs <- paste(nm, x$cluster_sizes, sep = "=")

    cat("sizes:   ", paste(pairs, collapse = ", "), "\n", sep = "")
  } else{
    cat("sizes:   -\n")
  }

  invisible(x)
}

print.haufenR_optics <- function(x, ...){
  # console output for OPTICS results
  cat("OPTICS result\n")

  cat("n:     ", x$n, "\n", sep = "")
  cat("eps:   ", x$eps, "\n", sep = "")
  cat("minPts:", x$minPts, "\n", sep = "")
  cat("core:  ", x$core_count, "\n", sep = "")
  cat("starts:", x$start_points_count, "\n", sep = "")

  invisible(x)
}