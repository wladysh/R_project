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

#' @noRd
#' @export
#' @method print haufenR_dbscan
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

#' @noRd
#' @export
#' @method print haufenR_optics
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

#' @noRd
#' @export
#' @method plot haufenR_dbscan
plot.haufenR_dbscan <- function(x, y, ...){
  # plot DBSCAN clustering on the first two columns of the original data
  # use: plot(res, data)

  if (missing(y) == TRUE || is.null(y) == TRUE){
    stop("Use plot(res, x): please provide the original data as the second argument.")
  }
  if (!(is.matrix(y) == TRUE || is.data.frame(y) == TRUE)){
    stop("y must be a matrix or data.frame.")
  }

  df <- as.data.frame(y)

  if (anyNA(df) == TRUE){
    stop("y must not contain NA values.")
  }
  is_num <- sapply(df, is.numeric)
  if (!all(is_num) == TRUE){
    stop("All columns of y must be numeric.")
  }

  y <- as.matrix(df)
  
  if (nrow(y) != x$n){
    stop("y must have the same number of rows as in the DBSCAN result.")
  }
  if (ncol(y) < 2){
    stop("y must have at least 2 columns.")
  }

  # color by cluster id
  col <- as.integer(x$clusters) + 1L
  col[x$clusters == 0L] <- 8L

  # noise = x, core = filled dot, border = open circle
  pch <- rep(1L, x$n)
  pch[x$clusters == 0L] <- 4L
  pch[x$core] <- 19L

  plot(
    y[, 1],
    y[, 2],
    col = col,
    pch = pch,
    xlab = "x[,1]",
    ylab = "x[,2]",
    main = "DBSCAN",
    ...
  )

  invisible(x)
}

#' @noRd
#' @export
#' @method plot haufenR_optics
plot.haufenR_optics <- function(x, y, ...){
  # reachability plot for OPTICS results
  # use: plot(res)

  r <- x$reachability[x$order]

  if (length(r) == 0){
    stop("Empty OPTICS result")
  }
  if (!any(is.finite(r)) == TRUE){
    stop("All reachability distances are Inf")
  }

  # replace Inf by max finite value so the plot is readable
  max_local <- max(r[is.finite(r)])

  if (max_local <= 0){
    max_local <- 1
  }

  r_plot <- r
  r_plot[is.infinite(r_plot)] <- max_local * 1.3 # for small data

  plot(
    r_plot,
    type = "h",
    xlab = "order index",
    ylab = "reachability",
    main = "OPTICS",
    ...
  )

  invisible(x)
}
