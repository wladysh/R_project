## OPTICS (Ordering Points To Identify the Clustering Structure)
## Pipeline:
##  1) validate eps and minPts
##  2) compute full distance matrix (O(n^2))
##  3) init: processed, order, reachability, core_dist
##  4) for each unprocessed point p:
##       - mark processed, append to order
##       - compute core_dist(p)
##       - if p is core, then update a seed set (priority by reachability)
##  5) return ordering + reachability + core distances


## Functions:

#' OPTICS ordering
#'
#' Computes an ordering of points and reachability distances that describe the
#' clustering structure and density. This does not directly return cluster labels.
#'
#' @param x Numeric matrix or data frame (n*m). Rows are points.
#' @param eps Single numeric value, that > 0. Maximum radius for neighborhood queries.
#' @param minPts Single integer >= 1. Minimum number of points in the eps-neighborhood
#' for a core point, including the point itself.
#'
#' @return A list with:
#' \describe{
#'   \item{order}{Integer vector: 1...n (visit order).}
#'   \item{reachability}{Numeric vector length n. Inf for start points and new regions.}
#'   \item{core_dist}{Numeric vector length n. Distance to the minPts neighbor within eps, else Inf.}
#'   \item{eps}{The eps used.}
#'   \item{minPts}{The minPts used.}
#' }
#'
#' @details
#' For plotting or inspection, usually look at \code{reachability[order]}.
#' For large \code{n}, the function may warn because it computes a full distance matrix (O(n^2)).
#'
#' @examples
#' x <- matrix(c(
#'   0, 0,
#'   0.1, 0,
#'   0, 0.1,
#'   5, 5
#' ), ncol = 2, byrow = TRUE)
#' res <- optics(x, eps = 0.2, minPts = 3)
#' res$reachability[res$order]
#'
#' @export

optics <- function(x, eps, minPts){
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

  processed <- rep(FALSE, n)
  order <- integer(0)
  reachability <- rep(Inf, n)  # Inf for start points/noise
  core_dist <- rep(Inf, n) # min dist around point, when minPts points inside

  seeds <- integer(0) # candidate points
  in_seeds <- rep(FALSE, n) # already in seeds

  update_seeds <- function(p, neigh, core_dist_p){
    # update reachability for neighbors of p and add them to seeds
    for (q in neigh) {
      if (processed[q] == TRUE){
        next
      }

      new_reach <- max(core_dist_p, dist_mat[p, q])

      if (is.infinite(reachability[q]) == TRUE || new_reach < reachability[q]){
        reachability[q] <<- new_reach
      }

      if (!in_seeds[q] == TRUE){
        seeds <<- c(seeds, q)
        in_seeds[q] <<- TRUE
      }
    }
  }

  for (p in seq_len(n)){
    # process new start point p (neighbors + core_dist)
    if (processed[p] == TRUE){
      next
    }

    processed[p] <- TRUE
    order <- c(order, p)

    neigh <- db_neighbors_eps(dist_mat, p, eps)
    core_dist_p <- db_core_distance(dist_mat, p, eps, minPts)
    core_dist[p] <- core_dist_p

    if (is.finite(core_dist_p) == TRUE){
      # only core points can expand a region
      update_seeds(p, neigh, core_dist_p)

      while (length(seeds) > 0){
        # pick point with smallest reachability
        q <- seeds[which.min(reachability[seeds])]
        seeds <- seeds[seeds != q]
        in_seeds[q] <- FALSE

        if (processed[q] == TRUE){
          next
        }

        processed[q] <- TRUE
        order <- c(order, q)

        neigh_q <- db_neighbors_eps(dist_mat, q, eps)
        core_dist_q <- db_core_distance(dist_mat, q, eps, minPts)
        core_dist[q] <- core_dist_q

        if (is.finite(core_dist_q) == TRUE){
          update_seeds(q, neigh_q, core_dist_q)
        }
      }
    }
  }

  list(
    order = order,
    reachability = reachability,
    core_dist = core_dist,
    eps = eps,
    minPts = minPts
  )
}
