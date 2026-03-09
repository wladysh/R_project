## k_means
## Pipeline:
## 1) Squared euclidean distance function to calculate the distance of the Data Points to the centers (computationally cheaper)
## 2) Assign the Data Points to the nearest Center
## 3) Update the Center to the mean of the Data Points inside
## 4) Test if the algorithm has converged
## 5) Calculate the total within-cluster sum of squares

## --- Helpers ---

# Distance
mn_sq_euclidean_distance <- function(X, centers) {
  rowSums((centers - X)^2) 
}


# Assign the Clusters
mn_assign_clusters <- function(X, centers) {
  n <- nrow(X)
  clusters <- integer(n)
  
  for(i in seq_len(n)) {
    clusters[i] <- which.min(mn_sq_euclidean_distance(X[i,], centers))
  }
  
  clusters
}


# Update the Centers
mn_update_centers <- function(X, clusters, K) {
  d <- ncol(X)
  centers <- matrix(0, nrow = K, ncol = d)
  
  for(k in seq_len(K)) {
    points_inside <- X[clusters == k, , drop = FALSE]
    
    if(nrow(points_inside) > 0) { # cluster contains points
      centers[k,] <- colMeans(points_inside) #cluster-center is mean
    } else { # empty Clusters
      centers[k,] <- X[sample(1:nrow(X),1),] #cluster-center is random
    }
    
  }
  
  centers
}


# Test if converged
mn_has_converged <- function(old_centers, new_centers, tol = 1e-6) {
  sum((new_centers - old_centers)^2) < tol
}

# Total within-cluster sum of squares
mn_tot_withinss <- function(X, clusters, centers){
  sum(sapply(1:nrow(centers), function(k) {
    sum(rowSums((X[clusters == k, , drop = FALSE] - centers[k, ])^2))
  }))
}


## --- main function ---

#' k-means algorithm
#' 
#' This function implements the k-means clustering algorithm. (Chapter 9.1, Stefan Richter - Statistisches und maschinelles Lernen)
#' It supports multiple random starts to find a better local minimum.
#' 
#' @param X Numeric matrix or data frame of size n x d
#' @param K Number of Clusters
#' @param max_iter Maximum number of iterations (default 100)
#' @param tol Convergence tolerance (default 1e-6)
#' @param nstart Number of random starts (default 10)
#' @return A list with following elements:
#' \item{centers}{Matrix of cluster centers (K x d)}
#' \item{clusters}{Vector of cluster assignments for each observation}
#' \item{iter}{Number of iterations until convergence}
#' \item{tot_withinss}{Total within-cluster sum of squares}
#' @export
k_means <- function(X, K, max_iter = 100, tol = 1e-6, nstart = 10) {
  
  # Input checks
  if(!is.matrix(X) && !is.data.frame(X)) {
    stop("X must be a matrix or data frame")
  }
  X <- as.matrix(X)
  
  if(!is.numeric(X)) {
    stop("X must only have numeric values")
  }
  
  if(!(is.numeric(K) && length(K) == 1 && K > 0 && K <= nrow(X))) {
    stop("K invalid")
  }
  K <- as.integer(K)
  
  best_withinss <- Inf
  best_result <- NULL
  
  for(start in seq_len(nstart)) {
    centers <- X[sample(1:nrow(X), K),, drop = FALSE]
    
    for(t in seq_len(max_iter)) {
      clusters <- mn_assign_clusters(X, centers)
      new_centers <- mn_update_centers(X, clusters, K)
      
      if(mn_has_converged(centers, new_centers, tol)) break
      
      centers <- new_centers
    }
    
    tot_withinss <- mn_tot_withinss(X, clusters, centers)
    if(tot_withinss < best_withinss) {
      best_withinss <- tot_withinss
      best_result <- list(centers = centers,
                          clusters = clusters,
                          iter = t,
                          tot_withinss = tot_withinss)
    }
    
  }
  
  best_result
}
