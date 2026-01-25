## Implementing our own k-means function with chapter 9.1 (Richter)


## Helpers ####################################################################

#Distance
#' Compute euclidean distance
#' @param x Numeric Vector
#' @param centers Numeric Vector
#' @return Numeric euclidean distances between x and centers as vector
euclidean_distance <- function(x, centers) {
  sqrt(rowSums((centers-x)^2)) 
}


#Assign
#' Assign the Clusters
#' @param X Matrix n x d
#' @param centers Matrix K x d
#' @return Integer Vector of cluster assignments
assign_clusters_mean <- function(X, centers) {
  n <- nrow(X) 
  clusters <- integer(n)
  
  for(i in seq_len(n)) {
    clusters[i] <- which.min(euclidean_distance(X[i,], centers))
  }
  
  clusters
}


#Update
#' Update the Centers
#' @param X Matrix n x d
#' @param clusters Integer Vector of Cluster assignments
#' @param K Number of Clusters
#' @return Matrix K x d with updated Cluster Centers
update_centers <- function(X, clusters, K) {
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


#Converged
#' Test if converged
#' @param old_centers previous centers
#' @param new_centers current centers
#' @param tol Tolerance for converged
#' @return Logical, true if converged
has_converged <- function(old_centers, new_centers, tol = 1e-6) {
  max(abs(new_centers - old_centers)) < tol
}


## k-means #####################################################################
#' k-means algorithm
#' @param X Numeric matrix or data frame of size n x d
#' @param K Number of Clusters
#' @param max_iter Maximum number of iterations
#' @return A list with cluster assignments and cluster centers
#' @export
k_means <- function(X, K, max_iter = 100) {
  
  #Test if correct inputs
  if(!is.matrix(X) && !is.data.frame(X)) {
    stop("X must be a matrix or data frame")
  }
  
  X <- as.matrix(X)
  
  if(!is.numeric(X)) {
    stop("X must only have numeric values")
  }
  
  if(!(is.numeric(K) && length(K) == 1 && K > 0 && K <= nrow(X))) {
    stop("K does not work")
  }
  K <- as.integer(K)
  
  #Calculating k-means
  centers <- X[sample(1:nrow(X), K),, drop = FALSE]
  
  for(t in seq_len(max_iter)) {
    clusters <- assign_clusters_mean(X, centers)
    new_centers <- update_centers(X, clusters, K)
    
    if(has_converged(centers, new_centers)) break
    
    centers <- new_centers
  }
  
  list(centers = centers,
       clusters = clusters)
}
