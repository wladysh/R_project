## Implementing our own k-means function with chapter 9.1 (Richter)

#To dos:
  # Optimize Code
  # Change to Package-Structure
  # Add tests and vignette


## Helpers

#Distance
euclidian_distance <- function(x, centers) {
  sqrt(rowSums((centers-x)^2))
}


#Assign
assign_clusters <- function(X, centers) {
  n <- nrow(X)
  clusters <- integer(n)
  
  for(i in seq_len(n)) {
    clusters[i] <- which.min(euclidian_distance(X[i,], centers))
  }
  
  clusters
}


#Update
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


#Convergence
has_converged <- function(old_centers, new_centers, tol = 1e-6) {
  max(abs(new_centers - old_centers)) < tol
}


k_means <- function(X, K, max_iter = 100) {
  #set.seed(123)
  centers <- X[sample(1:nrow(X), K),]
  
  for(t in seq_len(max_iter)) {
    clusters <- assign_clusters(X, centers)
    new_centers <- update_centers(X, clusters, K)
    
    if(has_converged(centers, new_centers)) {
      cat("Convergence after", t, "iteration(s)\n")
      break
    }
    
    centers <- new_centers
  }
  
  list(centers = centers,
       clusters = clusters)
}


## Test
#X <- rbind( matrix(rnorm(10, mean = 0, sd = 0.3), ncol = 2),
#            matrix(rnorm(10, mean = 0, sd = 0.3), ncol = 2),
#            matrix(rnorm(10, mean = 0, sd = 0.3), ncol = 2))

#result <- k_means(X, K = 3, max_iter = 50)

#print(result$centers)
#print(result$clusters)

#plot(X, col = result$clusters, pch = 19, main = "Test k-means")
#points(result$centers, col = 1:3, pch = 8, cex = 2)

