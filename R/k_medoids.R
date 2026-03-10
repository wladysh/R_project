# k_medoids
# Pipeline:
# 1) Compute the pairwise distance matrix of all data points
# 2) Randomly initialize k medoids
# 3) Assign each data point to the nearest medoid
# 4) Evaluate swaps between medoids and non-medoids
# 5) Accept the swap if it reduces the total clustering cost
# 6) Repeat until no improvement or max_iter is reached

md_dist_matrix <- function(x) {
  
  # x wird hier überpüft (muss eine Matrix oder Data frame sein, keine NAs, nur Zahlen)
  stopifnot("x must be a matrix or data.frame" = 
              is.matrix(x) || is.data.frame(x),
            "all columns of x must be numeric" = all(sapply(x, is.numeric)),
            "x must not contain NA values" = !anyNA(x)
  )
  
  # dann wird die Distanzmatrix  mit der euklidischen Distanz berechnet    
  as.matrix(dist(x, method="euclidean"))
}

# Indizes von k Medoids auswählen (zufällig)
md_init_medoids <- function(k, n) {
  stopifnot("k must be >= 1 " = k >= 1,
            "k must be <= number of data points" = k <= n)
  sample(seq_len(n), size = k, replace = FALSE)
} 

md_assign_clusters <- function(dist_mat, medoids) {
  n <- nrow(dist_mat) # Anzahl der Datenpunkte
  k <- length(medoids) # Anzahl der Cluster
  clusters <- integer(n) # Vektor, der jedem Datenpunkt sagt, zu welchem Cluster er gehört
  
  # mit which.min wird herausgefunden, welcher medoid die kleinste Distanz zu Datenpunkt i hat
  for(i in seq_len(n)) {
    clusters[i] <- which.min(dist_mat[i, medoids]) 
  }
  
  clusters
  
}

md_total_cost <- function(dist_mat, medoids) {
  # für jeden Punkt die Distanz zum nächstgelegenen Medoid summieren
  sum(sapply(1:nrow(dist_mat), function(i) {
    min(dist_mat[i, medoids])
  }))
}

md_swap <- function(medoids, dist_mat) {
  n <- nrow(dist_mat)
  current_cost <- md_total_cost(dist_mat, medoids) # Kosten für aktuelle Medoids
  best_medoids <- medoids
  best_cost <- current_cost
  
  for(m_idx in seq_along(medoids)){
    for(o in setdiff(1:n, medoids)) { # alle Nicht-Medoids
      trial_medoids <- medoids
      trial_medoids[m_idx] <- o # Swap durchführen
      new_cost <- md_total_cost(dist_mat, trial_medoids)
      
      if(new_cost < best_cost){
        best_medoids <- trial_medoids
        best_cost <- new_cost
      }
    }
  }
  best_medoids
}

#' K-Medoids clustering algorithm
#'
#' This function partitions a dataset into k clusters by selecting k
#' representative data points (medoids). Each observation is assigned
#' to the cluster of the nearest medoid based on the Euclidean distance.
#'
#' @param x Numeric matrix or data frame containing the dataset.
#' @param k Number of clusters (medoids).
#' @param max_iter Maximum number of iterations (default = 100).
#'
#' @return
#' A list with the following components:
#'
#' \item{medoids}{Indices of the final medoids}
#' \item{clusters}{Cluster assignment for each observation}
#' \item{iterations}{Number of iterations performed}
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100), ncol = 2)
#' res <- k_medoids(x, k = 3)
#' print(res$clusters)
#' 
#' @export
k_medoids <- function(x, k, max_iter=100) {
  n <- nrow(x) # Anzahl der Punkte
  
  dist_mat <- md_dist_matrix(x) # Distanzmatrix berechnen
  
  medoids <- md_init_medoids(k, n) # Ausgangsmedoids zufällig wählen
  
  for(iter in 1:max_iter){
   new_medoids <- md_swap(medoids, dist_mat)
   
   if(all(new_medoids == medoids)) break # kein Swap verbessert die Kosten
   medoids <- new_medoids
   
  }
  
  clusters <- md_assign_clusters(dist_mat, medoids)
  list(medoids = medoids, clusters = clusters, iterations = iter)
}
