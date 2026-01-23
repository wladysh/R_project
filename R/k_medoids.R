dist_matrix <- function(x) {
  
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
init_medoids <- function(k, n) {
  stopifnot("k must be >= 1 " = k >= 1,
            "k must be <= number of data points" = k <= n)
  sample(seq_len(n), size = k, replace = FALSE)
} 

assign_clusters <- function(dist_mat, medoids) {
  n <- nrow(dist_mat) # Anzahl der Datenpunkte
  k <- length(medoids) # Anzahl der Cluster
  clusters <- integer(n) # Vektor, der jedem Datenpunkt sagt, zu welchem Cluster er gehört
  
  # mit which.min wird herausgefunden, welcher medoid die kleinste Distanz zu Datenpunkt i hat
  for(i in seq_len(n)) {
    clusters[i] <- which.min(dist_mat[i, medoids]) 
  }
  
  clusters
  
}

total_cost <- function(dist_mat, medoids) {
  # für jeden Punkt die Distanz zum nächstgelegenen Medoid summieren
  sum(sapply(1:nrow(dist_mat), function(i) {
    min(dist_mat[i, medoids])
  }))
}

swap <- function(medoids, dist_mat) {
  n <- nrow(dist_mat)
  current_cost <- total_cost(dist_mat, medoids) # Kosten für aktuelle Medoids
  best_medoids <- medoids
  best_cost <- current_cost
  
  for(m_idx in seq_along(medoids)){
    for(o in setdiff(1:n, medoids)) { # alle Nicht-Medoids
      trial_medoids <- medoids
      trial_medoids[m_idx] <- o # Swap durchführen
      new_cost <- total_cost(dist_mat, trial_medoids)
      
      if(new_cost < best_cost){
        best_medoids <- trial_medoids
        best_cost <- new_cost
      }
    }
  }
  best_medoids
}


k_medoids <- function(x, k, max_iter=100) {
  n <- nrow(x) # Anzahl der Punkte
  
  dist_mat <- dist_matrix(x) # Distanzmatrix berechnen
  
  medoids <- init_medoids(k, n) # Ausgangsmedoids zufällig wählen
  
  for(iter in 1:max_iter){
   new_medoids <- swap(medoids, dist_mat)
   
   if(all(new_medoids == medoids)) break # kein Swap verbessert die Kosten
   medoids <- new_medoids
   
  }
  
  clusters <- assign_clusters(dist_mat, medoids)
  list(medoids = medoids, clusters = clusters, iterations = iter)
}

# TODO: Roxygen-Dokumentation ergänzen
# TODO: Tests hinzufügen
