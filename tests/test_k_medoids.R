# install.packages("testthat")
library(testthat)

# Implementierungsdatei
source("../k_medoids.R")


# Beispiel-Test für die Distanzmatrix-Funktion
test_that("dist_matrix funktioniert korrekt", {
  x <- matrix(c(1, 2, 3, 4), nrow = 2)
  dist_mat <- dist_matrix(x)
  
  expect_equal(dim(dist_mat), c(2, 2))  # Die Distanzmatrix muss 2x2 sein
  expect_true(all(dist_mat >= 0))  # Alle Distanzen sollten >= 0 sein
})

# Test für die Funktion init_medoids
test_that("init_medoids wählt k Medoids", {
  set.seed(42)
  n <- 10
  k <- 3
  medoids <- init_medoids(k, n)
  
  expect_equal(length(medoids), k)  # Anzahl der Medoids muss k sein
  expect_true(all(medoids <= n))  # Medoid-Indizes dürfen nicht größer als n sein
  expect_true(all(medoids >= 1))  # Medoid-Indizes müssen >= 1 sein
})

# Test für K-Medoids-Algorithmus
test_that("k_medoids liefert Cluster", {
  x <- matrix(rbind(
    c(1, 2), c(2, 1), c(1, 1),     # Cluster 1
    c(8, 8), c(9, 8), c(8, 9),     # Cluster 2
    c(5, 5), c(5, 6), c(6, 5)      # Cluster 3
  ), ncol = 2, byrow = TRUE)
  
  k <- 3
  res <- k_medoids(x, k)
  
  expect_equal(length(res$clusters), nrow(x))  # Anzahl der Cluster muss der Anzahl der Punkte entsprechen
  expect_equal(length(res$medoids), k)        # Anzahl der Medoids muss k sein
})

test_that("K-Medoids funktioniert auch bei größeren Datensätzen", {
  set.seed(123)
  X_large <- matrix(rnorm(1000 * 4), ncol = 4)  # 1000 Datenpunkte
  k <- 5
  
  res_large <- k_medoids(X_large, k)
  expect_equal(length(res_large$clusters), 1000)  # Überprüfe, ob alle Datenpunkte zugeordnet wurden
})

