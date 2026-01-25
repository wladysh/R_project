#install.packages("testthat")

library(testthat)
library(haufenR)


#Test Structure
test_that("k-means returns correct structure", {
  set.seed(123)
  X <- matrix(rnorm(30*2), ncol = 2)
  K <- 3
  res <- k_means(X,K)
  
  expect_type(res, "list")
  expect_setequal(res, c("centers", "clusters"))
  expect_equal(nrow(res$centers), K)
  expect_equal(length(res$clusters), nrow(X))
})

#Test Konvergence
test_that("k-means changes centers", {
  set.seed(42)
  X <- matrix(rnorm(20*2), ncol = 2)
  K <- 2
  res <- k_means(X,K)
  
  expect_false(anyDuplicated(res$centers))
})

#Test with known Clusters
test_that("k-means recognises Structure", {
  X <- matrix(c(1,1,1,2,2,1,8,8,8,9,9,8), ncol = 2, byrow = TRUE)
  K <- 2
  res <- k_means(X,K)
  
  expect_equal(nrow(res$centers), K)
  expect_equal(length(unique(res$clusters)), K)
})