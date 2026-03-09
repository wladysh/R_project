#install.packages("testthat")

library(testthat)
library(haufenR)


set.seed(123)
n <- 50
X <- rbind(matrix(rnorm(n, mean = 0, sd = 0.3), ncol = 2),
           matrix(rnorm(n, mean = 3, sd = 0.3), ncol = 2),
           matrix(rnorm(n, mean = 6, sd = 0.3), ncol = 2)
           )

test_that("k-means returns correct structure", {
  K <- 3
  res <- k_means(X, K, max_iter = 50, nstart = 3)
  
  expect_type(res, "list")
  expect_named(res, c("centers", "clusters", "iter", "tot_withinss"))
  expect_equal(nrow(res$centers), K)
  expect_equal(ncol(res$centers), ncol(X))
  expect_equal(length(res$clusters), nrow(X))
  expect_true(is.numeric(res$tot_withinss))
  expect_true(res$iter <= 50)
})

test_that("clusters are integers in 1 to K", {
  K <- 3
  res <- k_means(X,K, max_iter = 50, nstart = 3)
  
  expect_true(all(res$clusters %in% 1:K))
  expect_type(res$clusters, "integer")
})

test_that("total within-cluster sum of squares decreases with nstart", {
  K <- 3
  res1 <-  k_means(X,K, max_iter = 50, nstart = 1) #one start
  res2 <-  k_means(X,K, max_iter = 50, nstart = 5) #multiple starts
  
  expect_lte(res2$tot_withinss, res1$tot_withinss)
})

test_that("function handles edge cases", {
  single_point <- matrix(c(1,2), nrow = 1)
  res <- k_means(single_point, 1)
  
  expect_equal(res$clusters, 1)
  expect_equal(res$centers, single_point)
  
  X2 <- rbind(matrix(0, nrow = 2, ncol = 2), matrix(5, nrow = 2, ncol = 2))
  K2 <- 3
  res <- k_means(X2, K2, nstart = 3)
  
  expect_equal(nrow(res$centers), K2)
  expect_equal(length(res$clusters), nrow(X2))
})

test_that("k-means changes centers", {
  set.seed(42)
  X <- matrix(rnorm(20*2), ncol = 2)
  K <- 2
  res <- k_means(X,K)
  
  expect_equal(anyDuplicated(res$centers), 0)
})

test_that("k-means recognises Structure", {
  X <- matrix(c(1,1,1,2,2,1,8,8,8,9,9,8), ncol = 2, byrow = TRUE)
  K <- 2
  res <- k_means(X,K)
  
  expect_equal(nrow(res$centers), K)
  expect_equal(length(unique(res$clusters)), K)
})

test_that("k_means throws error for invalid input", {
  Char <- matrix(c("a", "b", "c", "d"), ncol = 2)
  expect_error(k_means(Char, 2))
  expect_error(k_means("not data", 2))
  
  X <- matrix(rnorm(20), ncol = 2)
  expect_error(k_means(X, 0))
  expect_error(k_means(X, nrow(X) + 1))
})