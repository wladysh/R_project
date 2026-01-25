test_that("spectral_clustering returns correct structure", {
  set.seed(1)
  X <- rbind(
    matrix(rnorm(200, mean = 0), ncol = 2),
    matrix(rnorm(200, mean = 4), ncol = 2)
  )
  
  res <- spectral_clustering(X, k = 2, affinity = "rbf", sigma = 1, seed = 42)
  
  expect_type(res, "list")
  expect_named(res, c("clusters", "embedding", "eigenvalues"))
  expect_equal(length(res$clusters), nrow(X))
  expect_equal(nrow(res$embedding), nrow(X))
  expect_equal(ncol(res$embedding), 2)
  expect_equal(length(res$eigenvalues), 2)
})

test_that("spectral_clustering separates two blobs (label-invariant)", {
  set.seed(1)
  X1 <- matrix(rnorm(200, mean = 0), ncol = 2)
  X2 <- matrix(rnorm(200, mean = 4), ncol = 2)
  X <- rbind(X1, X2)
  truth <- c(rep(1, nrow(X1)), rep(2, nrow(X2)))
  
  res <- spectral_clustering(X, k = 2, affinity = "rbf", sigma = 1, seed = 42)
  pred <- res$clusters
  
  acc1 <- mean(pred == truth)
  acc2 <- mean(pred == (3 - truth))  # swap labels for k=2
  expect_true(max(acc1, acc2) > 0.9)
})
