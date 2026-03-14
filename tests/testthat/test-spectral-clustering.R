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

test_that("spectral_clustering can return affinity matrix", {
  set.seed(1)
  X <- rbind(
    matrix(rnorm(100, mean = 0), ncol = 2),
    matrix(rnorm(100, mean = 3), ncol = 2)
  )
  
  res <- spectral_clustering(
    X,
    k = 2,
    affinity = "rbf",
    sigma = 1,
    seed = 42,
    return_affinity = TRUE
  )
  
  expect_true("affinity" %in% names(res))
  expect_equal(dim(res$affinity), c(nrow(X), nrow(X)))
  expect_true(is.matrix(res$affinity))
  expect_true(all(diag(res$affinity) == 0))
})

test_that("spectral_clustering is reproducible for fixed seed", {
  set.seed(1)
  X <- rbind(
    matrix(rnorm(100, mean = 0), ncol = 2),
    matrix(rnorm(100, mean = 3), ncol = 2)
  )
  
  res1 <- spectral_clustering(
    X,
    k = 2,
    affinity = "rbf",
    sigma = 1,
    seed = 123
  )
  
  res2 <- spectral_clustering(
    X,
    k = 2,
    affinity = "rbf",
    sigma = 1,
    seed = 123
  )
  
  expect_equal(res1$clusters, res2$clusters)
  expect_equal(res1$embedding, res2$embedding)
  expect_equal(res1$eigenvalues, res2$eigenvalues)
})

test_that("spectral_clustering works with epsilon affinity", {
  X <- rbind(
    matrix(rnorm(100, mean = 0, sd = 0.2), ncol = 2),
    matrix(rnorm(100, mean = 3, sd = 0.2), ncol = 2)
  )
  
  res <- spectral_clustering(
    X,
    k = 2,
    affinity = "epsilon",
    epsilon = 1.0,
    seed = 42
  )
  
  expect_type(res, "list")
  expect_named(res, c("clusters", "embedding", "eigenvalues"))
  expect_equal(length(res$clusters), nrow(X))
  expect_equal(nrow(res$embedding), nrow(X))
  expect_equal(ncol(res$embedding), 2)
  expect_equal(length(res$eigenvalues), 2)
  expect_true(all(res$clusters %in% 1:2))
})