library(testthat)
library(haufenR)

test_that("hc_hierarchical_clustering returns correct structure", {
  set.seed(123)
  x <- matrix(rnorm(30 * 2), ncol = 2)

  res <- hc_hierarchical_clustering(x, k = 3, linkage = "complete")

  expect_s3_class(res, "hclust")
  expect_type(unclass(res), "list")
  expect_true(all(c("merge", "height", "order", "labels", "method", "dist.method", "clusters") %in% names(res)))

  n <- nrow(x)
  expect_equal(dim(res$merge), c(n - 1, 2))
  expect_equal(length(res$height), n - 1)
  expect_equal(length(res$order), n)
  expect_equal(length(res$labels), n)
  expect_equal(length(res$clusters), n)
  expect_equal(length(unique(res$clusters)), 3)
})

test_that("hc_hierarchical_clustering produces non-decreasing heights", {
  x <- matrix(c(0, 0,
                0, 1,
                10, 10,
                10, 11), ncol = 2, byrow = TRUE)

  res <- hc_hierarchical_clustering(x, linkage = "average")
  expect_true(all(diff(res$height) >= -1e-12))
})

test_that("hc_hierarchical_clustering validates k", {
  x <- matrix(rnorm(20), ncol = 2)

  expect_error(hc_hierarchical_clustering(x, k = 0))
  expect_error(hc_hierarchical_clustering(x, k = nrow(x) + 1))
  expect_error(hc_hierarchical_clustering(x, k = 2.5))
})
