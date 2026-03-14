library(testthat)
library(haufenR)

test_that("hc_hierarchical_clustering returns correct structure", {
  set.seed(123)
  x <- matrix(rnorm(30 * 2), ncol = 2)

  res <- hc_hierarchical_clustering(x, k = 3, linkage = "complete")

  expect_s3_class(res, "hclust")
  expect_type(unclass(res), "list")
  expect_true(all(c("merge", "height", "order", "labels", "method", "dist.method", "call", "clusters") %in% names(res)))

  n <- nrow(x)
  expect_equal(dim(res$merge), c(n - 1, 2))
  expect_equal(length(res$height), n - 1)
  expect_equal(length(res$order), n)
  expect_equal(length(res$labels), n)
  expect_equal(length(res$clusters), n)
  expect_equal(length(unique(res$clusters)), 3)
  expect_equal(sort(res$order), seq_len(n))
  expect_identical(res$method, "complete")
  expect_identical(res$dist.method, "euclidean")
})

test_that("hc_hierarchical_clustering supports all linkage options", {
  x <- matrix(c(
    0, 0,
    0, 1,
    5, 5,
    5, 6
  ), ncol = 2, byrow = TRUE)

  for (linkage in c("single", "complete", "average", "ward")) {
    res <- hc_hierarchical_clustering(x, k = 2, linkage = linkage)
    expect_s3_class(res, "hclust")
    expect_equal(length(unique(res$clusters)), 2)
    expect_true(all(diff(res$height) >= -1e-12))
  }
})

test_that("hc_hierarchical_clustering works with data.frame input and preserves row names", {
  x <- data.frame(
    a = c(0, 0, 5, 5),
    b = c(0, 1, 5, 6),
    row.names = c("p1", "p2", "p3", "p4")
  )

  res <- hc_hierarchical_clustering(x, k = 2, linkage = "average")

  expect_equal(res$labels, rownames(x))
  expect_equal(length(unique(res$clusters)), 2)
})

test_that("hc_hierarchical_clustering can return singleton clusters", {
  x <- matrix(rnorm(20), ncol = 2)

  res <- hc_hierarchical_clustering(x, k = nrow(x))

  expect_equal(res$clusters, seq_len(nrow(x)))
})

test_that("hc_hierarchical_clustering validates k", {
  x <- matrix(rnorm(20), ncol = 2)

  expect_error(hc_hierarchical_clustering(x, k = 0))
  expect_error(hc_hierarchical_clustering(x, k = nrow(x) + 1))
  expect_error(hc_hierarchical_clustering(x, k = 2.5))
})

test_that("hc_hierarchical_clustering validates x", {
  expect_error(hc_hierarchical_clustering("not a matrix"))
  expect_error(hc_hierarchical_clustering(matrix(c(1, NA, 2, 3), ncol = 2)))
  expect_error(hc_hierarchical_clustering(data.frame(a = c(1, 2), b = c("x", "y"))))
  expect_error(hc_hierarchical_clustering(matrix(c(1, 2), nrow = 1)))
})
