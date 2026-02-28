library(testthat)

test_that("optics returns correct structure and types", {
  x <- matrix(rnorm(40), ncol = 2)

  res <- optics(x, eps = 0.5, minPts = 3)

  expect_type(res, "list")
  expect_named(res, c("order", "reachability", "core_dist", "eps", "minPts"))

  expect_type(res$order, "integer")
  expect_type(res$reachability, "double")
  expect_type(res$core_dist, "double")

  expect_equal(length(res$order), nrow(x))
  expect_equal(length(res$reachability), nrow(x))
  expect_equal(length(res$core_dist), nrow(x))

  # order should be a permutation of 1...n
  expect_equal(sort(res$order), seq_len(nrow(x)))

  expect_true(all(res$reachability >= 0 | is.infinite(res$reachability)))
  expect_true(all(res$core_dist >= 0 | is.infinite(res$core_dist)))

  expect_equal(res$eps, 0.5)
  expect_equal(res$minPts, as.integer(3))
})

test_that("optics gives expected result on a simple 1D dataset", {
  # points 1...3 = one dense region
  # point 4 = isolated (noise / new region start)
  x <- matrix(c(0.00, 0.10, 0.20, 1.00), ncol = 1)

  res <- optics(x, eps = 0.25, minPts = 3)

  # deterministic for this dataset with this implementation
  expect_equal(res$order, c(1L, 2L, 3L, 4L))

  # start points keep Inf reachability
  expect_true(is.infinite(res$reachability[1]))
  expect_true(is.infinite(res$reachability[4]))

  # inside the region reachability is finite and small
  expect_true(is.finite(res$reachability[2]))
  expect_true(is.finite(res$reachability[3]))
  expect_true(res$reachability[3] < res$reachability[2])

  # core distances: finite for points 1...3; Inf for isolated point
  expect_true(all(is.finite(res$core_dist[1:3])))
  expect_true(is.infinite(res$core_dist[4]))

  # numeric sanity checks
  expect_equal(res$core_dist[1], 0.20, tolerance = 1e-8)
  expect_equal(res$core_dist[2], 0.10, tolerance = 1e-8)
  expect_equal(res$core_dist[3], 0.20, tolerance = 1e-8)

  expect_equal(res$reachability[2], 0.20, tolerance = 1e-8)
  expect_equal(res$reachability[3], 0.10, tolerance = 1e-8)
})

test_that("optics can produce all Inf reachability when eps is too small", {
  x <- matrix(c(0.00, 0.10, 0.20), ncol = 1)

  res <- optics(x, eps = 1e-6, minPts = 2)

  # no point can reach any other point within eps
  expect_equal(res$order, c(1L, 2L, 3L))
  expect_true(all(is.infinite(res$reachability)))
  expect_true(all(is.infinite(res$core_dist)))
})

test_that("optics validates eps and minPts", {
  x <- matrix(rnorm(10), ncol = 2)

  expect_error(optics(x, eps = 0, minPts = 3))
  expect_error(optics(x, eps = -1, minPts = 3))
  expect_error(optics(x, eps = NA, minPts = 3))
  expect_error(optics(x, eps = c(0.2, 0.3), minPts = 3))

  expect_error(optics(x, eps = 0.2, minPts = 0))
  expect_error(optics(x, eps = 0.2, minPts = NA))
  expect_error(optics(x, eps = 0.2, minPts = c(3, 4)))
})

test_that("optics validates input x through db_dist_matrix", {
  # NA = failed
  x_na <- matrix(c(0, 0, NA, 1), ncol = 2, byrow = TRUE)
  expect_error(optics(x_na, eps = 0.5, minPts = 2))

  # non-numeric column = failed
  df_bad <- data.frame(a = c(0, 1, 2), b = c("x", "y", "z"))
  expect_error(optics(df_bad, eps = 0.5, minPts = 2))
})

test_that("optics works with data.frame input", {
  x <- data.frame(a = c(0, 0.10, 0.20, 1.00), b = c(0, 0.00, 0.00, 1.00))
  res <- optics(x, eps = 0.25, minPts = 2)

  expect_equal(length(res$order), nrow(x))
})

test_that("optics warns on large n (full distance matrix O(n^2))", {
  # n = 1001 triggers warning if your limit is 1000
  n <- 1001L
  x <- matrix(seq_len(n), ncol = 1)

  expect_warning(
    res <- optics(x, eps = 1e-12, minPts = 2),
    "Large dataset"
  )

  expect_equal(res$order, seq_len(n))
  expect_true(all(is.infinite(res$reachability)))
  expect_true(all(is.infinite(res$core_dist)))
})
