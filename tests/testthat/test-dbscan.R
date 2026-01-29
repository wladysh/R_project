library(testthat)

test_that("dbscan returns correct structure and types", {
  x <- matrix(rnorm(40), ncol = 2)

  res <- dbscan(x, eps = 0.5, minPts = 3)

  expect_type(res, "list")
  expect_named(res, c("clusters", "core", "eps", "minPts"))

  expect_type(res$clusters, "integer")
  expect_type(res$core, "logical")

  expect_equal(length(res$clusters), nrow(x))
  expect_equal(length(res$core), nrow(x))

  expect_true(all(!is.na(res$clusters)))
  expect_true(all(res$clusters >= 0L))

  expect_equal(res$eps, 0.5)
  expect_equal(res$minPts, as.integer(3))
})

test_that("dbscan finds one cluster + border + noise on a simple dataset", {
  # 1...5 = core cluster
  # 6 = border point
  # 7 = noise
  x <- rbind(
    c(0.00, 0.00),
    c(0.01, 0.00),
    c(0.00, 0.01),
    c(0.01, 0.01),
    c(0.02, 0.00),
    c(0.045, 0.00),
    c(1.00, 1.00)
  )

  res <- dbscan(x, eps = 0.03, minPts = 5)
  clt <- res$clusters
  core <- res$core

  # noise point
  expect_equal(clt[7], 0L)

  # points 1...6 should be in the same cluster
  expect_true(all(clt[1:6] > 0L))
  cluster_ids <- unique(clt[1:6])
  expect_equal(length(cluster_ids), 1)

  # border point
  expect_false(core[6])

  # core points exist check
  expect_true(all(core[1:5]))
})

test_that("dbscan can produce all-noise result when eps is too small", {
  x <- rbind(
    c(0, 0),
    c(1, 0),
    c(0, 1)
  )

  res <- dbscan(x, eps = 1e-6, minPts = 2)

  expect_true(all(res$clusters == 0L))
  expect_true(all(res$core == FALSE))
})

test_that("dbscan validates eps and minPts", {
  x <- matrix(rnorm(10), ncol = 2)

  expect_error(dbscan(x, eps = 0, minPts = 3))
  expect_error(dbscan(x, eps = -1, minPts = 3))
  expect_error(dbscan(x, eps = NA, minPts = 3))
  expect_error(dbscan(x, eps = c(0.2, 0.3), minPts = 3))

  expect_error(dbscan(x, eps = 0.2, minPts = 0))
  expect_error(dbscan(x, eps = 0.2, minPts = NA))
  expect_error(dbscan(x, eps = 0.2, minPts = c(3, 4)))
})

test_that("dbscan validates input x through db_dist_matrix", {
  # NA = fail
  x_na <- matrix(c(0, 0, NA, 1), ncol = 2, byrow = TRUE)
  expect_error(dbscan(x_na, eps = 0.5, minPts = 2))

  # non-numeric column = fail
  df_bad <- data.frame(a = c(0, 1, 2), b = c("x", "y", "z"))
  expect_error(dbscan(df_bad, eps = 0.5, minPts = 2))
})

test_that("dbscan works with data.frame input", {
  x <- data.frame(a = c(0, 0.01, 1), b = c(0, 0.01, 1))
  res <- dbscan(x, eps = 0.05, minPts = 2)

  expect_equal(length(res$clusters), nrow(x))
})
