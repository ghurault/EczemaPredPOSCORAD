test_that("compute_accuracy works", {
  N_post <- 1000
  y <- c(1, 3)
  pred <- lapply(1:length(y), function(x) {rep(1, N_post)})
  expect_equal(compute_accuracy(y, pred, 1), c(1, 0))
  expect_error(compute_accuracy(y, do.call(rbind, pred), 1))
})

test_that("compute_quantile_error works", {
  N_post <- 1000
  y <- c(1, 3)
  pred <- lapply(1:length(y), function(x) {rep(1, N_post)})
  expect_equal(compute_quantile_error(y, pred, 0.5), c(0, 2))
  expect_error(compute_quantile_error(y, do.call(rbind, pred), 1))
})
