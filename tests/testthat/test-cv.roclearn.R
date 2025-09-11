test_that("cv.roclearn cross-validation works", {
  set.seed(123)
  n <- 1500
  n_pos <- round(0.2 * n)
  n_neg <- n - n_pos

  X <- rbind(
    matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
    matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
  )
  y <- c(rep(-1, n_neg), rep(1, n_pos))

  cvfit <- cv.roclearn(X, y,
                       lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),nfolds=5)

  expect_s3_class(cvfit, "cv.roclearn")
  expect_true(is.numeric(cvfit$optimal.lambda))
  expect_true(all(is.finite(cvfit$auc.mean)))
})

test_that("summary.cv.roclearn prints without error", {
  set.seed(123)
  n <- 1500
  n_pos <- round(0.2 * n)
  n_neg <- n - n_pos
  X <- rbind(
    matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
    matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
  )
  y <- c(rep(-1, n_neg), rep(1, n_pos))

  cvfit <- cv.roclearn(X, y,
                       lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),nfolds=5)
  expect_invisible(summary(cvfit))
})

test_that("plot.cv.roclearn runs without error", {
  set.seed(123)
  n <- 1500
  n_pos <- round(0.2 * n)
  n_neg <- n - n_pos

  X <- rbind(
    matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
    matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
  )
  y <- c(rep(-1, n_neg), rep(1, n_pos))

  cvfit <- cv.roclearn(X, y,
                       lambda.vec = exp(seq(log(0.01), log(5), length.out = 20)),nfolds=5)

  expect_invisible(plot(cvfit))
})
