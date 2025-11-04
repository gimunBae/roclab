test_that("cv.kroclearn cross-validation works", {
  set.seed(123)
  n <- 1500
  r <- sqrt(runif(n, 0.05, 1))
  theta <- runif(n, 0, 2 * pi)
  X <- cbind(r * cos(theta), r * sin(theta))
  y <- ifelse(r < 0.5, 1, -1)

  cvfit <- cv.kroclearn(X, y,
                        lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),
                        kernel = "radial",nfolds=5)

  expect_s3_class(cvfit, "cv.kroclearn")
  expect_true(is.numeric(cvfit$optimal.lambda))
  expect_true(all(is.finite(cvfit$auc.mean)))
})

test_that("summary.cv.kroclearn prints without error", {
  set.seed(123)
  n <- 1500
  r <- sqrt(runif(n, 0.05, 1))
  theta <- runif(n, 0, 2 * pi)
  X <- cbind(r * cos(theta), r * sin(theta))
  y <- ifelse(r < 0.5, 1, -1)

  cvfit <- cv.kroclearn(X, y,
                        lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),
                        kernel = "radial",nfolds=5)
  expect_invisible(summary(cvfit))
})

test_that("plot.cv.kroclearn runs without error", {
  set.seed(123)
  n <- 1500
  r <- sqrt(runif(n, 0.05, 1))
  theta <- runif(n, 0, 2 * pi)
  X <- cbind(r * cos(theta), r * sin(theta))
  y <- ifelse(r < 0.5, 1, -1)

  cvfit <- cv.kroclearn(X, y,
                        lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),
                        kernel = "radial",nfolds=5)

  expect_invisible(plot(cvfit))
})
