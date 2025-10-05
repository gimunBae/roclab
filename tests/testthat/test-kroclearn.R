test_that("kroclearn training and prediction works", {
  set.seed(123)
  n_train <- 1500
  r <- sqrt(runif(n_train, 0.05, 1))
  theta <- runif(n_train, 0, 2 * pi)
  X_train <- cbind(r * cos(theta), r * sin(theta))
  y_train <- ifelse(r < 0.5, 1, -1)

  n_test <- 300
  r_test <- sqrt(runif(n_test, 0.05, 1))
  theta_test <- runif(n_test, 0, 2 * pi)
  X_test <- cbind(r_test * cos(theta_test), r_test * sin(theta_test))

  fit <- kroclearn(X_train, y_train, lambda = 0.1, kernel = "radial")

  expect_s3_class(fit, "kroclearn")
  expect_true(is.numeric(fit$theta.hat))
  expect_false(is.null(fit$intercept))

  preds <- predict(fit, X_test, type = "class")
  expect_true(all(preds %in% c(-1, 1)))
})

test_that("auc.kroclearn computes AUC between 0 and 1", {
  set.seed(123)
  n_train <- 1500
  r <- sqrt(runif(n_train, 0.05, 1))
  theta <- runif(n_train, 0, 2 * pi)
  X_train <- cbind(r * cos(theta), r * sin(theta))
  y_train <- ifelse(r < 0.5, 1, -1)

  n_test <- 300
  r_test <- sqrt(runif(n_test, 0.05, 1))
  theta_test <- runif(n_test, 0, 2 * pi)
  X_test <- cbind(r_test * cos(theta_test), r_test * sin(theta_test))
  y_test <- ifelse(r_test < 0.5, 1, -1)

  fit <- kroclearn(X_train, y_train, lambda = 0.1, kernel = "radial")
  auc_val <- auc.kroclearn(fit, X_test, y_test)

  expect_true(is.numeric(auc_val))
  expect_gte(auc_val, 0)
  expect_lte(auc_val, 1)
})

test_that("summary.kroclearn prints without error", {
  set.seed(123)
  n <- 1500
  r <- sqrt(runif(n, 0.05, 1))
  theta <- runif(n, 0, 2 * pi)
  X <- cbind(r * cos(theta), r * sin(theta))
  y <- ifelse(r < 0.5, 1, -1)

  fit <- kroclearn(X, y, lambda = 0.1, kernel = "radial")
  expect_invisible(summary(fit))
})
