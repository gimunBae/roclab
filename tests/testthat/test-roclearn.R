test_that("roclearn training and prediction works", {
  set.seed(123)
  n_train <- 1500
  n_pos <- round(0.2 * n_train)
  n_neg <- n_train - n_pos

  X_train <- rbind(
    matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
    matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
  )
  y_train <- c(rep(-1, n_neg), rep(1, n_pos))

  n_test <- 300
  n_pos_test <- round(0.2 * n_test)
  n_neg_test <- n_test - n_pos_test
  X_test <- rbind(
    matrix(rnorm(2 * n_neg_test, mean = -1), ncol = 2),
    matrix(rnorm(2 * n_pos_test, mean =  1), ncol = 2)
  )

  fit <- roclearn(X_train, y_train, lambda = 0.1)

  expect_s3_class(fit, "roclearn")
  expect_true(is.numeric(fit$beta.hat))
  expect_equal(length(fit$beta.hat), ncol(X_train))
  expect_false(is.null(fit$intercept))

  preds <- predict(fit, X_test, type = "class")
  expect_true(all(preds %in% c(-1, 1)))
})

test_that("auc.roclearn computes AUC between 0 and 1", {
  set.seed(123)
  n_train <- 1500
  n_pos <- round(0.2 * n_train)
  n_neg <- n_train - n_pos

  X_train <- rbind(
    matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
    matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
  )
  y_train <- c(rep(-1, n_neg), rep(1, n_pos))

  n_test <- 300
  n_pos_test <- round(0.2 * n_test)
  n_neg_test <- n_test - n_pos_test
  X_test <- rbind(
    matrix(rnorm(2 * n_neg_test, mean = -1), ncol = 2),
    matrix(rnorm(2 * n_pos_test, mean =  1), ncol = 2)
  )
  y_test <- c(rep(-1, n_neg_test), rep(1, n_pos_test))

  fit <- roclearn(X_train, y_train, lambda = 0.1)
  auc_val <- auc.roclearn(fit, X_test, y_test)

  expect_true(is.numeric(auc_val))
  expect_gte(auc_val, 0)
  expect_lte(auc_val, 1)
})

test_that("summary.roclearn prints without error", {
  set.seed(123)
  n <- 1500
  n_pos <- round(0.2 * n)
  n_neg <- n - n_pos
  X <- rbind(
    matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
    matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
  )
  y <- c(rep(-1, n_neg), rep(1, n_pos))

  fit <- roclearn(X, y, lambda = 0.1)
  expect_invisible(summary(fit))
})

