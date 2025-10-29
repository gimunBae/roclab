if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("lower", "upper"))
}
#' Visualize Cross-Validation results for linear models
#'
#' Produce a visualization of cross-validation results from a fitted
#' \code{cv.roclearn} object. The plot shows the mean AUC across
#' regularization parameters \eqn{\lambda}, with error bars reflecting
#' the cross-validation standard deviation. Optionally, the selected
#' optimal \eqn{\lambda} is highlighted with a dashed line and marker.
#'
#' @param x A cross-validation object of class \code{"cv.roclearn"}.
#' @param highlight Logical; if \code{TRUE}, mark the selected optimal
#'   \eqn{\lambda} with a vertical dashed line with a red point (default
#'   \code{TRUE}).
#' @param ... Additional arguments passed to underlying \pkg{ggplot2} functions.
#'
#' @details
#' This function is a method for the generic \code{plot()} function,
#' designed specifically for cross-validation objects from
#' \code{cv.roclearn}. The x-axis is displayed on a log scale for
#' \eqn{\lambda}, and the y-axis represents AUC values. Error bars
#' show variability across folds.
#'
#' @return A \code{ggplot2} object is returned and drawn to the current device.
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' n <- 100
#' n_pos <- round(0.2 * n)
#' n_neg <- n - n_pos
#'
#' X <- rbind(
#'   matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
#'   matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
#' )
#' y <- c(rep(-1, n_neg), rep(1, n_pos))
#'
#' cvfit <- cv.roclearn(
#'   X, y,lambda.vec = exp(seq(log(0.01), log(5), length.out = 3)),
#'   approx=TRUE, nfolds = 2)
#'
#' plot(cvfit)
plot.cv.roclearn <- function(x, highlight = TRUE, ...) {
  object <- x
  stopifnot(inherits(object, "cv.roclearn"))

  lambda    <- object$lambda.vec
  auc.mean  <- object$auc.mean
  auc.sd    <- object$auc.sd
  opt.lambda <- object$optimal.lambda

  df <- data.frame(
    lambda = lambda,
    auc.mean = auc.mean,
    lower = pmax(auc.mean - auc.sd, 0),
    upper = pmin(auc.mean + auc.sd, 1)
  )
  if (min(df$lower) < 0.5) {
    ylimits <- c(0, 1)
  } else {
    ylimits <- c(0.5, 1)
  }
  p <- ggplot(df, aes(x = lambda, y = auc.mean)) +
    geom_line(aes(color = "Mean AUC")) +
    geom_point(aes(color = "Mean AUC")) +
    geom_errorbar(aes(ymin = lower, ymax = upper, color = "SD"),
                  width = 0.05) +
    scale_x_log10() +
    ggplot2::scale_y_continuous(limits = ylimits) +
    labs(
      title = expression("Cross-validated AUC vs " * lambda),
      x = expression(log(lambda)),
      y = "AUC"
    )
    theme_minimal()

  # --- highlight optimal lambda
  if (highlight) {
    opt.df <- df[which.min(abs(df$lambda - opt.lambda)), ]
    p <- p +
      geom_vline(xintercept = opt.lambda, linetype = "dashed", color = "red") +
      geom_point(data = opt.df,
                 aes(x = lambda, y = auc.mean, color = "Optimal lambda"),
                 size = 3)
  }

  # --- legend
  p <- p +
    scale_color_manual(
      values = c("Mean AUC" = "blue", "SD" = "gray40", "Optimal lambda" = "red")
    ) +
    theme(legend.position = "none")

  print(p)
}
