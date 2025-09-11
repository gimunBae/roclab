if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("lower", "upper"))
}
#' Plot Cross-Validation Curve for AUC Models
#'
#' Produce a visualization of cross-validation results from a fitted
#' \code{cv.roclearn} object. The plot shows the mean AUC across regularization
#' parameters \eqn{\lambda}, with error bars reflecting the cross-validation
#' standard deviation. Optionally, the selected optimal \eqn{\lambda} is
#' highlighted with a dashed line and marker.
#'
#' @param x A cross-validation object of class \code{"cv.roclearn"}.
#' @param highlight Logical; if \code{TRUE}, mark the selected optimal
#'   \eqn{\lambda} with a vertical dashed line and a red point (default
#'   \code{TRUE}).
#' @param ... Additional arguments passed to underlying \pkg{ggplot2} functions.
#'
#' @details
#' This function is a method for the generic \code{plot()} function, designed
#' specifically for cross-validation objects from \code{cv.roclearn}.
#' The x-axis is displayed on a log scale for \eqn{\lambda}, and the y-axis
#' represents mean AUC values. Error bars show variability across folds.
#'
#' @return A \code{ggplot2} object is returned and drawn to the current device.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#'
#' ## Simulated linear classification
#' n <- 1500
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
#'   X, y,lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),nfolds=5)
#'
#' # Plot CV curve with highlighted optimal lambda
#' plot(cvfit)
#' }
plot.cv.roclearn <- function(x, highlight = TRUE, ...) {
  object <- x
  stopifnot(inherits(object, "cv.roclearn"))

  lambda    <- object$lambda.vec
  auc.mean  <- object$auc.mean
  auc.sd    <- object$auc.sd
  opt.lambda <- object$optimal.lambda

  df <- data.frame(
    lambda   = lambda,
    auc.mean = auc.mean,
    lower    = auc.mean - auc.sd,
    upper    = auc.mean + auc.sd
  )

  p <- ggplot(df, aes(x = lambda, y = auc.mean)) +
    geom_line(aes(color = "Mean AUC")) +
    geom_point(aes(color = "Mean AUC")) +
    geom_errorbar(aes(ymin = lower, ymax = upper, color = "SD"),
                  width = 0.05) +
    scale_x_log10() +
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
    theme(legend.position = "right")

  print(p)
}
