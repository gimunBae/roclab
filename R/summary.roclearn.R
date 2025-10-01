#' Summarize a fitted linear AUC model
#'
#' Display key information from a fitted \code{"roclearn"} object, including:
#' data dimensions, model specification, convergence status, training time,
#' and leading coefficient estimates.
#'
#' @param object A fitted model of class \code{"roclearn"}.
#' @param ... Unused.
#'
#' @return Invisibly returns \code{object} after printing a formatted summary.
#' @export
#'
#' @seealso \code{\link{roclearn}}, \code{\link{summary.kroclearn}},
#'   \code{\link{cv.roclearn}}, \code{\link{cv.kroclearn}}
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 1500
#' n_pos <- round(0.2 * n)
#' n_neg <- n - n_pos
#' X <- rbind(
#'   matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
#'   matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
#' )
#' y <- c(rep(-1, n_neg), rep(1, n_pos))
#'
#' fit <- roclearn(X, y, lambda = 0.1)
#' summary(fit)
#' }
summary.roclearn <- function(object, ...) {
  if (!inherits(object, "roclearn"))
    stop("object must be of class 'roclearn'.", call. = FALSE)

  cat("Linear Model Summary (AUC Maximization)\n")
  cat("----------------------\n")

  # Call
  if (!is.null(object$call)) {
    cat("Call:\n")
    print(object$call)
    cat("\n")
  }

  # Data info
  cat("Number of observations:", object$nobs, "\n")
  cat("Number of predictors:", object$p, "\n\n")

  # Model specification
  cat("Penalty:", object$penalty, "\n")
  cat("Lambda:", object$lambda, "\n")
  if (!is.null(object$param.penalty))
    cat("Penalty parameter:", object$param.penalty, "\n")
  cat("Loss function:", object$loss, "\n")
  if (object$approx) {
    cat("Approximation: Yes (B =", object$B, ")\n")
  } else {
    cat("Approximation: No\n")
  }
  cat("\n")

  # Optimization info
  intercept <- if (!is.null(object$intercept)) round(object$intercept, 4) else NA
  cat("Intercept:", intercept, "\n")

  converged <- if (!is.null(object$converged)) object$converged else NA
  cat("Converged:", converged, "\n")

  n.iter <- if (!is.null(object$n.iter)) object$n.iter else NA
  cat("Iterations:", n.iter, "\n")

  time <- if (!is.null(object$time)) round(object$time, 3) else NA
  cat("Training time (sec):", time, "\n\n")

  # Coefficients
  if (!is.null(object$beta.hat) && length(object$beta.hat) > 0) {
    cat("Coefficients (first 10):\n")
    print(utils::head(object$beta.hat, 10))
    if (length(object$beta.hat) > 10) cat("...\n\n")
  }

  # Preprocessing
  if (!is.null(object$preprocessing$cat.vars) &&
      length(object$preprocessing$cat.vars) > 0) {
    cat("Categorical variables dummy-encoded:\n")
    print(object$preprocessing$cat.vars)
  }
  if (!is.null(object$preprocessing$removed.cols) &&
      length(object$preprocessing$removed.cols) > 0) {
    cat("Removed columns:\n")
    print(object$preprocessing$removed.cols)
  }

  invisible(object)
}
