#' Summarize a fitted kernel AUC model
#'
#' Display key information from a fitted \code{"kroclearn"} object, including:
#' data dimensions, kernel specification, convergence status, training time,
#' and leading coefficient estimates.
#'
#' @param object A fitted model of class \code{"kroclearn"}.
#' @param ... Unused.
#'
#' @return Invisibly returns \code{object} after printing a formatted summary.
#' @export
#'
#' @seealso \code{\link{kroclearn}}, \code{\link{summary.roclearn}},
#'   \code{\link{cv.kroclearn}}, \code{\link{cv.roclearn}}
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 1500
#' r <- sqrt(runif(n, 0.05, 1))
#' theta <- runif(n, 0, 2*pi)
#' X <- cbind(r * cos(theta), r * sin(theta))
#' y <- ifelse(r < 0.5, 1, -1)
#'
#' fit <- kroclearn(X, y, lambda = 0.1, kernel = "radial")
#' summary(fit)
#' }
summary.kroclearn <- function(object, ...) {
  if (!inherits(object, "kroclearn"))
    stop("object must be of class 'kroclearn'.", call. = FALSE)

  cat("Kernel Model Summary (AUC Maximization)\n")
  cat("-----------------------------\n")

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
  cat("Kernel:", object$kernel, "\n")
  if (!is.null(object$param.kernel))
    cat("Kernel parameter:", object$param.kernel, "\n")
  cat("Lambda:", object$lambda, "\n")
  cat("Loss function:", object$loss, "\n")
  if (object$approx) {
    cat("Approximation: Yes (Nystrom, B =", object$B, ")\n")
    if (!is.null(object$nystrom$landmarks))
      cat("  Nystrom landmarks:", nrow(object$nystrom$landmarks), "\n")
  } else {
    cat("Approximation: No (full kernel)\n")
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
  if (!is.null(object$theta.hat) && length(object$theta.hat) > 0) {
    cat("Theta coefficients (first 10):\n")
    print(utils::head(object$theta.hat, 10))
    if (length(object$theta.hat) > 10) cat("...\n\n")
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
