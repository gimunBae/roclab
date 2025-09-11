#' Summarize Cross-Validation Results (Kernel Model)
#'
#' Print a concise summary of cross-validation results for a kernel
#' AUC maximization model. Reports the number of folds, the set of
#' candidate \eqn{\lambda} values, the selected optimal \eqn{\lambda},
#' and corresponding cross-validated AUC statistics.
#'
#' @param object A fitted cross-validation object of class
#'   \code{"cv.kroclearn"} (kernel).
#' @param ... Not used.
#'
#' @details
#' This is a method for the generic \code{summary()} function, applied to
#' objects of class \code{"cv.kroclearn"}. It prints training settings
#' (loss, kernel type, number of folds, number of candidate \eqn{\lambda}),
#' the selected optimal \eqn{\lambda}, the corresponding mean and standard
#' deviation of cross-validated AUC, and a truncated table of AUC results
#' across candidate \eqn{\lambda} values.
#'
#' @return Invisibly returns the input \code{object}, after printing a summary
#'   to the console.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#'
#' ## Kernel model cross-validation
#' n <- 1500
#' r <- sqrt(runif(n, 0.05, 1))
#' theta <- runif(n, 0, 2*pi)
#' X <- cbind(r * cos(theta), r * sin(theta))
#' y <- ifelse(r < 0.5, 1, -1)
#'
#' cvfit <- cv.kroclearn(
#'   X, y,
#'   lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),
#'   kernel = "radial",
#'   nfolds = 5
#' )
#'
#' summary(cvfit)
#' }
summary.cv.kroclearn <- function(object, ...) {
  if (!inherits(object, "cv.kroclearn"))
    stop("object must be of class 'cv.kroclearn'.", call. = FALSE)

  cat("Cross-validated Kernel Model (AUC Maximization)\n")
  cat("------------------------------\n")

  # Call
  if (!is.null(object$call)) {
    cat("Call:\n")
    print(object$call)
    cat("\n")
  }

  # Settings
  cat("Loss:", object$loss, "\n")
  cat("Kernel:", object$kernel, "\n")
  cat("Number of folds:", object$nfolds, "\n")
  cat("Number of candidate lambdas:", length(object$lambda.vec), "\n\n")

  # Optimal lambda
  cat("Optimal lambda:", object$optimal.lambda, "\n")
  idx.opt <- which.min(abs(object$lambda.vec - object$optimal.lambda))
  auc.opt <- object$auc.mean[idx.opt]
  sd.opt  <- object$auc.sd[idx.opt]
  cat("Cross-validated AUC at optimal lambda:",
      sprintf("%.4f (+/- %.4f)", auc.opt, sd.opt), "\n\n")

  # Global performance summary
  cat("Overall AUC (mean +/- sd) across lambdas:\n")
  auc.summary <- data.frame(
    lambda = signif(object$lambda.vec, 3),
    auc.mean = round(object$auc.mean, 4),
    auc.sd   = round(object$auc.sd, 4)
  )
  print(utils::head(auc.summary, 10))
  if (length(object$lambda.vec) > 10) cat("...\n")

  invisible(object)
}
