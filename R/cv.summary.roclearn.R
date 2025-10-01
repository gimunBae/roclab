#' Summarize Cross-Validation Results (Linear Model)
#'
#' Print a concise summary of cross-validation results for a linear
#' AUC maximization model. Reports the number of folds, the set of
#' candidate \eqn{\lambda} values, the selected optimal \eqn{\lambda},
#' and corresponding cross-validated AUC statistics.
#'
#' @param object A fitted cross-validation object of class
#'   \code{"cv.roclearn"} (linear).
#' @param ... Not used.
#'
#' @details
#' This is a method for the generic \code{summary()} function, applied to
#' objects of class \code{"cv.roclearn"}. It prints training settings
#' (loss, penalty, number of folds, number of candidate \eqn{\lambda}),
#' the selected optimal \eqn{\lambda}, the corresponding mean and standard
#' deviation of cross-validated AUC, and a truncated table of AUC results
#' across candidate \eqn{\lambda} values.
#'
#' @return Invisibly returns the input \code{object}, after printing a summary
#'   to the console.
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' ## Linear model cross-validation
#' n <- 1500
#' n_pos <- round(0.2 * n)
#' n_neg <- n - n_pos
#' X <- rbind(
#'   matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
#'   matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
#' )
#' y <- c(rep(-1, n_neg), rep(1, n_pos))
#'
#' cvfit <- cv.roclearn(
#'   X, y,
#'   lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),
#'   nfolds = 5
#' )
#'
#' summary(cvfit)
#' }
summary.cv.roclearn <- function(object, ...) {
  if (!inherits(object, "cv.roclearn"))
    stop("object must be of class 'cv.roclearn'.", call. = FALSE)

  cat("Cross-validated Linear Model (AUC Maximization)\n")
  cat("-----------------------\n")

  # Call
  if (!is.null(object$call)) {
    cat("Call:\n")
    print(object$call)
    cat("\n")
  }

  # Settings
  cat("Loss:", object$loss, "\n")
  cat("Penalty:", object$penalty, "\n")
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
  # filter out rows where auc.mean == 0 or NA
  n.na  <- sum(is.na(auc.summary$auc.mean))
  n.zero <- sum(auc.summary$auc.mean == 0, na.rm = TRUE)

  if (n.na + n.zero > 0) {
    auc.summary <- auc.summary[!is.na(auc.summary$auc.mean) & auc.summary$auc.mean != 0, ]

    if (n.na > 0) {
      cat("Note:", n.na, "lambda value(s) were excluded (NA AUC).\n")
    }
    if (n.zero > 0) {
      cat("Note:", n.zero, "lambda value(s) were excluded (zero AUC).\n")
    }
    cat("\n")
  }
  print(utils::head(auc.summary, 10))
  if (length(object$lambda.vec) > 10) cat("...\n")

  invisible(object)
}
