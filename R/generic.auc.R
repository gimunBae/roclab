#' Generic function for AUC
#'
#' Compute Area Under the Curve (AUC) for a fitted model.
#' Dispatches to class-specific methods such as \code{auc.rocsvm}.
#'
#' @param object A fitted model object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Numeric scalar: estimated AUC.
#' @export
auc <- function(object, ...) {
  UseMethod("auc")
}
