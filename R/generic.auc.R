#' Generic function for AUC
#'
#' Compute AUC (Area Under the ROC Curve) for a fitted model.
#' Dispatches to class-specific methods such as \code{auc.roclearn}.
#'
#' @param object A fitted model object.
#' @param ... Additional arguments passed to methods.
#'
#' @return Numeric scalar: estimated AUC.
#' @export
auc <- function(object, ...) {
  UseMethod("auc")
}
