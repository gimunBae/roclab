#' Compute AUC for a fitted kernel model
#'
#' Estimate the AUC (Area Under the ROC Curve) for a fitted kernel model on new data.
#'
#' @param object A fitted model object of class \code{"kroclearn"} (kernel model).
#' @param newdata A matrix or data.frame of test predictors. Must have the same
#'   structure as the training data (categorical variables are dummy-aligned
#'   automatically).
#' @param y Response vector of test labels (\{-1, 1\} or convertible).
#' @param ... Not used.
#'
#' @return A numeric scalar giving the estimated AUC.
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' n_train <- 100
#' r_train <- sqrt(runif(n_train, 0.05, 1))
#' theta_train <- runif(n_train, 0, 2*pi)
#' X_train <- cbind(r_train * cos(theta_train), r_train * sin(theta_train))
#' y_train <- ifelse(r_train < 0.5, 1, -1)
#'
#' n_test <- 10
#' r_test <- sqrt(runif(n_test, 0.05, 1))
#' theta_test <- runif(n_test, 0, 2*pi)
#' X_test <- cbind(r_test * cos(theta_test), r_test * sin(theta_test))
#' y_test <- ifelse(r_test < 0.5, 1, -1)
#'
#' fit <- kroclearn(X_train, y_train, lambda = 0.1,
#'   kernel = "radial", approx=TRUE)
#'
#' auc(fit, X_test, y_test)
auc.kroclearn <- function(object, newdata, y, ...) {
  if (!inherits(object, "kroclearn"))
    stop("Object must be of class 'kroclearn'.", call. = FALSE)

  # Normalize newdata to data.frame for consistent preprocessing
  X <- newdata
  if (is.matrix(X)) {
    X <- as.data.frame(X, stringsAsFactors = FALSE)
    if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(ncol(X)))
  } else if (is.data.frame(X)) {
    if (inherits(X, "tbl")) X <- as.data.frame(X, stringsAsFactors = FALSE)
  } else {
    stop("'newdata' must be a matrix or data.frame.", call. = FALSE)
  }

  # Apply the same dummy encoding as in training
  cat.vars <- object$preprocessing$cat.vars
  if (is.data.frame(X) && length(cat.vars)) {
    if (!requireNamespace("fastDummies", quietly = TRUE))
      stop("Package 'fastDummies' is required.", call. = FALSE)
    X <- fastDummies::dummy_cols(
      X,
      select_columns = cat.vars,
      remove_selected_columns = TRUE,
      remove_first_dummy = TRUE
    )
  }

  # Align columns to training layout
  train.cols <- object$preprocessing$colnames.
  removed    <- object$preprocessing$removed.cols
  if (is.data.frame(X) && length(removed))
    X <- X[, setdiff(colnames(X), removed), drop = FALSE]

  if (is.data.frame(X)) {
    missing <- setdiff(train.cols, colnames(X))
    if (length(missing)) for (nm in missing) X[[nm]] <- 0
    extra <- setdiff(colnames(X), train.cols)
    if (length(extra)) X <- X[, !(colnames(X) %in% extra), drop = FALSE]
    X <- X[, train.cols, drop = FALSE]
  }

  # To numeric matrix
  X <- as.matrix(X)
  storage.mode(X) <- "double"

  # Validate y and coerce to {-1,1}
  if (length(y) != nrow(X))
    stop("length(y) must equal nrow(newdata).", call. = FALSE)

  y <- as.vector(y)
  if (is.factor(y)) y <- as.character(y)
  if (is.character(y)) {
    u <- unique(y)
    if (length(u) == 2L) {
      y <- ifelse(y == u[1L], -1, 1)
    } else {
      stop("'y' must have exactly two classes (or be numeric -1/1).", call. = FALSE)
    }
  }
  if (!is.numeric(y))
    stop("'y' must be numeric or a 2-class factor/character.", call. = FALSE)
  if (any(!is.finite(y)))
    stop("y contains non-finite values after coercion.", call. = FALSE)
  if (setequal(unique(y), c(0, 1))) y <- ifelse(y == 0, -1, 1)
  if (!setequal(unique(y), c(-1, 1)))
    stop("'y' must contain only -1 and 1.", call. = FALSE)


  if (object$approx) {
    if (is.null(object$nystrom))
      stop("Nystrom approximation info missing in model object.", call. = FALSE)
    feature.matrix.test <- kernlab::kernelMatrix(object$kfunc, X, object$nystrom$landmarks) %*% object$nystrom$W.inv.sqrt
  } else{
    feature.matrix.test <- kernlab::kernelMatrix(object$kfunc,X,object$X)
  }

  theta <- object$theta.hat
  if (!is.numeric(theta) || anyNA(theta))
    stop("Model coefficients are missing, contain NA, or invalid.", call. = FALSE)
  scores <- drop(feature.matrix.test %*% theta)

  # Pairwise comparison AUC: proportion of positive-negative score differences > 0
  pos.idx <- which(y == 1)
  scores.plus  <- scores[pos.idx]
  scores.minus <- scores[-pos.idx]
  np <- length(scores.plus)
  nm <- length(scores.minus)

  i.idx <- rep(seq_len(np), each = nm)
  j.idx <- rep(seq_len(nm), times = np)
  delta.f <- scores.plus[i.idx] - scores.minus[j.idx]

  AUC <- sum(delta.f > 0) / length(delta.f)
  return(as.numeric(AUC))
}
