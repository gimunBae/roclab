#' Compute AUC for a fitted linear model
#'
#' Estimate the AUC (Area Under the ROC Curve) for a fitted linear model on new data.
#'
#' @param object A fitted model object of class \code{"roclearn"} (linear model).
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
#' n_pos <- round(0.2 * n_train)
#' n_neg <- n_train - n_pos
#'
#' X_train <- rbind(
#'   matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
#'   matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
#' )
#' y_train <- c(rep(-1, n_neg), rep(1, n_pos))
#'
#' n_test <- 10
#' n_pos_test <- round(0.2 * n_test)
#' n_neg_test <- n_test - n_pos_test
#'
#' X_test <- rbind(
#'   matrix(rnorm(2 * n_neg_test, mean = -1), ncol = 2),
#'   matrix(rnorm(2 * n_pos_test, mean =  1), ncol = 2)
#' )
#' y_test <- c(rep(-1, n_neg_test), rep(1, n_pos_test))
#'
#' fit <- roclearn(X_train, y_train, lambda = 0.1, approx=TRUE)
#'
#' auc(fit, X_test, y_test)
auc.roclearn <- function(object, newdata, y, ...) {
  if (!inherits(object, "roclearn"))
    stop("Object must be of class 'roclearn'.", call. = FALSE)

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

  # Compute decision scores (intercept does not affect AUC ordering)
  beta <- object$beta.hat
  if (!is.numeric(beta) || anyNA(beta))
    stop("Model coefficients are missing, contain NA, or invalid.", call. = FALSE)
  scores <- drop(X %*% beta)

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
