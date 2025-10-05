#' Predictions from a fitted linear model
#'
#' Generate predictions from a fitted linear model.
#'
#' @param object A fitted model object of class \code{"roclearn"} (linear).
#' @param newdata A data frame or matrix of predictors for which predictions
#'   are desired. Categorical variables are automatically dummy-encoded and
#'   aligned to the training structure.
#' @param type Prediction type: \code{"class"} for \{-1, 1\} labels, or
#'   \code{"response"} for raw decision scores.
#' @param ... Not used.
#'
#' @return A numeric vector of predictions (\{-1, 1\}) if \code{type = "class"},
#'   or raw decision scores if \code{type = "response"}.
#' @export
#'
#' @seealso \code{\link{roclearn}}, \code{\link{cv.roclearn}}
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
#'
#' # Predict classes {-1, 1}
#' predict(fit, X, type = "class")
#'
#' # Predict decision scores
#' predict(fit, X, type = "response")
#' }
predict.roclearn <- function(object, newdata, type = c("class", "response"), ...) {
  type <- match.arg(type, c("class", "response"))
  if (!inherits(object, "roclearn"))
    stop("Object must be of class 'roclearn'.", call. = FALSE)

  # Ensure valid input format
  X <- newdata
  if (nrow(X) == 0L) return(numeric(0))
  # Always coerce to data.frame first to allow dummy/alignment by names
  # (If the user passes a matrix, we temporarily wrap it as data.frame.)
  if (is.matrix(X)) {
    X <- as.data.frame(X, stringsAsFactors = FALSE)
    # If matrix had no colnames, create placeholder names to enable alignment
    if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(ncol(X)))
  } else if (is.data.frame(X)) {
    if (inherits(X, "tbl")) X <- as.data.frame(X)
  } else {
    stop("'newdata' must be a matrix or data.frame.", call. = FALSE)
  }
  if (!type %in% c("class", "response")) {
    stop("`type` must be either 'class' or 'response'.")
  }
  # Step 1: Apply the same dummy encoding as in training
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

  # Step 2: Align column structure with training data
  train.cols <- object$preprocessing$colnames.
  removed <- intersect(object$preprocessing$removed.cols, colnames(X))
  # Remove columns dropped during training
  if (is.data.frame(X) && length(removed))
    X <- X[, setdiff(colnames(X), removed), drop = FALSE]

  # Add missing columns as zeros
  if (is.data.frame(X)) {
    missing <- setdiff(train.cols, colnames(X))
    if (length(missing)) for (nm in missing) X[[nm]] <- 0
    # Remove extra columns
    extra <- setdiff(colnames(X), train.cols)
    if (length(extra)) X <- X[, !(colnames(X) %in% extra), drop = FALSE]
    # Reorder to match training
    X <- X[, train.cols, drop = FALSE]
  }

  # Convert to numeric matrix
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  # Step 3: Compute decision scores
  beta <- object$beta.hat
  if (!is.numeric(beta) || anyNA(beta))
    stop("Model coefficients are missing, contain NA, or invalid.", call. = FALSE)
  scores <- drop(X %*% beta)
  if (!is.null(object$intercept) && !is.na(object$intercept)) {
    scores <- scores + object$intercept
  } else {
      stop("Intercept is NA; cannot perform prediction. Set 'intercept=TRUE' during training.", call. = FALSE)
  }

  # Step 4: Return predictions
  if (type == "response")
    return(scores)

  return(ifelse(scores >= 0, 1, -1))
}

