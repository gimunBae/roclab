#' Cross-validation for linear models
#'
#' Perform k-fold cross-validation over a sequence of \eqn{\lambda} values and
#' select the optimal model based on AUC.
#'
#' @param X Predictor matrix or data.frame (categorical variables are
#'   automatically one-hot encoded).
#' @param y Response vector with class labels in \{-1, 1\}. Labels given as
#'   \{0, 1\} or as a two-level factor/character are automatically converted
#'   to this format.
#' @param lambda.vec Optional numeric vector of regularization parameters (lambda values).
#'    If \code{NULL} (default), a decreasing sequence is generated automatically.
#' @param lambda.length Number of \eqn{\lambda} values to generate if
#'   \code{lambda.vec} is \code{NULL}. Default is 30.
#' @param penalty Regularization penalty type: \code{"ridge"} (default), \code{"lasso"},
#'   \code{"elastic"}, \code{"alasso"}, \code{"scad"}, or \code{"mcp"}.
#' @param param.penalty Penalty-specific parameter:
#'   \itemize{
#'     \item Ignored for \code{"ridge"} and \code{"lasso"}.
#'     \item Mixing parameter \eqn{\alpha \in (0,1)} for \code{"elastic"}. Default is 0.5.
#'     \item Adaptive weight exponent \eqn{\gamma > 0} for \code{"alasso"}. Default is 1.
#'     \item Tuning parameter (default 3.7) for \code{"scad"} and \code{"mcp"}.
#'   }
#' @param loss Surrogate loss function type. One of:
#'   \code{"hinge"} (default), \code{"hinge2"} (squared hinge),
#'   \code{"logistic"}, or \code{"exponential"}.
#' @param approx Logical; enables a scalable approximation to accelerate training.
#'   The default is \code{TRUE} when \code{nrow(X) >= 1000}, and \code{FALSE} otherwise.
#'   For details about how approximation is applied, see the \code{details}
#'   section of the \code{roclearn} function.
#' @param intercept Logical; include an intercept in the model (default \code{TRUE}).
#' @param nfolds Number of cross-validation folds (default 10).
#' @param target.perf List with target sensitivity and specificity used when
#'   estimating the intercept (defaults to 0.9 each).
#' @param param.convergence List of convergence controls (e.g., \code{maxiter},
#'   \code{eps}). Default is \code{list(maxiter = 5e4, eps = 1e-4)}.
#'
#' @return An object of class \code{"cv.roclearn"} with:
#'   \itemize{
#'     \item \code{optimal.lambda} — selected \eqn{\lambda}.
#'     \item \code{optimal.fit} — model refit on the full data at
#'       \code{optimal.lambda}.
#'     \item \code{lambda.vec} — grid of penalty values considered.
#'     \item \code{auc.mean}, \code{auc.sd} — mean and sd of cross-validated AUC.
#'     \item \code{auc.result} — fold-by-lambda AUC matrix.
#'     \item \code{time.mean}, \code{time.sd} — mean and sd of training time.
#'     \item \code{time.result} — fold-by-lambda training time matrix.
#'     \item \code{nfolds}, \code{loss}, \code{penalty} — settings.
#'   }
#' @export
#'
#' @seealso \code{\link{roclearn}}
#'
#' @examples
#' \donttest{
#' set.seed(123)
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
#'   X, y,
#'   lambda.vec = exp(seq(log(0.01), log(5), length.out = 5)),
#'   nfolds = 5
#' )
#' cvfit$optimal.lambda
#' }
cv.roclearn <- function(
    X, y,
    lambda.vec= NULL,
    lambda.length = 30,
    penalty = "ridge",
    param.penalty = NULL,
    loss = "hinge",
    approx = NULL,
    intercept = TRUE,
    nfolds = 10,
    target.perf = list(),
    param.convergence = list()
) {
  # --- Basic checks for input X and y
  if (!(is.matrix(X) || is.data.frame(X)))
    stop("'X' must be a matrix or data.frame.", call. = FALSE)
  # Always convert to data.frame for consistency
  if (is.matrix(X)) X <- as.data.frame(X, stringsAsFactors = FALSE)
  if (inherits(X, "tbl")) X <- as.data.frame(X, stringsAsFactors = FALSE)
  if (nrow(X) < 2L || ncol(X) < 1L)
    stop("X must have at least 2 rows and 1 column.", call. = FALSE)
  if (length(y) != nrow(X))
    stop("length(y) must equal nrow(X).", call. = FALSE)
  if (!is.null(lambda.vec)) {
    if (!is.numeric(lambda.vec) || any(!is.finite(lambda.vec)) || any(lambda.vec <= 0)) {
      stop("'lambda.vec' must be a numeric vector of positive finite values.", call. = FALSE)
    }
    if (length(lambda.vec) < 2L) {
      stop("'lambda.vec' must contain at least two values.", call. = FALSE)
    }
  }

  # --- Reject list-columns in X
  if (is.data.frame(X)) {
    has.listcol <- any(vapply(X, function(col) is.list(col), logical(1)))
    if (has.listcol) stop("X must not contain list-columns.", call. = FALSE)
  }

  # --- Validate and coerce y to numeric {-1, 1}
  y <- as.vector(y)
  if (is.factor(y)) y <- as.character(y)
  if (is.character(y)) {
    u <- unique(y)
    if (length(u) == 2L) {
      y <- ifelse(y == u[1], -1, 1)
    } else {
      stop("'y' must have exactly two classes (or be numeric -1/1).", call. = FALSE)
    }
  }
  if (!is.numeric(y)) stop("'y' must be numeric or a 2-class factor/character.", call. = FALSE)
  if (any(!is.finite(y))) stop("y contains non-finite values after coercion.", call. = FALSE)
  if (setequal(unique(y), c(0, 1))) {
    warning("'y' contains {0, 1} labels; converting to {-1, 1}.", call. = FALSE)
    y <- ifelse(y == 0, -1, 1)
  }
  if (!setequal(unique(y), c(-1, 1)))
    stop("'y' must contain only -1 and 1.", call. = FALSE)

  # --- Check numeric columns of X for non-finite values
  if (is.data.frame(X)) {
    num.cols <- vapply(X, is.numeric, logical(1))
    if (any(num.cols)) {
      bad.num <- vapply(X[num.cols], function(col) any(!is.finite(col)), logical(1))
      if (any(bad.num)) {
        bad.cols <- names(which(bad.num))
        if (is.null(bad.cols)) bad.cols <- paste0("V", which(bad.num)) # fallback to index
        stop(sprintf("Non-finite values in numeric columns: %s", paste(bad.cols, collapse = ", ")),
             call. = FALSE)
      }
    }
  } else {
    # Matrix path
    if (!is.numeric(X)) {
      warning("X is a non-numeric matrix; attempting to convert to data.frame for encoding.", call. = FALSE)
      X <- as.data.frame(X, stringsAsFactors = TRUE)
    } else {
      if (any(!is.finite(X))) stop("X contains non-finite values.", call. = FALSE)
    }
  }
  # --- Drop constant (zero-variance) columns
  removed.cols <- character(0)
  if (ncol(X) > 0L) {
    const.idx <- vapply(seq_len(ncol(X)), function(j) all(X[, j] == X[1, j]), logical(1))
    if (any(const.idx)) {
      removed.cols <- colnames(X)[const.idx]
      warning(sprintf("Removing constant columns: %s", paste(removed.cols, collapse = ", ")),
              call. = FALSE)
      X <- X[, !const.idx, drop = FALSE]
    }
  }
  if (ncol(X) == 0L)
    stop("All predictors were constant or removed; no columns remain in X.", call. = FALSE)

  # --- Detect categoricals and apply one-hot encoding (remove first dummy)
  cat.vars <- character(0)
  if (is.data.frame(X)) {
    cat.vars <- names(X)[vapply(X, function(col) is.character(col) || is.factor(col), logical(1))]
  }
  if (length(cat.vars)) {
    for (var in cat.vars) {
      message(sprintf("[%s]: treated as categorical and one-hot encoded.", var))
    }
    if (!requireNamespace("fastDummies", quietly = TRUE))
      stop("Package 'fastDummies' is required.", call. = FALSE)
    X <- fastDummies::dummy_cols(
      X,
      select_columns = cat.vars,
      remove_selected_columns = TRUE,
      remove_first_dummy = TRUE
    )
  }

  # --- Finalize X as numeric matrix
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (any(!is.finite(X)))
    stop("X contains non-finite values after encoding.", call. = FALSE)



  # --- Approximation flag (default: TRUE if n >= 1000)
  if (is.null(approx)) approx <- nrow(X) >= 1000L
  if (!is.logical(approx) || length(approx) != 1L)
    stop("'approx' must be a single logical.", call. = FALSE)

  # --- Intercept flag
  if (!is.logical(intercept) || length(intercept) != 1L)
    stop("'intercept' must be a single logical.", call. = FALSE)

  # --- Normalize option arguments
  penalty <- match.arg(penalty, c("ridge","lasso","elastic","alasso","scad","mcp"))
  loss <- match.arg(loss, c("hinge", "logistic", "exponential", "hinge2"))

  # --- Penalty-specific parameter handling
  if (penalty %in% c("ridge", "lasso")) {
    if (!is.null(param.penalty))
      warning(sprintf("param.penalty is ignored for penalty = '%s'.", penalty), call. = FALSE)
    param.penalty <- NULL
  }  else if (penalty == "alasso") {
    if (is.null(param.penalty)) param.penalty <- 1
    if (!is.numeric(param.penalty) || length(param.penalty) != 1L ||
        !is.finite(param.penalty) || param.penalty <= 0)
      stop("For 'alasso', param.penalty must be a positive finite numeric scalar", call. = FALSE)
  }else if (penalty == "elastic") {
    if (is.null(param.penalty)) param.penalty <- 0.5
    if (!is.numeric(param.penalty) || length(param.penalty) != 1L ||
        !is.finite(param.penalty) || param.penalty <= 0 || param.penalty >= 1)
      stop("For 'elastic', param.penalty must be a finite numeric scalar in (0,1).", call. = FALSE)
  } else if (penalty %in% c("scad", "mcp")) {
    if (is.null(param.penalty)) param.penalty <- 3.7
    if (!is.numeric(param.penalty) || length(param.penalty) != 1L ||
        !is.finite(param.penalty) || param.penalty <= 0)
      stop(sprintf("For '%s', param.penalty must be a positive finite numeric scalar.", penalty), call. = FALSE)
  }

  # --- Convergence controls
  if (!is.list(param.convergence)) stop("'param.convergence' must be a list.", call. = FALSE)
  param.convergence <- utils::modifyList(
    list(
      maxiter = 5e4,
      eps = if (penalty %in% c("scad", "mcp")) 1e-3 else 1e-4
    ),
    param.convergence
  )
  if (!is.numeric(param.convergence$maxiter) || length(param.convergence$maxiter) != 1L ||
      !is.finite(param.convergence$maxiter) || param.convergence$maxiter <= 0)
    stop("param.convergence$maxiter must be a positive finite numeric scalar.", call. = FALSE)
  param.convergence$maxiter <- as.integer(param.convergence$maxiter)
  if (!is.numeric(param.convergence$eps) || length(param.convergence$eps) != 1L ||
      !is.finite(param.convergence$eps) || param.convergence$eps <= 0)
    stop("param.convergence$eps must be a positive finite numeric scalar.", call. = FALSE)

  maxiter <- param.convergence$maxiter
  eps <- param.convergence$eps

  # --- Construct lambda sequence if not provided
  if (is.null(lambda.vec)) {
    lambda.max <- compute.lambda.max(X, y, penalty, param.penalty, loss, approx, maxiter, eps)
    lambda_min_ratio <- 1e-3
    lambda.vec <- exp(seq(
      log(lambda.max),
      log(lambda.max * lambda_min_ratio),
      length.out = lambda.length
    ))
  }
  if (length(lambda.vec) == 1)
    stop("lambda.vec should have more than one value")

  # --- Cross-validation folds (stratified by y)
  data.train <- data.frame(X, y = y)
  cv.folds <- rsample::vfold_cv(data.train, v = nfolds, strata = "y")

  time.result <- NULL
  auc.result <- NULL
  diverge.lambda <- NULL

  # --- Loop over folds
  for (i in 1:nfolds) {
    time.vec <- NULL
    auc.vec <- NULL
    split <- cv.folds$splits[[i]]
    data.train.cv <- rsample::analysis(split)
    data.val <- rsample::assessment(split)
    X.cv <- dplyr::select(data.train.cv, -"y")
    y.cv <- data.train.cv$y
    X.val <- dplyr::select(data.val, -"y")
    y.val <- data.val$y

    L <- length(lambda.vec)
    for (i in 1:L) {
      lambda <- lambda.vec[i]
      if (lambda %in% diverge.lambda) {
        # Skip if previously failed
        time.vec <- c(time.vec, NA)
        auc.vec <- c(auc.vec, NA)
      } else {
        # Train ROC-SVM on training fold
        fit <- roclearn(
          X = X.cv, y = y.cv, lambda = lambda,
          penalty = penalty, param.penalty = param.penalty,
          loss = loss, approx = approx, intercept = FALSE,
          param.convergence = list(maxiter = maxiter, eps = eps)
        )
        # Mark non-converged solutions
        if (all(is.na(fit$beta.hat))){
          diverge.lambda <- c(diverge.lambda, lambda)
          time.vec <- c(time.vec, NA)
          auc.vec <- c(auc.vec, NA)
        } else{
          auc <- auc.roclearn(fit, X.val, y.val)
          time.vec <- c(time.vec, fit$time)
          auc.vec <- c(auc.vec, auc)
        }
      }
    }
    time.result <- rbind(time.result, time.vec)
    auc.result <- rbind(auc.result, auc.vec)
  }

  # --- Aggregate CV results
  time.mean <- apply(time.result, 2, mean)
  time.sd <- apply(time.result, 2, sd)
  auc.mean <- apply(auc.result, 2, mean)
  auc.sd <- apply(auc.result, 2, sd)

  # --- Report excluded (non-converged) lambdas
  for (l in unique(sort(diverge.lambda))) {
    message <- paste0("lambda = ", l, " was excluded because the algorithm did not converge.")
    print(message)
  }

  # --- Fail if no valid results
  if (all(is.na(auc.mean)))
    stop("The algorithm did not converge for any of the lambda values. Please set 'lambda.vec' to a different range.")

  # --- Select optimal lambda (maximize mean AUC; tie-breaking by time and sd)
  j <- which(auc.mean == max(auc.mean, na.rm = TRUE))
  if (length(j) > 1) {
    sub.auc.sd <- auc.sd[j]
    sub.time.mean <- time.mean[j]
    sub.time.sd <- time.sd[j]
    order.j <- order(sub.time.mean, sub.auc.sd, sub.time.sd)
    optimal.index <- j[order.j[1]]
  } else {
    optimal.index <- j
  }
  optimal.lambda <- lambda.vec[optimal.index]
  optimal.fit <- roclearn(
    X = X, y = y, lambda = optimal.lambda,
    penalty = penalty, param.penalty = param.penalty,
    loss = loss, approx = approx, intercept = intercept,
    target.perf = target.perf,
    param.convergence = param.convergence
  )
  out <- list(
    optimal.lambda = optimal.lambda,
    optimal.fit    = optimal.fit,
    lambda.vec     = lambda.vec,
    auc.mean       = auc.mean,
    auc.sd         = auc.sd,
    auc.result     = auc.result,
    time.mean      = time.mean,
    time.sd        = time.sd,
    time.result    = time.result,
    nfolds         = nfolds,
    penalty        = penalty,
    loss           = loss
  )
  class(out) <- "cv.roclearn"
  return(out)
}
