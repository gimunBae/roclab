#' Cross-validation for Kernel AUC Maximization Models
#'
#' Perform k-fold cross-validation over a sequence of \eqn{\lambda} values
#' and select the optimal kernel model based on cross-validated AUC.
#'
#' @param X Predictor matrix or data.frame (categorical variables are
#'   automatically one-hot encoded).
#' @param y Response vector with class labels in \{-1, 1\}. Labels given as
#'   \{0,1\} or as a two-level factor/character are coerced automatically.
#' @param lambda.vec Optional numeric vector of penalty values. If \code{NULL} (default),
#'   a decreasing sequence is generated automatically.
#' @param lambda.length Number of \eqn{\lambda} values to generate if
#'   \code{lambda.vec} is \code{NULL}. Default is 30.
#' @param kernel Kernel type: \code{"radial"} (default), \code{"polynomial"},
#'   \code{"linear"}, or \code{"laplace"}.
#' @param param.kernel Kernel-specific parameter:
#'   \itemize{
#'     \item \eqn{\sigma} for \code{"radial"} and \code{"laplace"} kernels
#'       (default \eqn{1/p}, where \eqn{p} is the number of predictors after preprocessing,
#'       i.e., after categorical variables are one-hot encoded).
#'     \item Degree for \code{"polynomial"} kernel (default 2).
#'     \item Ignored for \code{"linear"} kernel.
#'   }
#' @param loss Loss function: \code{"hinge"} (default), \code{"hinge2"} (squared hinge),
#'   \code{"logistic"}, or \code{"exponential"}.
#' @param approx Logical; if \code{TRUE}, train the kernel model using
#'   subsampled positive–negative pairs (incomplete U-statistic), and further
#'   apply a Nyström approximation to the kernel matrix. This reduces memory and
#'   time cost for large datasets.
#'   Default is \code{TRUE} when \code{nrow(X) >= 1000}, otherwise \code{FALSE}.
#' @param intercept Logical; include an intercept in the model (default \code{TRUE}).
#' @param nfolds Number of cross-validation folds (default 10).
#' @param target.perf List with target sensitivity and specificity used when
#'   estimating the intercept (defaults to 0.9 each).
#' @param param.convergence List of convergence controls (e.g., \code{maxiter},
#'   \code{eps}). Default is \code{list(maxiter = 5e4, eps = 1e-4)}.
#'
#' @return An object of class \code{"cv.kroclearn"} with:
#'   \itemize{
#'     \item \code{optimal.lambda} — selected \eqn{\lambda}.
#'     \item \code{optimal.fit} — model refit on the full data at
#'       \code{optimal.lambda}.
#'     \item \code{lambda.vec} — grid of penalty values considered.
#'     \item \code{auc.mean}, \code{auc.sd} — mean and sd of cross-validated AUC.
#'     \item \code{auc.result} — fold-by-lambda AUC matrix.
#'     \item \code{time.mean}, \code{time.sd} — mean and sd of training time.
#'     \item \code{time.result} — fold-by-lambda training time matrix.
#'     \item \code{nfolds}, \code{loss}, \code{kernel} — settings.
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' ## Kernel model example
#' set.seed(123)
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
#' cvfit$optimal.lambda
#' }
cv.kroclearn <- function(
    X, y,
    lambda.vec = NULL,
    lambda.length = 30,
    kernel = "radial",
    param.kernel = NULL,
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


  n <- nrow(X)
  p <- ncol(X)
  # --- Approximation flag (default: TRUE if n >= 1000)
  if (is.null(approx)) approx <- nrow(X) >= 1000L
  if (!is.logical(approx) || length(approx) != 1L)
    stop("'approx' must be a single logical.", call. = FALSE)

  # --- Intercept flag
  if (!is.logical(intercept) || length(intercept) != 1L)
    stop("'intercept' must be a single logical.", call. = FALSE)

  # --- normalize args
  kernel <- match.arg(kernel, c("radial","polynomial","linear","laplace"))
  loss   <- match.arg(loss,   c("hinge","logistic","exponential","hinge2"))

  if (!is.list(param.convergence))
    stop("'param.convergence' must be a list.", call. = FALSE)
  param.convergence <- utils::modifyList(
    list(maxiter = 5e4, eps = 1e-4),
    param.convergence
  )
  maxiter <- as.integer(param.convergence$maxiter)
  eps     <- param.convergence$eps

  if (!is.list(target.perf))
    stop("'target.perf' must be a list.", call. = FALSE)
  target.perf <- utils::modifyList(
    list(sensitivity=0.9, specificity=0.9),
    target.perf
  )
  target.sens <- target.perf$sensitivity
  target.spec <- target.perf$specificity
  # --- kernel function setup ------------------------------------------------
  if (kernel=="radial") {
    if (is.null(param.kernel)) param.kernel <- 1/p
    kfunc <- kernlab::rbfdot(sigma=param.kernel)
  } else if (kernel=="polynomial") {
    if (is.null(param.kernel)) param.kernel <- 2
    kfunc <- kernlab::polydot(degree=param.kernel, scale=(1/p)^param.kernel, offset=1)
  } else if (kernel=="linear") {
    if (!is.null(param.kernel)) warning("'param.kernel' ignored for linear kernel.")
    kfunc <- kernlab::polydot(degree=1, scale=1/p, offset=0)
  } else if (kernel=="laplace") {
    if (is.null(param.kernel)) param.kernel <- 1/p
    kfunc <- kernlab::laplacedot(sigma=param.kernel)
  } else stop("Unsupported kernel type.")

  # --- lambda sequence if not given
  if (is.null(lambda.vec)) {
    lambda.max <- 1
    lambda.min.ratio <- 1e-3
    lambda.vec <- exp(seq(
      log(lambda.max),
      log(lambda.max * lambda.min.ratio),
      length.out = lambda.length
    ))
  }
  if (length(lambda.vec) < 2L)
    stop("'lambda.vec' must contain at least two values.")

  # --- stratified CV folds
  data.train <- data.frame(X, y = y)
  cv.folds <- rsample::vfold_cv(data.train, v = nfolds, strata = "y")

  time.result <- NULL
  auc.result  <- NULL
  diverge.lambda <- NULL

  # --- loop over folds
  for (k in seq_len(nfolds)) {
    split <- cv.folds$splits[[k]]
    data.train.cv <- rsample::analysis(split)
    data.val <- rsample::assessment(split)
    X.cv <- dplyr::select(data.train.cv, -"y")
    y.cv <- data.train.cv$y
    X.val <- dplyr::select(data.val, -"y")
    y.val <- data.val$y
    time.vec <- NULL
    auc.vec <- NULL

    for (j in seq_along(lambda.vec)) {
      lam <- lambda.vec[j]
      if (lam %in% diverge.lambda) {
        time.vec <- c(time.vec, NA)
        auc.vec <- c(auc.vec, NA)
      } else {
        fit <- kroclearn(
          X = X.cv, y = y.cv, lambda = lam,
          kernel = kernel, param.kernel = param.kernel,
          loss = loss, approx = approx, intercept = FALSE,
          param.convergence = list(maxiter=maxiter, eps=eps)
        )
        if (all(is.na(fit$theta.hat))){
          diverge.lambda <- c(diverge.lambda, lambda)
          time.vec[j] <- NA
          auc.vec[j]  <- NA
        } else{
          auc <- auc.kroclearn(fit, X.val, y.val)
          time.vec[j] <- fit$time
          auc.vec[j]  <- auc
        }
      }
    }
    time.result <- rbind(time.result, time.vec)
    auc.result  <- rbind(auc.result, auc.vec)
  }

  # --- aggregate
  time.mean <- apply(time.result, 2, mean, na.rm=TRUE)
  time.sd   <- apply(time.result, 2, sd, na.rm=TRUE)
  auc.mean  <- apply(auc.result, 2, mean, na.rm=TRUE)
  auc.sd    <- apply(auc.result, 2, sd, na.rm=TRUE)
  # --- Report excluded (non-converged) lambdas
  for (l in unique(sort(diverge.lambda))) {
    message <- paste0("lambda = ", l, " was excluded because the algorithm did not converge.")
    print(message)
  }
  if (all(is.na(auc.mean)))
    stop("The algorithm did not converge for any of the lambda values. Please set 'lambda.vec' to a different range.")

  # --- choose optimal
  j <- which(auc.mean == max(auc.mean, na.rm=TRUE))
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
  optimal.fit <- kroclearn(
    X = X, y = y, lambda = optimal.lambda,
    kernel = kernel, param.kernel = param.kernel,
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
    kernel         = kernel,
    loss           = loss
  )
  class(out) <- "cv.kroclearn"
  return(out)
}
