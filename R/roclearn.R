#' @importFrom stats plogis quantile sd
#' @importFrom graphics abline arrows legend points
#' @importFrom utils head
#' @importFrom ggplot2 ggplot geom_point geom_contour labs theme_minimal aes
#' @importFrom ggplot2 geom_line geom_errorbar geom_vline scale_x_log10 scale_color_manual theme scale_y_continuous
#' @importFrom fastDummies dummy_cols
#' @importFrom kernlab rbfdot polydot laplacedot kernelMatrix
#' @importFrom pracma pinv
#' @importFrom rsample vfold_cv
#' @importFrom dplyr select
#' @importFrom caret createFolds
#' @importFrom pROC roc auc ggroc
#' @importFrom ggplot2 ggtitle element_text
NULL
#' Fit a linear model
#'
#' @param X Predictor matrix or data.frame (categorical variables are
#'   automatically one-hot encoded).
#' @param y Response vector with class labels in \{-1, 1\}. Labels given as
#'   \{0, 1\} or as a two-level factor/character are automatically converted
#'   to this format.
#' @param lambda Positive scalar regularization parameter.
#' @param penalty Regularization penalty type:
#'   \code{"ridge"} (default), \code{"lasso"}, \code{"elastic"},
#'   \code{"alasso"}, \code{"scad"}, or \code{"mcp"}.
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
#'   For details about how approximation is applied, see the \code{details} section.
#' @param intercept Logical; include an intercept in the model (default \code{TRUE}).
#' @param target.perf List with target sensitivity and specificity used when
#'   estimating the intercept (defaults to 0.9 each).
#' @param param.convergence List of convergence controls (e.g., \code{maxiter},
#'   \code{eps}). Default is \code{list(maxiter = 5e4, eps = 1e-4)}.
#'
#' @return An object of class \code{"roclearn"}, a list containing:
#'   \itemize{
#'     \item \code{beta.hat} — estimated coefficient vector.
#'     \item \code{intercept} — fitted intercept (if applicable).
#'     \item \code{lambda}, \code{penalty}, \code{param.penalty}, \code{loss}.
#'     \item \code{approx}, \code{B} (number of sampled pairs if approximation used).
#'     \item \code{time} — training time (seconds).
#'     \item \code{nobs}, \code{p} — number of observations and predictors.
#'     \item \code{converged}, \code{n.iter} — convergence information.
#'     \item \code{preprocessing} — details on categorical variables,
#'       removed columns, and column names.
#'     \item \code{call} — the function call.
#'   }
#' @export
#'
#' @details
#' For large-scale data, the model is computationally prohibitive because its
#' loss is a U-statistic involving a double summation. To reduce this burden,
#' the package adopts an efficient algorithm based on an incomplete U-statistic,
#' which approximates the loss with a single summation. These approximations
#' substantially reduce computational cost and accelerate training, while
#' maintaining accuracy, making the model feasible for large-scale datasets.
#' This option is available when \code{approx = TRUE}.
#'
#' @examples
#' set.seed(123)
#' n <- 100
#' n_pos <- round(0.2 * n)
#' n_neg <- n - n_pos
#' X <- rbind(
#'   matrix(rnorm(2 * n_neg, mean = -1), ncol = 2),
#'   matrix(rnorm(2 * n_pos, mean =  1), ncol = 2)
#' )
#' y <- c(rep(-1, n_neg), rep(1, n_pos))
#'
#' fit <- roclearn(X, y, lambda = 0.1, penalty = "ridge", approx=TRUE)
roclearn <- function(
    X, y, lambda,
    penalty = "ridge",
    param.penalty = NULL,
    loss = "hinge",
    approx = NULL,
    intercept = TRUE,
    target.perf = list(),
    param.convergence = list()
) {
  # --- basic checks (shape + lambda)
  if (!(is.matrix(X) || is.data.frame(X)))
    stop("'X' must be a matrix or data.frame.", call. = FALSE)

  # Always convert to data.frame for consistency
  if (is.matrix(X))
    X <- as.data.frame(X, stringsAsFactors = FALSE)
  if (inherits(X, "tbl"))
    X <- as.data.frame(X, stringsAsFactors = FALSE)
  if (nrow(X) < 2L || ncol(X) < 1L)
    stop("X must have at least 2 rows and 1 column.", call. = FALSE)
  if (length(y) != nrow(X))
    stop("length(y) must equal nrow(X).", call. = FALSE)
  if (!is.numeric(lambda) || length(lambda) != 1L || !is.finite(lambda) || lambda <= 0)
    stop("lambda must be a positive finite scalar.", call. = FALSE)

  # --- reject list-columns (unsupported)
  if (is.data.frame(X)) {
    has.listcol <- any(vapply(X, function(col) is.list(col), logical(1)))
    if (has.listcol) stop("X must not contain list-columns.", call. = FALSE)
  }

  # --- y validation & coercion to numeric {-1,1}
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
  if (!is.numeric(y))
    stop("'y' must be numeric or a 2-class factor/character.", call. = FALSE)
  if (any(!is.finite(y)))
    stop("y contains non-finite values after coercion.", call. = FALSE)
  if (setequal(unique(y), c(0, 1))) {
    warning("'y' contains {0, 1} labels; converting to {-1, 1}.", call. = FALSE)
    y <- ifelse(y == 0, -1, 1)
  }
  if (!setequal(unique(y), c(-1, 1)))
    stop("'y' must contain only -1 and 1.", call. = FALSE)

  # --- check numeric columns of X for non-finite (categoricals handled later)
  if (is.data.frame(X)) {
    num.cols <- vapply(X, is.numeric, logical(1))
    if (any(num.cols)) {
      bad.num <- vapply(X[num.cols], function(col) any(!is.finite(col)), logical(1))
      if (any(bad.num)) {
        bad.cols <- names(which(bad.num))
        if (is.null(bad.cols)) {
          bad.cols <- paste0("V", which(bad.num))  # fallback to column index
        }
        stop(sprintf("Non-finite values in numeric columns: %s",
                     paste(bad.cols, collapse = ", ")), call. = FALSE)
      }
    }
  } else { # matrix path
    if (!is.numeric(X)) {
      warning("X is a non-numeric matrix; attempting to convert to data.frame for encoding.", call. = FALSE)
      X <- as.data.frame(X, stringsAsFactors = TRUE)
    } else {
      if (any(!is.finite(X)))
        stop("X contains non-finite values.", call. = FALSE)
    }
  }

  # --- drop constant (zero-variance) columns
  removed.cols <- character(0)
  if (ncol(X) > 0L) {
    const.idx <- vapply(seq_len(ncol(X)),
                        function(j) all(X[, j] == X[1, j]),
                        logical(1))
    if (any(const.idx)) {
      removed.cols <- colnames(X)[const.idx]
      warning(sprintf("Removing constant columns: %s", paste(removed.cols, collapse = ", ")), call. = FALSE)
      X <- X[, !const.idx, drop = FALSE]
    }
  }
  if (ncol(X) == 0L)
    stop("All predictors were constant or removed; no columns remain in X.", call. = FALSE)

  # --- detect categoricals and one-hot encode (remove first level)
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

  # --- finalize X as numeric matrix
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (any(!is.finite(X)))
    stop("X contains non-finite values after encoding.", call. = FALSE)

  # --- approx flag
  if (is.null(approx)) approx <- nrow(X) >= 1000L
  if (!is.logical(approx) || length(approx) != 1L)
    stop("'approx' must be a single logical.", call. = FALSE)

  # --- intercept flag
  if (!is.logical(intercept) || length(intercept) != 1L)
    stop("'intercept' must be a single logical.", call. = FALSE)

  # --- normalize option arguments
  penalty <- match.arg(penalty, c("ridge","lasso","elastic","alasso","scad","mcp"))
  loss    <- match.arg(loss,    c("hinge", "logistic", "exponential", "hinge2"))

  # --- penalty-specific parameter handling
  if (penalty %in% c("ridge", "lasso")) {
    if (!is.null(param.penalty))
      warning(sprintf("param.penalty is ignored for penalty = '%s'.", penalty), call. = FALSE)
    param.penalty <- NULL
  }  else if (penalty == "alasso") {
    if (is.null(param.penalty)) param.penalty <- 1
    if (!is.numeric(param.penalty) || length(param.penalty) != 1L ||
        !is.finite(param.penalty) || param.penalty <= 0)
      stop("For 'alasso', param.penalty must be a positive finite numeric scalar", call. = FALSE)
  }
  else if (penalty == "elastic") {
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

  # --- convergence controls
  if (!is.list(param.convergence))
    stop("'param.convergence' must be a list.", call. = FALSE)
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

  # --- target performance controls
  if (!is.list(target.perf))
    stop("'target.perf' must be a list.", call. = FALSE)
  target.perf <- utils::modifyList(
    list(sensitivity = 0.9, specificity = 0.9),
    target.perf
  )
  for (nm in c("sensitivity","specificity")) {
    v <- target.perf[[nm]]
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || v < 0 || v > 1))
      stop(sprintf("target.perf$%s must be a scalar in [0,1].", nm), call. = FALSE)
  }

  # --- training (AUC pairs sampling + solver) -------------------------------
  n <- nrow(X)
  p <- ncol(X)

  plus.idx <- which(y > 0)
  np <- length(plus.idx)
  nm <- n - np
  if (np == 0L || nm == 0L)
    stop("Both classes must be present after preprocessing.", call. = FALSE)

  X.plus <- X[plus.idx, , drop = FALSE]
  X.minus <- X[-plus.idx, , drop = FALSE]

  maxiter <- param.convergence$maxiter
  eps     <- param.convergence$eps
  target.sens <- target.perf$sensitivity
  target.spec <- target.perf$specificity

  B <- n
  total.pairs <- np * nm
  if (approx) {
    B.use <- min(B, total.pairs)
    measure <- system.time({
      sub.idx <- sample.int(total.pairs, size = B.use, replace = TRUE)
      i.idx <- (sub.idx - 1L) %/% nm + 1L
      j.idx <- sub.idx %% nm; j.idx[j.idx == 0L] <- nm
      sub.indices <- cbind(i.idx, j.idx)

      X.ij.incomplete <- X.func.incomplete(X.plus = X.plus, X.minus = X.minus, sub.indices = sub.indices)
      obj <- GD.linear(
        X.ij          = X.ij.incomplete,
        lambda        = lambda,
        penalty       = penalty,
        param.penalty = param.penalty,
        loss          = loss,
        maxiter       = maxiter,
        eps           = eps
      )
    })
  } else {
    measure <- system.time({
      X.ij.complete <- X.func.complete(X.plus = X.plus, X.minus = X.minus)
      obj <- GD.linear(
        X.ij          = X.ij.complete,
        lambda        = lambda,
        penalty       = penalty,
        param.penalty = param.penalty,
        loss          = loss,
        maxiter       = maxiter,
        eps           = eps
      )
    })
  }

  time <- unname(measure["elapsed"])

  success <- !any(is.na(obj$beta.hat))

  b <- NA_real_
  if (isTRUE(intercept) && success) {
    b <- find.intercept.roclearn(X, y, obj$beta.hat, target.sens, target.spec)
  }

  # --- S3 return object ------------------------------------------------------
  res <- list(
    beta.hat = obj$beta.hat,
    intercept = b,
    lambda = lambda,
    penalty = penalty,
    param.penalty = param.penalty,
    loss = loss,
    approx = approx,
    B = if (approx) B.use else NULL,
    time = time,
    nobs = n,
    p = p,
    converged = if (!is.null(obj$converged)) obj$converged else NA,
    n.iter = if (!is.null(obj$n.iter)) obj$n.iter else NA,
    preprocessing = list(
      cat.vars = cat.vars,
      removed.cols = removed.cols,
      colnames. = colnames(X)
    ),
    call = match.call()
  )
  class(res) <- "roclearn"
  return(res)
}
