#' Fit a Kernel AUC Maximization Model
#'
#' Train a kernel-based model that directly maximizes the Area Under the ROC Curve (AUC).
#' Several kernel types and loss functions are supported.
#' For large datasets, the algorithm can speed up training by using only a sampled subset of
#' positive–negative pairs (incomplete U-statistic approximation), together with a low-rank
#' approximation of the kernel matrix (Nyström approximation).
#'
#' @param X Predictor matrix or data.frame. Categorical variables are automatically
#'   one-hot encoded (first level dropped).
#' @param y Class labels in \{-1, 1\}. Labels given as \{0,1\} or as two-level
#'   factors/characters are coerced automatically.
#' @param lambda Positive scalar regularization parameter.
#' @param kernel Kernel type: \code{"radial"}, \code{"polynomial"},
#'   \code{"linear"}, or \code{"laplace"}.
#' @param param.kernel Kernel-specific parameter:
#'   \itemize{
#'     \item \eqn{\sigma} for \code{"radial"} and \code{"laplace"} kernels (default \eqn{1/p}).
#'     \item Degree for \code{"polynomial"} kernel (default 2).
#'     \item Ignored for \code{"linear"} kernel.
#'   }
#' @param loss Loss function:
#'   \code{"hinge"}, \code{"hinge2"} (squared hinge),
#'   \code{"logistic"}, or \code{"exponential"}.
#' @param approx Logical; if \code{TRUE}, train the kernel model using
#'   subsampled positive–negative pairs (incomplete U-statistic), and further
#'   apply a Nyström approximation to the kernel matrix. This reduces memory and
#'   time cost for large datasets. Default is \code{TRUE} when \code{nrow(X) >= 1000}.
#' @param intercept Logical; if \code{TRUE}, estimate an intercept term
#'   (default \code{TRUE}).
#' @param target.perf List with target sensitivity and specificity used when
#'   estimating the intercept (defaults to 0.9 each).
#' @param param.convergence List of convergence controls (\code{maxiter}, \code{eps}).
#'
#' @return An object of class \code{"kroclearn"}, a list containing:
#'   \itemize{
#'     \item \code{theta.hat} — estimated dual coefficient vector.
#'     \item \code{intercept} — fitted intercept (if applicable).
#'     \item \code{lambda}, \code{kernel}, \code{param.kernel}, \code{loss}.
#'     \item \code{approx}, \code{B} (number of sampled pairs if approximation used).
#'     \item \code{time} — training time (seconds).
#'     \item \code{nobs}, \code{p} — number of observations and predictors.
#'     \item \code{converged}, \code{n.iter} — convergence information.
#'     \item \code{kfunc} — kernel function object.
#'     \item \code{nystrom} — Nyström approximation details (if used).
#'     \item \code{X} — training data (post-preprocessing).
#'     \item \code{preprocessing} — details on categorical variables,
#'       removed columns, and column names.
#'     \item \code{call} — the function call.
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 1500
#' r <- sqrt(runif(n, 0.05, 1))
#' theta <- runif(n, 0, 2*pi)
#' X <- cbind(r * cos(theta), r * sin(theta))
#' y <- ifelse(r < 0.5, 1, -1)
#'
#' fit <- kroclearn(X, y, lambda = 0.1, kernel = "radial")
#' }
kroclearn <- function(
    X, y, lambda,
    kernel = "radial",
    param.kernel = NULL,
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
  n <- nrow(X)
  p <- ncol(X)
  # --- approx flag
  if (is.null(approx)) approx <- nrow(X) >= 1000L
  if (!is.logical(approx) || length(approx) != 1L)
    stop("'approx' must be a single logical.", call. = FALSE)

  # --- intercept flag
  if (!is.logical(intercept) || length(intercept) != 1L)
    stop("'intercept' must be a single logical.", call. = FALSE)

  # --- kernel & loss arg normalization --------------------------------------
  kernel <- match.arg(kernel, c("radial","polynomial","linear","laplace"))
  loss   <- match.arg(loss,   c("hinge","logistic","exponential","hinge2"))

  # --- convergence / perf controls ------------------------------------------
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

  # --- indices --------------------------------------------------------------
  plus.idx <- which(y > 0)
  np <- length(plus.idx)
  nm <- n - np
  if (np == 0L || nm == 0L)
    stop("Both classes must be present after preprocessing.", call. = FALSE)

  # --- training -------------------------------------------------------------
  B <- n
  d <- min(300, round(sqrt(n)))

  if (approx) {
    measure <- system.time({
      sub.idx <- sample.int(np*nm, size=B, replace=TRUE)
      i.idx <- (sub.idx - 1L) %/% nm + 1L
      j.idx <- sub.idx %% nm; j.idx[j.idx==0L] <- nm
      sub.indices <- cbind(i.idx,j.idx)

      nystrom <- nystrom.approx(X, y, d=d, kfunc=kfunc)
      Phi <- nystrom$phi
      Phi.plus <- Phi[plus.idx,,drop=FALSE]
      Phi.minus<- Phi[-plus.idx,,drop=FALSE]

      Phi.ij <- Phi.func.incomplete(Phi.plus, Phi.minus, sub.indices)
      theta.init <- rep(0,d)
      obj <- GD.nystrom(Phi.ij, lambda=lambda, loss=loss,
                        theta.init=theta.init, maxiter=maxiter, eps=eps)
    })
    feature.matrix <- nystrom$phi
  } else {
    measure <- system.time({
      K0 <- kernlab::kernelMatrix(kfunc, X, X)
      K0.plus  <- kernlab::kernelMatrix(kfunc, X[plus.idx,,drop=FALSE], X)
      K0.minus <- kernlab::kernelMatrix(kfunc, X[-plus.idx,,drop=FALSE], X)
      K.ij <- K.func.complete(K0.plus, K0.minus)
      theta.init <- rep(0,n)
      obj <- GD.nonlinear(K0=K0, K.ij=K.ij, lambda=lambda, loss=loss,
                          theta.init=theta.init, maxiter=maxiter, eps=eps)
    })
    feature.matrix <- K0
    nystrom <- NULL
  }
  time <- unname(measure["elapsed"])

  # --- intercept ------------------------------------------------------------
  b <- NA_real_
  if (isTRUE(intercept) && !any(is.na(obj$theta.hat))) {
    b <- find.intercept.kroclearn(feature.matrix=feature.matrix, y=y,
                                  theta.hat=obj$theta.hat,
                                  target.sens=target.sens,
                                  target.spec=target.spec)
  }

  # --- return S3 object -----------------------------------------------------
  res <- list(
    # coefficients
    theta.hat = obj$theta.hat,
    intercept = b,

    # training setup
    lambda = lambda,
    kernel = kernel,
    param.kernel = param.kernel,
    loss = loss,
    approx = approx,
    B = if (approx) B else NULL,

    # training results
    time = time,
    nobs = n,
    p = p,
    converged = if (!is.null(obj$converged)) obj$converged else NA,
    n.iter = if (!is.null(obj$n.iter)) obj$n.iter else NA,

    # kernel-specific internals
    kfunc = kfunc,
    nystrom = nystrom,
    X = X,

    # preprocessing + call
    preprocessing = list(
      cat.vars = cat.vars,
      removed.cols = removed.cols,
      colnames. = colnames(X)
    ),
    call = match.call()
  )
  class(res) <- "kroclearn"
  return(res)
}
