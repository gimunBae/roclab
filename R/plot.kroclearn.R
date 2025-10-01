#' Plot Decision Boundary for Kernel AUC Maximization Models
#'
#' Visualize the fitted decision function of a kernel AUC maximization model
#' using two selected features. A scatter plot of the observed data is overlaid
#' with the decision boundary where \eqn{f(x) = 0}.
#'
#' Recommended when the dataset has only two features. For datasets with more
#' than two features, the plot fixes all non-selected features at baseline
#' values (e.g., zero or mean) and shows a 2D slice of the decision boundary
#' on the chosen feature pair. The selected features must therefore be chosen
#' carefully to obtain a meaningful boundary.
#'
#' @param x A fitted model object of class \code{"kroclearn"} (kernel).
#' @param ... Additional arguments passed as a list:
#'   \itemize{
#'     \item \code{newdata}: Predictor matrix or data.frame for visualization.
#'     \item \code{y}: Corresponding labels (\{-1, 1\} or convertible).
#'     \item \code{features}: Two feature names or indices (before preprocessing)
#'       to be displayed on the axes.
#'     \item \code{levels}: Optional named list giving levels for categorical
#'       features (needed if the feature is expanded into multiple dummies).
#'     \item \code{grid_points}: Number of grid points per axis (default 200).
#'     \item \code{point_size}: Size of scatter plot points (default 1.8).
#'     \item \code{point_alpha}: Transparency of scatter points (default 0.8).
#'     \item \code{others}: How to fix non-selected features when generating
#'       the grid. One of \code{"mean"} (default), or \code{"median"}
#'   }
#'
#' @return A \code{ggplot2} object showing the training data points and the
#'   estimated decision boundary.
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 1500
#' r <- sqrt(runif(n, 0.05, 1))
#' theta <- runif(n, 0, 2*pi)
#' X <- cbind(r * cos(theta), r * sin(theta))
#' y <- ifelse(r < 0.5, 1, -1)
#'
#' fit <- kroclearn(X, y, lambda = 0.1, kernel = "radial")
#'
#' # Plot decision boundary on the first two features
#' plot(fit, newdata = X, y = y, features = c(1, 2))
#' }
plot.kroclearn <- function(x, ...) {
  object <- x
  args <- list(...)
  newdata    <- args$newdata
  y          <- args$y
  features   <- args$features
  levels     <- args$levels
  grid_points <- args$grid_points %||% 200
  point_size <- args$point_size %||% 1.8
  point_alpha<- args$point_alpha %||% 0.8
  others     <- args$others %||% "mean"
  others <- match.arg(others,c("mean","median"))
  # --- Basic checks ---
  if (!inherits(object, "kroclearn"))
    stop("object must be of class 'kroclearn'.", call. = FALSE)
  if (missing(features) || length(features) != 2L)
    stop("'features' must be length-2 (names or indices).", call. = FALSE)

  # --- Keep a copy of raw column names BEFORE dummy/removal (for index mapping) ---
  X <- newdata
  if (is.matrix(X)) {
    X <- as.data.frame(X, stringsAsFactors = FALSE)
    if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(ncol(X)))
  } else if (is.data.frame(X)) {
    if (inherits(X, "tbl")) X <- as.data.frame(X, stringsAsFactors = FALSE)
  } else {
    stop("'newdata' must be a matrix or data.frame.", call. = FALSE)
  }
  orig_cn <- colnames(X)

  # --- Apply training-time preprocessing to X ---
  cat.vars <- object$preprocessing$cat.vars
  if (is.data.frame(X) && length(cat.vars)) {
    if (!requireNamespace("fastDummies", quietly = TRUE))
      stop("Package 'fastDummies' is required.", call. = FALSE)
    X <- fastDummies::dummy_cols(
      X, select_columns = cat.vars,
      remove_selected_columns = TRUE,
      remove_first_dummy = TRUE
    )
  }
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
  X <- as.matrix(X)
  storage.mode(X) <- "double"

  # --- y to {-1,1} ---
  if (length(y) != nrow(X)) stop("length(y) must equal nrow(newdata).", call. = FALSE)
  y <- as.vector(y)
  if (is.factor(y)) y <- as.character(y)
  if (is.character(y)) {
    u <- unique(y)
    if (length(u) == 2L) y <- ifelse(y == u[1L], -1, 1) else
      stop("'y' must have exactly two classes (or be numeric -1/1).", call. = FALSE)
  }
  if (!is.numeric(y)) stop("'y' must be numeric or a 2-class factor/character.", call. = FALSE)
  if (any(!is.finite(y))) stop("y contains non-finite values after coercion.", call. = FALSE)
  if (setequal(unique(y), c(0, 1))) y <- ifelse(y == 0, -1, 1)
  if (!setequal(sort(unique(y)), c(-1, 1))) stop("'y' must contain only -1 and 1.", call. = FALSE)

  # --- Coefs & intercept ---
  theta <- object$theta.hat
  if (!is.numeric(theta) || anyNA(theta))
    stop("Model coefficients are missing, contain NA, or invalid.", call. = FALSE)
  intercept <- 0
  if (!is.null(object$intercept) &&
      is.numeric(object$intercept) &&
      length(object$intercept) == 1L) {
    intercept <- as.numeric(object$intercept)
  }

  # --- Resolve features to POST-preprocessing column indices ---
  cn_final <- colnames(X)
  resolve_one <- function(feat) {
    # feat can be name (raw or final) or index (raw)
    raw_name <- NULL
    if (is.character(feat)) {
      raw_name <- feat
    } else if (is.numeric(feat) && length(feat) == 1L) {
      if (feat < 1 || feat > length(orig_cn))
        stop("Feature index out of bounds for raw newdata.", call. = FALSE)
      raw_name <- orig_cn[feat]
    } else {
      stop("Each feature must be a single name or a single integer index.", call. = FALSE)
    }

    # Exact match in final columns?
    if (raw_name %in% cn_final) {
      return(list(idx = match(raw_name, cn_final), label = raw_name))
    }

    # Otherwise try dummy-expanded columns with prefix "<raw_name>_"
    # (fastDummies default naming)
    cand <- grep(paste0("^", gsub("([\\W])","\\\\\\1", raw_name), "_"), cn_final, value = TRUE)
    if (length(cand) == 0L) {
      stop(sprintf("Cannot find feature '%s' after preprocessing.", raw_name), call. = FALSE)
    }
    # Need a specific level
    lev <- NULL
    if (!is.null(levels) && !is.null(levels[[raw_name]])) {
      lev <- as.character(levels[[raw_name]])
      # dummy column likely "<raw_name>_<lev>"
      want <- paste0(raw_name, "_", lev)
      if (!(want %in% cand)) {
        stop(sprintf("Level '%s' for '%s' not found. Candidates: %s",
                     lev, raw_name, paste(cand, collapse = ", ")), call. = FALSE)
      }
      return(list(idx = match(want, cn_final), label = want))
    } else {
      fallback <- cand[1L]
      warning(sprintf(
        "Feature '%s' expanded into multiple dummy columns. No level specified, using '%s' by default.",
        raw_name, fallback
      ), call. = FALSE)
      return(list(idx = match(fallback, cn_final), label = fallback))
    }
  }

  r1 <- resolve_one(features[1])
  r2 <- resolve_one(features[2])
  j1 <- r1$idx; j2 <- r2$idx
  f1_name <- cn_final[j1]; f2_name <- cn_final[j2]

  # --- Baseline for non-selected features ---
  if (others != "zero") {
    # compute per-column baseline on observed X (post-preproc)
    if (others == "mean") {
      base_vec <- colMeans(X)
    } else {
      base_vec <- apply(X, 2, stats::median)
    }
  } else {
    base_vec <- rep(0, ncol(X))
  }

  # --- Scatter data (observed) ---
  df_test <- data.frame(
    X1 = X[, j1],
    X2 = X[, j2],
    y  = factor(y, levels = c(-1, 1))
  )

  # --- Grid over the two chosen features; others fixed to baseline ---
  x1_range <- seq(min(df_test$X1, na.rm = TRUE),
                  max(df_test$X1, na.rm = TRUE),
                  length.out = grid_points)
  x2_range <- seq(min(df_test$X2, na.rm = TRUE),
                  max(df_test$X2, na.rm = TRUE),
                  length.out = grid_points)
  grid <- expand.grid(X1 = x1_range, X2 = x2_range)
  p <- ncol(X)
  G <- matrix(rep(base_vec, each = nrow(grid)), nrow = nrow(grid), ncol = p, byrow = FALSE)
  G[, j1] <- grid$X1
  G[, j2] <- grid$X2
  if (object$approx) {
    grid.K <- kernlab::kernelMatrix(object$kfunc, G, object$nystrom$landmarks) %*% object$nystrom$W.inv.sqrt
  } else {
    grid.K <- kernlab::kernelMatrix(object$kfunc, G, object$X)
  }
  fval <- drop(grid.K %*% theta) + intercept
  grid$fval <- fval

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.", call. = FALSE)

  p <-  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = df_test,
      ggplot2::aes(x = X1, y = X2, color = y),
      size = point_size, alpha = point_alpha
    ) +
    ggplot2::labs(
      title = paste("Kernel ROC-LEARN (", object$kernel, ")", sep = ""),
      x = f1_name, y = f2_name, color = "Class"
    ) +
    ggplot2::theme_minimal()

  # Try adding the decision boundary
  contour_added <- FALSE
  tryCatch({
    # Attempt to draw the contour line at f(x) = 0
    p <- p + ggplot2::geom_contour(
      data = grid,
      ggplot2::aes(x = X1, y = X2, z = fval),
      breaks = 0, color = "black", linewidth = 1
    )
    contour_added <- TRUE
  }, warning = function(w) {
    # If the warning indicates that no contour could be generated
    if (grepl("Zero contours were generated", conditionMessage(w))) {
      warning("The selected feature combination cannot properly represent the decision boundary. Please try different features.", call. = FALSE)
    } else {
      # Forward any other warnings as-is
      warning(conditionMessage(w), call. = FALSE)
    }
  }, error = function(e) {
    # Handle unexpected errors during contour computation
    warning("The selected feature combination cannot properly represent the decision boundary. Please try different features.", call. = FALSE)
  })

  # If no contour was added, return scatter plot only
  if (!contour_added) {
    return(p)
  }

  p
}
