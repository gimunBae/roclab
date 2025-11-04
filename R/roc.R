#' Plot Receiver Operating Characteristic (ROC) curve using ggroc
#'
#' Draws an ROC curve based on decision values. There is an option to display
#' the AUC in the plot title and to print the ROC summary object.
#'
#' @param y_true Response vector with class labels in \{-1, 1\}. Labels given as
#'   \{0, 1\} or as a two-level factor/character are automatically converted
#'   to this format.
#' @param y_score Numeric vector of predicted scores or decision values.
#' @param col Line color.
#' @param size Line width.
#' @param title Logical; if TRUE, displays AUC in the plot title.
#' @param summary Logical; if TRUE, prints the ROC object summary.
#' @param ... Additional arguments passed to pROC::ggroc().
#'
#' @return A ggplot object of the ROC curve.
#' @export
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
#' fit <- roclearn(X, y, lambda = 0.1, approx=TRUE)
#'
#' y_score <- predict(fit, X, type = "response")
#'
#' plot_roc(y, y_score)
plot_roc <- function(y_true, y_score, col = "blue", size = 1.2,
                     title = TRUE, summary = FALSE, ...) {
  # Convert labels to {-1, 1} if necessary
  y_true <- as.vector(y_true)
  if (is.factor(y_true)) y_true <- as.character(y_true)
  if (is.character(y_true)) {
    u <- unique(y_true)
    if (length(u) == 2L) {
      y_true <- ifelse(y_true == u[1], -1, 1)
    } else {
      stop("'y_true' must have exactly two classes (or be numeric -1/1).", call. = FALSE)
    }
  }
  if (!is.numeric(y_true))
    stop("'y_true' must be numeric or a 2-class factor/character.", call. = FALSE)
  if (any(!is.finite(y_true)))
    stop("y_true contains non-finite values after coercion.", call. = FALSE)
  if (setequal(unique(y_true), c(0, 1))) {
    warning("'y_true' contains {0, 1} labels; converting to {-1, 1}.", call. = FALSE)
    y_true <- ifelse(y_true == 0, -1, 1)
  }
  if (!setequal(unique(y_true), c(-1, 1)))
    stop("'y_true' must contain only -1 and 1.", call. = FALSE)

  # --- Ensure numeric y_score ---
  if (is.matrix(y_score) || is.data.frame(y_score)) {
    y_score <- as.vector(as.matrix(y_score))
  } else if (is.factor(y_score) || is.character(y_score)) {
    warning("'y_score' is not numeric; coercing to numeric.", call. = FALSE)
    y_score <- as.numeric(as.character(y_score))
  }
  if (!is.numeric(y_score))
    stop("'y_score' must be a numeric vector after coercion.", call. = FALSE)
  if (any(!is.finite(y_score)))
    stop("'y_score' contains non-finite values.", call. = FALSE)

  # --- Compute ROC and AUC ---
  roc_obj <- pROC::roc(y_true, y_score)
  auc_val <- round(pROC::auc(roc_obj), 4)

  # --- Title with AUC ---
  plot_title <- if (isTRUE(title)) {
    paste0("ROC Curve (AUC = ", auc_val, ")")
  } else {
    "ROC Curve"
  }

  # --- Plot ---
  g <- pROC::ggroc(roc_obj, color = col, size = size, ...) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if (summary) {
    print(roc_obj)
  }

  return(g)
}
