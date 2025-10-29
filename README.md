# roclab

<!-- badges: start -->
[![R-CMD-check](https://github.com/gimunBae/roclab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gimunBae/roclab/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package for **ROC-Optimizing Binary Classifiers**. It provides both
**linear** and **kernel** models:

- **Linear models** provide various **regularization penalties**
  (ridge, lasso, alasso, elastic net, scad, mcp).
- **Kernel models** support a range of kernel functions
  (radial, polynomial, linear, laplace).
- **Both models** include a variety of surrogate loss functions
  (hinge, hinge2 (squared hinge), logistic, exponential).

For large datasets, scalability is achieved by approximating the empirical
loss using **incomplete U-statistics**, and by applying a **Nyström low-rank
approximation** to the kernel matrix. These approximations reduce computational
cost while preserving accuracy, making ROC-Optimizing Binary Classifiers
feasible in large-scale data.

In addition, efficient optimization is performed using **Gradient Descent with
the Adamax update rule** —a variant of Adam, based on the infinity norm—for
linear models with the ridge penalty and kernel models. For linear models with
other penalties (i.e., those involving variable selection), **Proximal Gradient
Descent with an Adamax adaptive learning rate scheme** is employed.

## Installation

You can install the development version of roclab like so:

``` r
devtools::install_github("gimunBae/roclab")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(roclab)
set.seed(123)

# ========================
# Linear model example
# ========================
n_lin <- 1500
n_pos_lin <- round(0.2 * n_lin)
n_neg_lin <- n_lin - n_pos_lin

X_train_lin <- rbind(
  matrix(rnorm(2 * n_neg_lin, mean = -1), ncol = 2),
  matrix(rnorm(2 * n_pos_lin, mean =  1), ncol = 2)
)
y_train_lin <- c(rep(-1, n_neg_lin), rep(1, n_pos_lin))

n_test_lin <- 300
n_pos_test_lin <- round(0.2 * n_test_lin)
n_neg_test_lin <- n_test_lin - n_pos_test_lin
X_test_lin <- rbind(
  matrix(rnorm(2 * n_neg_test_lin, mean = -1), ncol = 2),
  matrix(rnorm(2 * n_pos_test_lin, mean =  1), ncol = 2)
)
y_test_lin <- c(rep(-1, n_neg_test_lin), rep(1, n_pos_test_lin))

# Fit a linear model
fit_lin <- roclearn(X_train_lin, y_train_lin, lambda = 0.1)

# Summary
summary(fit_lin)

# AUC on the test set
auc(fit_lin, X_test_lin, y_test_lin)

# Predict classes {-1, 1}
predict(fit_lin, X_test_lin, type = "class")

# 5-fold CV 
cvfit_lin <- cv.roclearn(
  X_train_lin, y_train_lin,
  lambda.vec = exp(seq(log(0.01), log(5), length.out = 20)),
  nfolds = 5
)

# Summarize cross-validation results
summary(cvfit_lin)

# Plot the cross-validation AUC across lambda values
plot(cvfit_lin)


# ========================
# Kernel model example
# ========================
n_ker <- 1500
r_train_ker <- sqrt(runif(n_ker, 0.05, 1))
theta_train_ker <- runif(n_ker, 0, 2*pi)
X_train_ker <- cbind(r_train_ker * cos(theta_train_ker), r_train_ker * sin(theta_train_ker))
y_train_ker <- ifelse(r_train_ker < 0.5, 1, -1)

n_test_ker <- 300
r_test_ker <- sqrt(runif(n_test_ker, 0.05, 1))
theta_test_ker <- runif(n_test_ker, 0, 2*pi)
X_test_ker <- cbind(r_test_ker * cos(theta_test_ker), r_test_ker * sin(theta_test_ker))
y_test_ker <- ifelse(r_test_ker < 0.5, 1, -1)

# Fit a kernel model
fit_ker <- kroclearn(X_train_ker, y_train_ker, lambda = 0.1, kernel = "radial")

# Summary
summary(fit_ker)

# AUC on the test set
auc(fit_ker, X_test_ker, y_test_ker)

# Predict classes {-1, 1}
predict(fit_ker, X_test_ker, type = "class")

# 5-fold CV 
cvfit_ker <- cv.kroclearn(
  X_train_ker, y_train_ker,
  lambda.vec = exp(seq(log(0.01), log(5), length.out = 20)),
  kernel = "radial",
  nfolds = 5
)

# Summarize cross-validation results
summary(cvfit_ker)

# Plot the cross-validation AUC across lambda values
plot(cvfit_ker)
```

## License

License: MIT (see [LICENSE](LICENSE) file).

## Citation

If you use this package in academic work, please cite as:

Bae, G., & Shin, S. J. (2025). *roclab: ROC-Optimizing Binary Classifiers*.
R package version 0.1.0. Available at: https://github.com/gimunBae/roclab
