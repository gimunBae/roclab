#' utils-internal.R - Internal utilities for ROC-SVM
#'
#' These helper functions are used only inside the package (not exported).
#' They handle pairwise difference construction, Adamax-based optimization,
#' proximal updates for penalties, intercept estimation, and lambda max.
#'
#' @keywords internal
# Build all pairwise differences X.plus[i,] - X.minus[j,]
X.func.complete <- function(X.plus, X.minus) {
  if (!is.matrix(X.plus) || !is.matrix(X.minus))
    stop("'X.plus' and 'X.minus' must be matrices.", call. = FALSE)
  if (ncol(X.plus) != ncol(X.minus))
    stop("ncol(X.plus) must equal ncol(X.minus).", call. = FALSE)

  np <- nrow(X.plus); nm <- nrow(X.minus)
  if (np == 0L || nm == 0L)
    stop("'X.plus' and 'X.minus' must have at least one row each.", call. = FALSE)

  i.idx <- rep(seq_len(np), each = nm)
  j.idx <- rep(seq_len(nm), times = np)

  X.plus[i.idx, , drop = FALSE] - X.minus[j.idx, , drop = FALSE]
}

# Build subset of pairwise differences given index pairs (i,j)
X.func.incomplete <- function(X.plus, X.minus, sub.indices) {
  if (!is.matrix(X.plus) || !is.matrix(X.minus))
    stop("'X.plus' and 'X.minus' must be matrices.", call. = FALSE)
  if (ncol(X.plus) != ncol(X.minus))
    stop("ncol(X.plus) must equal ncol(X.minus).", call. = FALSE)
  if (!is.matrix(sub.indices) || ncol(sub.indices) != 2L)
    stop("'sub.indices' must be an integer matrix with 2 columns (i, j).", call. = FALSE)

  np <- nrow(X.plus); nm <- nrow(X.minus)
  if (np == 0L || nm == 0L)
    stop("'X.plus' and 'X.minus' must have at least one row each.", call. = FALSE)

  i.idx <- sub.indices[, 1]
  j.idx <- sub.indices[, 2]
  if (any(i.idx < 1L | i.idx > np) || any(j.idx < 1L | j.idx > nm))
    stop("'sub.indices' out of bounds.", call. = FALSE)

  X.plus[i.idx, , drop = FALSE] - X.minus[j.idx, , drop = FALSE]
}
# Gradient descent solver (Adamax) for linear ROC-SVM
# Returns beta coefficients, convergence flag, and iteration count.
GD.linear <- function(X.ij, lambda, penalty, param.penalty, loss, maxiter, eps) {
  p <- ncol(X.ij)
  beta.init <- rep(0, p)
  if (penalty == "ridge") {
    update <- adamax.update(X.ij = X.ij, lambda = lambda, loss = loss, beta.init = beta.init, maxiter = maxiter, eps = eps)
  } else if (penalty=="lasso"){
    update <- adamax.update.pr(X.ij = X.ij, lambda = lambda, param.penalty = 1,
                                 loss = loss, beta.init = beta.init, maxiter = maxiter, eps = eps)
  } else if (penalty=="elastic"){
    update <- adamax.update.pr(X.ij = X.ij, lambda = lambda, param.penalty = param.penalty,
                                 loss = loss, beta.init = beta.init, maxiter = maxiter, eps = eps)
  } else if (penalty == "alasso") {
    beta.init.ad.lss <- adamax.update(X.ij = X.ij, lambda = lambda, loss = loss, beta.init = beta.init, maxiter = maxiter, eps = 10^(-3))$beta.hat
    if (any(is.na(beta.init.ad.lss)) ||  any(abs(beta.init.ad.lss) < 1e-8))    beta.init.ad.lss <- adamax.update(X.ij = X.ij, lambda = 0, loss = loss, beta.init = beta.init, maxiter = maxiter, eps = 10^(-3))$beta.hat
    if (any(is.na(beta.init.ad.lss)) || any(abs(beta.init.ad.lss) < 1e-8)) {
      warning("alasso initialization failed: OLS and Ridge estimates did not converge or contain zero coefficients. A standard lasso penalty will be applied instead.")
      beta.init.ad.lss <- rep(1, p)
    }
    weight <- 1 / (abs(beta.init.ad.lss)^(param.penalty) + 1e-8)
    update <- adamax.update.pr(X.ij = X.ij, lambda = lambda * weight,  param.penalty = 1,
                                 loss = loss, beta.init = beta.init, maxiter = maxiter, eps = eps)
  } else if (penalty %in% c("scad", "mcp")) {
    beta.init.lss <- adamax.update.pr(X.ij = X.ij, lambda = lambda, param.penalty = 1,
                                      loss = loss, beta.init = beta.init, maxiter = maxiter, eps = 10^(-3))$beta.hat
    if (any(is.na(beta.init.lss))) {
      beta.init.lss <-adamax.update(X.ij = X.ij, lambda = 0, loss = loss, beta.init = beta.init, maxiter = maxiter, eps = 10^(-3))$beta.hat
    }
    if (any(is.na(beta.init.lss))) {
      beta.init.lss <- rep(0, p)
    }
    update <- adamax.update.pr.nonconvex(X.ij = X.ij, lambda = lambda,
                                           penalty = penalty,param.penalty = param.penalty, loss = loss, beta.init = beta.init.lss,
                                           maxiter = maxiter, eps = eps)
  }
  beta.hat <- update$beta.hat
  iter <- update$iter
  return(list(beta.hat = beta.hat, converged = iter < maxiter, n.iter = iter))
}
# Adamax update (ridge-type penalties)
adamax.update <- function(X.ij, lambda,loss, beta.init, maxiter, eps) {
  alpha0 = 0.002
  beta1 = 0.9
  beta2 = 0.999
  m <- u <- 0
  iter <- 0
  p <- ncol(X.ij)
  beta <- beta.init
  while (iter < maxiter) {
    iter <- iter + 1
    beta.X.ij <- as.vector(X.ij %*% beta)
    if (loss == "hinge") {
      g <- lambda * beta - colMeans(X.ij * as.vector(1 - beta.X.ij > 0))
    } else if (loss == "logistic") {
      g <- lambda * beta - colMeans(X.ij * plogis(-beta.X.ij))
    } else if (loss == "exponential") {
      g <- lambda * beta - colMeans(X.ij * exp(-beta.X.ij))
    } else if (loss == "hinge2") {
      g <- lambda * beta - colMeans(2 * X.ij * (1 - beta.X.ij) * as.vector(1 - beta.X.ij > 0))
    } else {
      stop("Unsupported type of loss.")
    }
    m <- beta1 * m + (1 - beta1) * g
    u <- max(beta2 * u, max(abs(g)))
    new.beta <- beta - alpha0 * m / ((1 - beta1^iter) * u)
    if (max(abs(new.beta - beta)) < eps) break
    beta <- new.beta
  }
  if (iter >= maxiter) {
    warning("Algorithm did not converge.")
    new.beta <- rep(NA_real_, p)
  }
  return(list(beta.hat = new.beta,iter=iter))
}
# Adamax + proximal update (lasso/elastic/alasso)
adamax.update.pr <- function(X.ij, lambda, param.penalty, loss, beta.init,
                             maxiter, eps) {
  alpha0 = 0.002
  beta1 = 0.9
  beta2 = 0.999
  m <- u <- 0
  iter <- 0
  p <- ncol(X.ij)
  if (any(is.na(beta.init))) stop("beta.init contains NA")
  beta <- beta.init
  while (iter < maxiter) {
    iter <- iter + 1
    beta.X.ij <- as.vector(X.ij %*% beta)
    if (loss == "hinge") {
      g <- lambda * (1 - param.penalty) * beta - colMeans(X.ij * as.vector(1 - beta.X.ij > 0))
    } else if (loss == "logistic") {
      g <- lambda * (1 - param.penalty) * beta - colMeans(X.ij * plogis(-beta.X.ij))
    } else if (loss == "exponential") {
      g <- lambda * (1 - param.penalty) * beta - colMeans(X.ij * exp(-beta.X.ij))
    } else if (loss == "hinge2") {
      g <- lambda * (1 - param.penalty) * beta - colMeans(2 * (1 - beta.X.ij) * X.ij * as.vector(1 - beta.X.ij > 0))
    }
    m <- beta1 * m + (1 - beta1) * g
    u <- max(beta2 * u, max(abs(g)))
    eta.m <- alpha0 / ((1 - beta1^iter) * u)
    v <- beta - eta.m * m
    new.beta <- prox(v, lambda * param.penalty, eta.m)
    if (iter > 1) {
      if (max(abs(new.beta - beta)) < eps) break
    }
    beta <- new.beta
  }
  if (iter >= maxiter) {
    warning("Algorithm did not converge.")
    new.beta <- rep(NA_real_, p)
  }
  return(list(beta.hat = new.beta,iter=iter))
}
# Adamax + proximal update for nonconvex penalties (SCAD, MCP)
adamax.update.pr.nonconvex <- function(X.ij, lambda, penalty,param.penalty, loss, beta.init,
                                       maxiter, eps) {
  alpha0 = 0.002
  beta1 = 0.9
  beta2 = 0.999
  m <- u <- 0
  iter <- 0
  p <- ncol(X.ij)
  if (any(is.na(beta.init))) stop("beta.init contains NA")
  beta <- beta.init
  while (iter < maxiter) {
    iter <- iter + 1
    beta.X.ij <- as.vector(X.ij %*% beta)
    if (loss == "hinge") {
      g <- -colMeans(X.ij * as.vector(1 - beta.X.ij > 0))
    } else if (loss == "logistic") {
      g <- - colMeans(X.ij * plogis(-beta.X.ij))
    } else if (loss == "exponential") {
      g <- - colMeans(X.ij * exp(-beta.X.ij))
    } else if (loss == "hinge2") {
      g <- -colMeans(2 * (1 - beta.X.ij) * X.ij * as.vector(1 - beta.X.ij > 0))
    }
    m <- beta1 * m + (1 - beta1) * g
    u <- max(beta2 * u, max(abs(g)))
    eta.m <- alpha0 / ((1 - beta1^iter) * u)
    v <- beta - eta.m * m
    if (penalty == "scad") {
      new.beta <- prox.scad.scalar(v, lambda, param.penalty, eta.m)
    } else if (penalty == "mcp") {
      new.beta <- prox.mcp.scalar(v, lambda, param.penalty, eta.m)
    }
    if (iter > 1) {
      if (max(abs(new.beta - beta)) < eps) break
    }
    beta <- new.beta
  }
  if (iter >= maxiter) {
    warning("Algorithm did not converge.")
    new.beta <- rep(NA_real_, p)
  }
  return(list(beta.hat = new.beta,iter=iter))
}
# Estimate intercept b so that sensitivity/specificity ≈ targets
find.intercept.roclearn <- function(X, y, beta.hat, target.sens, target.spec) {
  if (!is.matrix(X)) stop("'X' must be a matrix.", call. = FALSE)
  if (!is.numeric(y) || length(y) != nrow(X))
    stop("'y' must be a numeric vector of length nrow(X).", call. = FALSE)
  if (!is.numeric(beta.hat) || length(beta.hat) != ncol(X))
    stop("'beta.hat' must be a numeric vector of length ncol(X).", call. = FALSE)

  n <- length(y)
  grid.points <- min(n, round(sqrt(n) * 20))

  scores <- as.numeric(X %*% beta.hat)
  sorted.scores <- sort(scores)

  # avoid extreme endpoints
  b.seq <- seq(-sorted.scores[length(scores) - 1L],
               -sorted.scores[2L],
               length.out = grid.points)

  best.b <- NA_real_
  best.diff <- Inf

  for (b in b.seq) {
    preds <- ifelse(scores + b >= 0, 1, -1)

    TP <- sum(preds == 1 & y == 1)
    TN <- sum(preds == -1 & y == -1)
    FP <- sum(preds == 1 & y == -1)
    FN <- sum(preds == -1 & y == 1)

    sens <- if ((TP + FN) > 0) TP / (TP + FN) else 0
    spec <- if ((TN + FP) > 0) TN / (TN + FP) else 0

    diff <- abs(sens - target.sens) + abs(spec - target.spec)

    if (diff < best.diff) {
      best.diff <- diff
      best.b <- b
    }
  }
  return(best.b)
}
# Soft-thresholding proximal operator
prox <- function(v, lambda, eta) {
  return(sign(v) * pmax((abs(v) - lambda * eta), 0))
}
# SCAD proximal operator (elementwise)
prox.scad.scalar <- function(v, lambda, param.penalty, eta) {
  if (!is.finite(param.penalty) || param.penalty <= 2)
    stop("'param.penalty' (alpha) for SCAD must be > 2.", call. = FALSE)
  idx1 <- abs(v) < 2 * lambda * eta
  idx2 <- (2 * lambda * eta <= abs(v)) & (abs(v) <= param.penalty * lambda * eta)

  v[idx1] <- prox(v[idx1], lambda, eta)
  v[idx2] <- ((param.penalty - 1) * v[idx2] - sign(v[idx2]) * param.penalty * lambda * eta) / (param.penalty - 2)

  return(v)
}
# MCP proximal operator (elementwise)
prox.mcp.scalar <- function(v, lambda, param.penalty, eta) {
  if (!is.finite(param.penalty) || param.penalty <= 1)
    stop("'param.penalty' (gamma) for MCP must be > 1.", call. = FALSE)
  idx1 <- abs(v) <= param.penalty * lambda
  v[idx1] <- prox(v[idx1], lambda, eta) / (1 - 1 / param.penalty)
  return(v)
}
# Compute maximum lambda (lambda.max) based on data and loss
# Used to generate lambda sequence in cv.rocsvm().
compute.lambda.max <- function(X, y, penalty, param.penalty, loss,
                               approx, maxiter, eps) {
  # Split into positive and negative classes
  n <- nrow(X)
  p <- ncol(X)
  plus.idx <- which(y > 0)
  np <- length(plus.idx)
  nm <- n - np
  X.plus <- X[plus.idx, , drop = FALSE]
  X.minus <- X[-plus.idx, , drop = FALSE]
  B <- n
  total.pairs <- np * nm

  if (approx) {
    B.use <- min(B, total.pairs)
    sub.idx <- sample.int(total.pairs, size = B.use, replace = TRUE)
    i.idx <- (sub.idx - 1L) %/% nm + 1L
    j.idx <- sub.idx %% nm; j.idx[j.idx == 0L] <- nm
    sub.indices <- cbind(i.idx, j.idx)
    X.ij <- X.func.incomplete(X.plus = X.plus, X.minus = X.minus, sub.indices = sub.indices)
  } else {
    X.ij <- X.func.complete(X.plus = X.plus, X.minus = X.minus)
  }
  # Compute mean difference
  pairwise.mean.diff <- colMeans(X.ij)
  # Base lambda.max depending on loss
  if (loss %in% c("hinge", "exponential")) {
    lambda.max <- max(abs(pairwise.mean.diff))
  } else if (loss == "logistic") {
    lambda.max <- max(abs(pairwise.mean.diff)) / 2
  } else if (loss == "hinge2") {
    lambda.max <- 2 * max(abs(pairwise.mean.diff))
  }
  if (penalty=="elastic") lambda.max <- lambda.max / param.penalty
  # alasso adjustment
  if (penalty == "alasso") {
    beta.init <- adamax.update(
      X.ij = X.ij, lambda = lambda.max * 1e-3,
      loss = loss, beta.init = rep(0, p), maxiter = maxiter, eps = 1e-3
    )$beta.hat
    # Retry with lambda=0 if initialization failed
    if ((length(beta.init) == 1 && is.na(beta.init)) || any(abs(beta.init) < 1e-8)) {
      beta.init <- adamax.update(
        X.ij = X.ij, lambda = 0,
        loss = loss, beta.init = rep(0, p), maxiter = maxiter, eps = 1e-3
      )$beta.hat
    }
    if ((length(beta.init) == 1 && is.na(beta.init)) || any(abs(beta.init) < 1e-8)) {
      stop("alasso initialization failed: estimates did not converge or contain zeros. Try a different penalty.")
    }
    weight <- 1 / (abs(beta.init)^param.penalty + 1e-8)
    lambda.max <- max(abs(pairwise.mean.diff / weight))
  }
  lambda.max <- 0.15 * lambda.max
  return(lambda.max)
}
K.func.complete <- function(plus.matrix, minus.matrix) {
  np <- nrow(plus.matrix)
  nm <- nrow(minus.matrix)
  i.idx <- rep(seq_len(np), each = nm)
  j.idx <- rep(seq_len(nm), times = np)
  K.ij <- plus.matrix[i.idx,,drop=FALSE] - minus.matrix[j.idx,,drop=FALSE]
  return(K.ij)
}
Phi.func.incomplete <- function(plus.matrix, minus.matrix, sub.indices) {
  i.idx <- sub.indices[,1]
  j.idx <- sub.indices[,2]
  Phi.ij <- plus.matrix[i.idx,,drop=FALSE] - minus.matrix[j.idx,,drop=FALSE]
  return(Phi.ij)
}
find.intercept.kroclearn <- function(feature.matrix,y,theta.hat,target.sens,target.spec) {
  n <- length(y)
  grid.points <- min(n,sqrt(n)*20)
  scores <- as.numeric(feature.matrix %*% theta.hat)
  sorted.scores <-  sort(scores)
  # avoid extreme endpoints
  b.seq <- seq(-sorted.scores[length(scores) - 1L],
               -sorted.scores[2L],
               length.out = grid.points)
  best.b <- NA_real_
  best.diff <- Inf
  for (b in b.seq) {
    preds <- ifelse(scores + b >= 0, 1, -1)
    TP <- sum(preds == 1 & y == 1)
    TN <- sum(preds == -1 & y == -1)
    FP <- sum(preds == 1 & y == -1)
    FN <- sum(preds == -1 & y == 1)
    sens <- if ((TP + FN) > 0) TP / (TP + FN) else 0
    spec <- if ((TN + FP) > 0) TN / (TN + FP) else 0
    diff <- abs(sens - target.sens) + abs(spec - target.spec)
    if (diff < best.diff) {
      best.diff <- diff
      best.b <- b
    }
  }
  return(best.b)
}
stratified.sample.exact <- function(y, d) {
  idx <- caret::createDataPartition(y, p = d / length(y), list = FALSE)
  current.n <- length(idx)

  if (current.n < d) {
    remain <- setdiff(seq_along(y), idx)
    extra <- sample(remain, d - current.n)
    idx <- c(idx, extra)
  }
  if (current.n > d) {
    idx <- sample(idx, d, replace = FALSE)
  }
  return(idx)
}
nystrom.approx <- function(X, y, d, kfunc) {
  idx.nystrom <- stratified.sample.exact(y, d)
  landmarks <- X[idx.nystrom, , drop = FALSE]

  W <- kernlab::kernelMatrix(kfunc, landmarks, landmarks)
  W <- (W + t(W)) / 2

  C <- kernlab::kernelMatrix(kfunc, X, landmarks)

  W.inv <- pracma::pinv(W + diag(1e-6, d))
  W.inv <- (W.inv + t(W.inv)) / 2

  eig <- eigen(W.inv, symmetric = TRUE)
  Q <- eig$vectors
  Lambda <- eig$values
  Lambda[Lambda < 0] <- 0
  W.inv.sqrt <- Q %*% diag(sqrt(Lambda)) %*% t(Q)

  phi <- C %*% W.inv.sqrt
  return(list(phi = phi,
              landmarks = landmarks,
              W.inv.sqrt = W.inv.sqrt))
}
GD.nonlinear <- function(K0,K.ij,lambda,loss,theta.init,maxiter,eps) {
  iter <- 0
  m <- u <- 0
  alpha0 <- 0.002
  beta1 <- 0.9
  beta2 <- 0.999
  p <- length(theta.init)
  n <- ncol(K0)
  theta <- theta.init
  while(iter < maxiter){
    iter <- iter+1
    K.ij.theta <- as.vector(K.ij %*% theta)
    if(loss=="hinge"){
      g <- lambda * K0 %*% theta - colMeans( K.ij * as.vector(1 - K.ij.theta > 0))
    }else if(loss=="logistic"){
      g <- lambda * K0 %*% theta - colMeans(K.ij*plogis(-K.ij.theta))
    }else if(loss=="exponential"){
      g <- lambda * K0 %*% theta - colMeans(K.ij*exp(- K.ij.theta))
    }else if(loss=="hinge2"){
      g <- lambda * K0 %*% theta - colMeans(2 * K.ij* (1-K.ij.theta) * as.vector(1 - K.ij.theta > 0))
    } else{
      stop("Unsupported type of loss.")
    }
    m <- beta1 * m + (1 - beta1) * g
    u <- max(beta2 * u,max(abs(g)))
    new.theta <- theta - alpha0 * m / ((1-beta1**iter)*u)
    if (max(abs(new.theta - theta)) < eps) break
    theta <- new.theta
  }
  if(iter>=maxiter){
    warning("Algorithm did not converge.")
    new.theta <- rep(NA_real_, p)
  }
  return(list(theta.hat=new.theta,converged = iter < maxiter, n.iter = iter))
}


GD.nystrom <- function(Phi.ij,lambda,loss,theta.init,maxiter,eps) {
  iter <- 0
  m <- u <- 0
  alpha0 <- 0.002
  beta1 <- 0.9
  beta2 <- 0.999
  p <- length(theta.init)
  theta <- theta.init
  while(iter < maxiter){
    iter <- iter+1
    theta.Phi.ij <- as.vector(Phi.ij %*% theta)
    if(loss=="hinge"){
      g <- lambda * theta - colMeans( Phi.ij * as.vector(1 - theta.Phi.ij > 0))
    }else if(loss=="logistic"){
      g <- lambda * theta - colMeans(Phi.ij * plogis(-theta.Phi.ij))
    }else if(loss=="exponential"){
      g <- lambda * theta - colMeans(Phi.ij*exp(- theta.Phi.ij))
    }else if(loss=="hinge2"){
      g <- lambda * theta - colMeans(2 * Phi.ij* (1-theta.Phi.ij) * as.vector(1 - theta.Phi.ij > 0))
    } else{
      stop("Unsupported type of loss.")
    }
    m <- beta1 * m + (1 - beta1) * g
    u <- max(beta2 * u,max(abs(g)))
    new.theta <- theta - alpha0 * m / ((1-beta1**iter)*u)
    if (max(abs(new.theta - theta)) < eps) break
    theta <- new.theta
  }
  if(iter>=maxiter){
    warning("Algorithm did not converge.")
    new.theta <- rep(NA_real_, p)
  }
  return(list(theta.hat=new.theta,converged = iter < maxiter, n.iter = iter))
}
