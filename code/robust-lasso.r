
library(quantreg)
library(mvtnorm)
library(Matrix)

run.robust.lasso = function(n, p, s, lambda.const=.5, trials = 100, delta = 0.025, Q.mean=5) {

  eps = seq(0, 1, by = delta)
  error = matrix(rep(0, trials * length(eps)), trials, length(eps))

  for (t in 1:trials) {

    X = matrix(rnorm(n * p), n, p)
    beta.star = c(sign(rnorm(s, mean=0))*rnorm(s, mean=2), rep(0, p-s))
    y.star = X %*% beta.star
    lambda = lambda.const*sqrt(n*log(2*p))
    X.tilde = rbind(X, diag(lambda,p))
    data = data.frame(X=X.tilde)

    for (i in 1:length(eps)) {
      corrupt = rbinom(n, 1, prob = eps[i])
      clean = rep(1, n) - corrupt
      Q = rnorm(n, mean=Q.mean)
      z = clean * rnorm(n, mean=0) + corrupt * Q
      y.tilde = rbind(y.star + z, matrix(rep(0,p), p, 1))
      data$y = y.tilde
      fit = rq(y ~ 0 + ., data = data, tau = 0.5, method = "fn")
      #fit = rq(y ~ 0 + ., data = data, tau = 0.5)
      beta.hat = as.vector(fit$coef)
      error[t, i] = sum((beta.hat - beta.star)^2)
    }

    if (t%%(trials/10) == 0) {
      cat(sprintf("trial=%d, %s\n", t, date()))
    }
  }
  return(list(error = error, epsilon = eps))
}

run.robust.lasso.tau = function(n, p, s, rho=0, lambda.const=.5, trials = 100, delta = 0.025, Q.mean=5) {

  eps = seq(0, 1, by = delta)
  error = matrix(rep(0, trials * length(eps)), trials, length(eps))
  p = p + p%%2

  for (t in 1:trials) {

    Z = matrix(rnorm(n * p), n, p)
    M = matrix(c(1,rho, rho, 1), 2, 2)
    Q = list()
    for (j in 1:(p/2)) {
      Q[[j]] = M
    }
    X = Z %*% bdiag(Q)
    X = as.matrix(X)
    beta.star = c(sign(rnorm(s, mean=0))*rnorm(s, mean=2), rep(0, p-s))
    y.star = X %*% beta.star
    lambda = lambda.const*sqrt(n*log(2*p))
    X.tilde = rbind(X, diag(lambda,p))
    data = data.frame(X=X.tilde)

    for (i in 1:length(eps)) {
      corrupt = rbinom(n, 1, prob = eps[i])
      clean = rep(1, n) - corrupt
      Q = rnorm(n, mean=Q.mean)
      z = clean * rnorm(n, mean=0) + corrupt * Q
      y.tilde = rbind(y.star + z, matrix(rep(0,p), p, 1))
      data$y = y.tilde
      fit = rq(y ~ 0 + ., data = data, tau = 0.5, method = "fn")
      #fit = rq(y ~ 0 + ., data = data, tau = 0.5)
      beta.hat = as.vector(fit$coef)
      error[t, i] = sum((beta.hat - beta.star)^2)
    }

    if (t%%(trials/10) == 0) {
      cat(sprintf("trial=%d, %s\n", t, date()))
    }
  }
  return(list(error = error, epsilon = eps))
}


plot.error = function(out, p, add=FALSE, title='') {
  if (add) {
    lines(out$epsilon, colMeans(out$error))
  } else {
    plot(out$epsilon, colMeans(out$error), "l", ylab = "l2 error",
      xlab = "epsilon", main=title)
  }
  points(out$epsilon, colMeans(out$error))
}
