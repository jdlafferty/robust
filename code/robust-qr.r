
library(quantreg)
library(mvtnorm)
library(Matrix)
library(ggplot2)

n=1000; tau=.50; eps=.75; Q.mean=10; fit.intercept=F;
set.seed(13)

p = 1
beta = c(0, 3)
eta = c(4, 1.5)
X = cbind(rep(1, n), sort(runif(n, min=0, max=5)-2.5))
#X = cbind(rep(1, n), sort(rnorm(n)))
#noise=rexp(n)-log(2)
noise = rnorm(n)
w = X%*%eta * noise
y = X %*% beta

corrupt = rbinom(n, 1, prob=eps)
clean = rep(1, n) - corrupt
Q = rnorm(n, mean=Q.mean, sd=1)
flip = rbinom(n, 1, prob=.5)
flip = 2*flip - rep(1,n)
y = y + clean*w + corrupt * Q

dat = data.frame(X=X[,2], y=y)
true.intercept = beta[1] + eta[1]*(qnorm(tau))
if (fit.intercept==T) {
   out = rq(y ~ X, data=dat, tau=tau)
   beta.hat = as.vector(out$coef)
} else {
  out = rq(I(y-true.intercept) ~ 0 + X, data=dat, tau=tau)
  beta.hat = c(true.intercept, as.numeric(out$coef))
}

plot(X[,2], y)
#lines(X[,2], X%*%beta + X%*%eta*(qnorm(tau)), lwd=2, col='red')
lines(X[,2], X%*%beta + X%*%eta*(qexp(tau)-log(2)), lwd=2, col='red')
lines(X[,2], X%*%beta.hat, lwd=2, col='blue')
cat(sprintf("true intercept: %f estimated intercept: %f\n", true.intercept, beta.hat[1]))
cat(sprintf("true slope: %f estimated slope: %f\n", beta[2]+eta[2]*(qexp(tau)-log(2)), beta.hat[2]))

#}

dat = data.frame(x=X[,2], y=y, yhat=X%*%beta.hat, ystar=X%*%beta)
g = ggplot(dat, aes(x, y)) + geom_point(size=.9, color='salmon') + geom_line(aes(x, yhat), size=.50) + geom_line(aes(x,ystar), linetype = 'dashed', color='black', size=.50)

pdf("../fig1a.pdf", width=6, height=4)
print(g)
dev.off()
