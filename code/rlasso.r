n = 50
p = 200
s = 5
X = matrix(rnorm(n * p), n, p)
beta.star = c(sign(rnorm(s, mean=0))*rnorm(s, mean = 2), rep(0, p-s))
y.star = X %*% beta.star
lambda = .1*sqrt(n*log(2*p))
X.tilde = rbind(X, diag(lambda,p))
data = data.frame(X=X.tilde)
eps = .3
corrupt = rbinom(n, 1, prob = eps)
clean = rep(1, n) - corrupt
Q = rnorm(n, mean = 5)
z = clean * rnorm(n, mean=0) + corrupt * Q
y.tilde = rbind(y.star + z, matrix(rep(0,p), p, 1))
data$y = y.tilde
fit = rq(y ~ 0 + ., data = data, tau = 0.5, method = "fn")
beta.hat = as.vector(fit$coef)
df = data.frame(beta.star=beta.star, beta.hat=beta.hat)
pdf('rlasso.pdf', width=10, height=5)
barplot(t(as.matrix(df[1:20,1:2])), beside=TRUE)
dev.off()
