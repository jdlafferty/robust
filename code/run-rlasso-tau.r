

source("robust-lasso.r")

out = list()
n = 3000
trials = 750
p = 10
s = 10
rho = .75 - 1/2^{1:6}
rho = c(0, rho)
trials = c(50, 100, 100, 250, 500, 750, 750)
for (i in 1:length(rho)) {
  cat(sprintf("%s: n=%d, p=%d, rho=%f, \n", date(), n, p, rho[i]))
  out[[i]] = run.robust.lasso.tau(n=n, p=p, s=s, rho=rho[i], lambda.const=0.4, trials=trials[i], delta=0.025, Q.mean=10)
  plot.error(out[[i]], add=(i>1))
}

for (i in 1:length(out)) { plot.error(out[[i]], add=(i>1))}

pdf('robust-lasso-tau.pdf')
for (i in 1:length(out)) { plot.error(out[[i]], add=(i>1))}
dev.off()
save(n, p, rho, out, file="robust-lasso-tau2.Rdata")
