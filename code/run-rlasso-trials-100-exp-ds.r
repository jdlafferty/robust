

source("robust-lasso.r")

out = list()
n = 100
p = floor(exp((1.2)^(0:10)))

for (i in 1:length(p)) {
  cat(sprintf("%s: n=%d, p=%d\n", date(), n, p[i]))
  out[[i]] = run.robust.lasso.ds(n=n, p=p[i], s=1, lambda.const=0.4, trials=10, delta=0.025, Q.mean=10)
  plot.error(out[[i]], add=(i>1))
}

for (i in 1:length(out)) { plot.error(out[[i]], add=(i>1))}

pdf('robust-lasso-n-100-exp-ds.pdf')
for (i in 1:length(out)) { plot.error(out[[i]], add=(i>1))}
dev.off()
save(n, p, out, file="robust-lasso-n-100-exp-ds.Rdata")
