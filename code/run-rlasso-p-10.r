

source("robust-lasso.r")

out = list()
n = rev(round(2^(0:9)*20))
trials = c(rep(50,4), rep(500,3), rep(2000, 3))
p = 10
s = 5

for (i in 1:length(n)) {
  cat(sprintf("%s: n=%d, p=%d\n", date(), n[i], p))
  out[[i]] = run.robust.lasso(n=n[i], p=p, s=s, lambda.const=0.4, trials=trials[i], delta=0.025, Q.mean=10)
  plot.error(out[[i]], add=(i>1))
}

for (i in 1:length(out)) { plot.error(out[[i]], add=(i>1))}

pdf('robust-lasso-p-10-s-1-vary-n.pdf')
for (i in 1:length(out)) { plot.error(out[[i]], add=(i>1))}
dev.off()
save(n, p, out, file="robust-lasso-p-10-s-5-vary-n.Rdata")
