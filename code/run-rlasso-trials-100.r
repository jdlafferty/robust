

source("robust-lasso.r")

out = list()
n = 100
p = round(2^((0:7)*1.21))

for (i in 1:length(p)) {
  cat(sprintf("%s: n=%d, p=%d\n", date(), n, p[i]))
  out[[i]] = run.robust.lasso(n, p[i], 1, lambda.const=.5, trials=750, delta=0.025, Q.mean=20)
  plot.error(out[[i]], add=(i>1))
}

for (i in 1:length(out)) { plot.error(out[[i]], add=(i>1))}

pdf('robust-lasso-n=100.pdf')
for (i in 1:length(out)) { plot.error(out[[i]], add=(i>1))}
dev.off()
save(n, p, out, file="robust-lasso-n=100.Rdata")
