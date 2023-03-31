library(ggplot2)

load("data/robust-lasso-p-10-s-5-vary-n.Rdata")
# = p[1:length(p)-1]

df = list()
ns = c()
K = 0
for (i in 1:length(n)) {
   cat(sprintf("n=%d\n", n[i]))
   error = colMeans(out[[i]]$error)
   K = K+1
   df[[K]] = data.frame(error=error, epsilon=out[[i]]$epsilon)
   ns = c(ns, n[i])
}

dat = c()
sz = c()
for (k in 1:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

e0=.70

dat$n = rep(factor(ns[1:K]),times=sz)
g = ggplot(dat, aes(epsilon, error, color=n, linetype=n)) + geom_line(size=0.8) + xlab('epsilon') + ylab('squared error') + scale_y_continuous(limits=c(0,15)) + theme(legend.key.width=unit(1,"cm"))
print(g)

pdf("fig2.pdf", width=6, height=4)
print(g)
dev.off()
