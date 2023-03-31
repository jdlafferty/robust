library(ggplot2)

load("data/robust-lasso-n-100-exp.Rdata")
# = p[1:length(p)-1]

df = list()
ps = c()
K = 0
for (i in 1:length(p)) {
   cat(sprintf("p=%d\n", p[i]))
   error = colMeans(out[[i]]$error)
   K = K+1
   df[[K]] = data.frame(error=error, epsilon=out[[i]]$epsilon)
   ps = c(ps, p[i])
}

dat = c()
sz = c()
for (k in 1:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}


dat$p = rep(factor(ps[1:K]),times=sz)
g = ggplot(dat, aes(epsilon, error, color=p, linetype=p)) + geom_line(size=0.8) + xlab('epsilon') + ylab('squared error') + scale_y_continuous(limits=c(0,15)) + theme(legend.key.width=unit(1,"cm"))
print(g)

pdf("fig1.pdf", width=6, height=4)
print(g)
dev.off()
