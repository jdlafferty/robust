library(ggplot2)

load("../robust-lasso-tau2.Rdata")
# = p[1:length(p)-1]

df = list()
rhos = c()
K = 0
for (i in 1:length(rho)) {
   cat(sprintf("rho=%f\n", rho[i]))
   error = colMeans(out[[i]]$error)
   K = K+1
   df[[K]] = data.frame(error=error, epsilon=out[[i]]$epsilon)
   rhos = c(rhos, rho[i])
}

dat = c()
sz = c()
K = 7
for (k in 1:K) {
  dat = rbind(dat, df[[k]])
  sz = c(sz, nrow(df[[k]]))
}

e0=.70

taus = 2*rhos / (1+rhos*rhos)
corr = round(taus,3)
rhos = round(rhos,3)
dat$rho = rep(factor(rhos[1:K]),times=sz)
dat$correlation = rep(factor(corr[1:K]),times=sz)
g = ggplot(dat, aes(epsilon, error, color=correlation, linetype=correlation)) + geom_line(size=0.8) + xlab('epsilon') + ylab('squared error') + scale_y_continuous(limits=c(0,10)) + theme(legend.key.width=unit(1,"cm"))
print(g)

pdf("fig3.pdf", width=6, height=4)
print(g)
dev.off()
