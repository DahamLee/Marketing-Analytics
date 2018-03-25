rm(list=ls())
library("MCMCpack")
library("Matrix")
library("coda")

# 복습, simple LM
rp.ba1 = MCMCregress(logPurchase~Age+logPrice+Feature+Age:logPrice, mcmc=6000, data=rp.data)
summary(rp.ba1)

# Plot posterior simulation
head(rp.ba1)
plot(rp.ba1[,"(Intercept)"], type="l")
quantile(rp.ba1[, "(Intercept)"], prob=c(0.025, 0.975))

# auto-correlation
autocorr.plot(rp.ba1[,"Age"])

# effective sample size
effectiveSize(rp.ba1[,"sigma2"])
