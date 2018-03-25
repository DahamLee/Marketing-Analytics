rm(list=ls())
library("MCMCpack")
library("Matrix")
library("lme4")

# Using Classical HLM Fitting
rp.data = read.csv("/Users/daham/Desktop/Marketing Analysis/Repeated_purchase_HLM.csv", header=T)
rp.data$ConsumerID = as.factor(rp.data$ConsumerID)
rp.data$logPurchase = log(rp.data$Purchase)
rp.data$logPrice = log(rp.data$Price)
rp.re1 = lmer(logPurchase~Age+logPrice+Feature+Age:logPrice+(1+logPrice|ConsumerID), data=rp.data, REML=F, control=lmerControl(optimizer ="Nelder_Mead"))
summary(rp.re1)

# check fixed effect and random effect
fixef(rp.re1)
head(ranef(rp.re1)$ConsumerID, 5)
head(coef(rp.re1)$ConsumerID,5)

hist(coef(rp.re1)$ConsumerID[,1])
hist(coef(rp.re1)$ConsumerID[,3])

# Is HLM really better than a simple regression?
# Simple LM
rp.lm1 = lm(log(Purchase)~Age+log(Price)+Age:log(Price)+Feature, data=rp.data)
summary(rp.lm1)
# Simple Linear Model (AIC, BIC)
AIC(rp.lm1)
BIC(rp.lm1)

# HLM (AIC, BIC)
AIC(rp.re1)
BIC(rp.re1)

### Simulate Posteriors in HLM

# Simple linear model with MCMC
rp.ba1 = MCMCregress(logPurchase~Age+logPrice+Feature+Age:logPrice, mcmc=6000, data=rp.data)
summary(rp.ba1)

# HLM with MCMC
rp.ba2 = MCMChregress(fixed=logPurchase~Age+logPrice+Feature+Age:logPrice, random=~logPrice, group="ConsumerID", data=rp.data, r=2, R=diag(2))

# results of fixed effects
summary(rp.ba1)
summary(rp.ba1[,1:5])
head(rp.ba1)
rp.ba1[c(6:10,206:210)]
rp.ba1[406:411]

