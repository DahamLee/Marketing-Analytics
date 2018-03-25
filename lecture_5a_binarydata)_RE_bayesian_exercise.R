rm(list=ls())
library("Matrix")
library("lme4")
library("MCMCpack")

sc.data = read.csv("/Users/daham/Desktop/Marketing Analysis/CreditCard_ServiceCall.csv", header=T)
sc.data$logIncome = log(sc.data$Income)
sc.glm1 = glm(ServiceCall~History+Balance+History:Balance+logIncome:Balance, data=sc.data, family=binomial(link="logit"))

# resacle
sc.data$Balance2 = sc.data$Balance/100

# GLM
sc.re1 = glmer(ServiceCall~History+logIncome+Balance2+logIncome:Balance2+History:Balance2+(1+Balance2|ConsumerID), data=sc.data, family=binomial, glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

# simple GLM과 Random Effect 고려한 GLM 비교
sc.glm2 = glm(ServiceCall~History+logIncome+Balance2+logIncome:Balance2+History:Balance2, data=sc.data, family=binomial(link="logit"))
summary(sc.glm2)
summary(sc.re1)

# Bayesian modeling fitting
sc.ba1 = MCMChlogit(fixed=ServiceCall~History+logIncome+Balance2+logIncome:Balance2+History:Balance2, random=~Balance2, group="ConsumerID", data=sc.data, mcmc=6000, r=2, R=diag(2))
summary(sc.ba1$mcmc[,1:6])

sc.ba2 = MCMCprobit(ServiceCall~History+logIncome+Balance2+logIncome:Balance2+History:Balance2, data=sc.data, mcmc=6000)
summary(sc.ba2[,1:6])
