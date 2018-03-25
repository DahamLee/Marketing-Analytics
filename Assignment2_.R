rm(list=ls())
library("Matrix")
library("lme4")
library("MCMCpack")

cc.data = read.csv("/Users/daham/Desktop/Marketing Analysis/assignment2/CreditCard_SOW_Data.csv", header=T)

cc.data$ConsumerID = as.factor(cc.data$ConsumerID)
cc.data$logIncome = log(cc.data$Income)
cc.data$logSowRatio = log(cc.data$WalletShare/(1-cc.data$WalletShare))


cc.re1 = MCMCregress(logSowRatio~History+Balance+Promotion+History:Promotion+logIncome:Promotion, mcmc=6000, data=cc.data)
summary(cc.re1)

#cc.re1[1,1]
#cc.re1[1]

# Plot posterior simulation
head(cc.re1)
plot(cc.re1[,"Promotion"], type="l")
plot(cc.re1[,"Promotion:logIncome"], type="l")

# quantile(cc.re1[, "Promotion"], prob=c(0.025, 0.975))

# auto-correlation
autocorr.plot(cc.re1[,c("Promotion","Promotion:logIncome")])
#autocorr.plot(cc.re1[,"Promotion:logIncome"])

# MCMC HLM
cc.bayeshlm = MCMChregress(fixed=logSowRatio~History+Balance+Promotion+History:Promotion+logIncome:Promotion, random=~Promotion, group="ConsumerID", data=cc.data, r=2, R=diag(2), mcmc=6000)

summary(cc.bayeshlm$mcmc[,1:6])

cc.bayeshlm$mcmc
head(cc.bayeshlm)
plot(cc.bayeshlm$mcmc[,"beta.History"], type="l")
plot(cc.bayeshlm$mcmc[,"beta.Promotion:logIncome"], type="l")

autocorr.plot(cc.bayeshlm$mcmc[,c("beta.History","beta.Promotion:logIncome")])
#autocorr.plot(cc.bayeshlm$mcmc[,"beta.Promotion:logIncome"])

# 3. GLM considering Random Effect
brd.data = read.csv("/Users/daham/Desktop/Marketing Analysis/assignment2/Bank_Retention_Data.csv", header=T)
brd.data$TractID = as.factor(brd.data$TractID)

brd.glm = glm(Churn~Age+Income+HomeVal+Tenure+DirectDeposit+Loan+Dist+MktShare, data=brd.data, family=binomial(link="logit"))
summary(brd.glm)

brd.glmer = glmer(Churn~Age+Income+HomeVal+Tenure+DirectDeposit+Loan+Dist+MktShare+(1|TractID), data=brd.data, family=binomial, glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(brd.glmer)

AIC(brd.glm)
BIC(brd.glm)

AIC(brd.glmer)
BIC(brd.glmer)