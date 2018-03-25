rm(list=ls())
# Extra question
## 1
cc.data = read.csv("/Users/daham/Desktop/Marketing Analysis/assignment1/CreditCard_SOW_Data.csv", header=T)
str(cc.data)

# change 'ConsumerID' into factor
cc.data$ConsumerID = as.factor(cc.data$ConsumerID)

# Add logSoWRatio
cc.data$logSowRatio = log(cc.data$WalletShare/(1-cc.data$WalletShare))

## 2
# Regression (Simple)
cc.lm = lm(logSowRatio~History+Balance+Promotion+History:Promotion+log(Income):Promotion,data=cc.data)
summary(cc.lm)
# To see results
fitted(cc.lm)

## 3
# HLM Regression
cc.hlm = lmer(logSowRatio~Balance+History+Promotion+History:Promotion+log(Income):Promotion+(1+Promotion|ConsumerID), data=cc.data, REML=F, control=lmerControl(optimizer ="Nelder_Mead"))
summary(cc.hlm)
# To see results
fitted(cc.hlm)


## 4
AIC(cc.lm)
BIC(cc.lm)
AIC(cc.hlm)
BIC(cc.hlm)
