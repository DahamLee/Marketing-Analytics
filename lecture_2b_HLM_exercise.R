rp.data = read.csv("/Users/daham/Desktop/Marketing Analysis/Repeated_purchase_HLM.csv", header=T)
rp.data$ConsumerID = as.factor(rp.data$ConsumerID)
by(rp.data$Purchase, rp.data$ConsumerID, mean)

# simple linear model
rp.lm1 = lm(log(Purchase)~Age+log(Price)+Age:log(Price)+Feature, data=rp.data)
summary(rp.lm1)
AIC(rp.lm1)

# Hierachy Linear Model 
# ( X X X | Y Y) => Random effects 
# ( X X : Y Y) => 곱해진 것
# 예를 들어 logPurchase = beta0 + beta1*logPrice + beta2*Feature + epsilon
# 에서 beta0 = mu0 + mu1*Age + zeta
# 와 beta1 = gamma0 + gamma1*Age + Xi 을 통해서
# logPurchase = (mu0+zeta) + mu1*Age + (gamma0+Xi)*logPrice + gamma1*Age*logPrice + beta2*Feature + epsilon
# Random effects: contain zeta, Xi, epsilon
# 그래서 ( X X X | Y Y) 식에 intercept (1)와 logPrice 를 넣은 것임.

rp.data$logPurchase = log(rp.data$Purchase)
rp.data$logPrice = log(rp.data$Price)
rp.re1 = lmer(logPurchase~Age+logPrice+Feature+Age:logPrice+(1+logPrice|ConsumerID), data=rp.data, REML=F, control=lmerControl(optimizer ="Nelder_Mead"))
summary(rp.re1)

fixef(rp.re1)
head(ranef(rp.re1)$ConsumerID, 5)
head(coef(rp.re1)$ConsumerID,5)

# fixed + randeff = coeff

hist(coef(rp.re1)$ConsumerID[,1], main="Intercept")
hist(coef(rp.re1)$ConsumerID[,3], main="Price Sensitivity")
