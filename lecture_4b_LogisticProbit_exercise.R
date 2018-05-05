rm(list=ls())
bac= read.csv("/Users/daham/Desktop/Marketing Analysis/Bank_customer_acquisition.csv", header=T)
head(bac)
bac$Acquisition
# logit regression
bac.glm1 = glm(Acquisition~Age+Income+HomeVal+LoanInd+Dist2Bank+MktShare, data=bac, family=binomial(link="logit"))
summary(bac.glm1)
# probit regression
bac.glm2 = glm(Acquisition~Age+Income+HomeVal+LoanInd+Dist2Bank+MktShare, data=bac, family=binomial(link="probit"))

# logit regression AIC, BIC
AIC(bac.glm1)
BIC(bac.glm1)

# probit regression AIC, BIC
AIC(bac.glm2)
BIC(bac.glm2)

# logit regression predict
bac.pred1 = predict(bac.glm1, type="response")
cor(bac$Acquisition, bac.pred1)

# probit regression predict
bac.pred2 = predict(bac.glm2, type="response")
cor(bac$Acquisition, bac.pred2)
