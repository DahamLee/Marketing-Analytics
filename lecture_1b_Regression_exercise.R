rm(list=ls())
aps.data = read.csv("/Users/daham/Desktop/Marketing Analysis/AmusementPark_Data.csv", header=T)
summary(aps.data)
# correlation plots
pairs(aps.data)

# new variable
aps.data$logDist = log(aps.data$distance)

# 1. Simple Linear Regression
aps.lm1 = lm(overall~rides, data=aps.data)
plot(overall~rides, data=aps.data)
abline(aps.lm1, col='green')

summary(aps.lm1)
confint(aps.lm1)

# 2. Multiple linear regression
aps.lm2 = lm(overall~rides+games+wait+clean, data=aps.data)
summary(aps.lm2)

# regression 식에 각 variable 값들을 대입했을 떄,
fitted(aps.lm2)

plot(aps.data$overall, fitted(aps.lm1), col="green")
points(aps.data$overall, fitted(aps.lm2), col="blue")
legend("topleft", legend=c("model 1", "model 2"), col=c("green", "blue"), pch=1)

# 3. Multiple linear regression
aps.lm3 = lm(overall~rides+games+wait+clean+logDist+num.child+weekend, data=aps.data)
summary(aps.lm3)
points(aps.data$overall, fitted(aps.lm3), col="red")

# 4. Multiple linear regression
aps.data$has.child = (aps.data$num.child>0)*1
aps.lm4 = lm(overall~rides+games+wait+clean+logDist+has.child+weekend, data=aps.data)
summary(aps.lm4)
points(aps.data$overall, fitted(aps.lm4), col="purple")


# 5. Interaction Terms
aps.lm5 = lm(overall~rides+games+wait+clean+logDist+has.child+weekend+has.child:weekend, data=aps.data)
summary(aps.lm5)
points(aps.data$overall, fitted(aps.lm5), col="black")

AIC(aps.lm5)

# 6. Stepwise regression
aps.lm6 =step(aps.lm5, direction="backward")
summary(aps.lm6)

### Data example 2
# 1. single LM
ncaaf = read.csv("/Users/daham/Desktop/Marketing Analysis/NCAAF_Data.csv", header=T)
ncaaf.lm1 = lm(Comp~Conf+Age+Yrs+Win+Loss+Tie+WinPct, data=ncaaf)

# 2. Log LM
ncaaf$logComp = log(ncaaf$Comp)
ncaaf.lm2 = lm(logComp~Conf+Age+Yrs+Win+Loss+Tie+WinPct, data=ncaaf)

ncaaf$TopConf = (ncaaf$Conf == "ACC" | ncaaf$Conf=="Big 12" | ncaaf$Conf=="Big Ten" | ncaaf$Conf=="Pac 12" | ncaaf$Conf=="SEC")*1
ncaaf.lm3 = lm(logComp~TopConf+Age+Yrs+Win+Loss+Tie+WinPct, data=ncaaf)
summary(ncaaf.lm3)

hist(ncaaf.lm2$residuals)
qqnorm(ncaaf.lm2$residuals)
qqline(ncaaf.lm2$residuals)

AIC(ncaaf.lm2)
BIC(ncaaf.lm2)
AIC(ncaaf.lm3)
BIC(ncaaf.lm3)
