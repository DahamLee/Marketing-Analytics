library("Matrix")
library("lme4")

apc.data = read.csv("/Users/daham/Desktop/Marketing Analysis/AmusementPark_conjoint_data.csv", header=T)
apc.data$speed = as.factor(apc.data$speed)
apc.data$height = as.factor(apc.data$height)
apc.data$resp.id = as.factor(apc.data$resp.id)
summary(apc.data)

by(apc.data$rating, apc.data$height, mean)

# LM with only fixed effects
ride.lm = lm(rating ~ speed + height + const + theme, data=apc.data)
summary(ride.lm)

# LM with random efect only for the intercept
ride.re1 <- lmer(rating ~ speed + height + const + theme + (1|resp.id), data=apc.data)
summary(ride.re1)

# Fixed effects
fixef(ride.re1)

# Random effects
head(ranef(ride.re1)$resp.id, 10)

# Individual-level partworths
head(coef(ride.re1)$resp.id, 10)

# The complete hierachial model (all coefficients are Random Effect)
ride.re2 = lmer(rating ~ speed + height + const + theme + (speed + height + const + theme|resp.id), data=apc.data, control=lmerControl(optCtrl=list(maxfun=100000)))

# Results
fixef(ride.re2)
head(ranef(ride.re2)$resp.id, 5)
head(coef(ride.re2)$resp.id,5)

AIC(ride.re1)
AIC(ride.re2)
