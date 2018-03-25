rm(list=ls())
library("Matrix")
library("lme4")
library("MASS")

# Poisson
ml = read.csv("/Users/daham/Desktop/Marketing Analysis/Assignment3/Mall_visit.csv", header=T)
ml$Target = as.factor(ml$Target)
ml$Gender = as.factor(ml$Gender)
ml.glm1 = glm(Visit~Discount+Target+Income+Distant+Gender, data=ml, family=poisson)
summary(ml.glm1)
AIC(ml.glm1)
BIC(ml.glm1)

# Poisson Random Effect
ml.re1 = glmer(Visit~Discount+Target+Income+Distant+Gender+(1|customerID), data=ml, family=poisson)
summary(ml.re1)
AIC(ml.re1)
BIC(ml.re1)

# Negative Binomial
m1.nbre = glm.nb(Visit~Discount+Target+Income+Distant+Gender+(1|customerID), data=ml, link="log")
summary(m1.nbre)
AIC(m1.nbre)
BIC(m1.nbre)


hotel = read.csv("/Users/daham/Desktop/Marketing Analysis/Assignment3/HHonors_booking.csv", header=T)
interval = c( )
for(i in 1:400) {
  hotel.i = hotel[hotel$customerID==i,]
  interval.i = rep(0, 50)
  sinceBooking = 0
  for(t in 1:50) {
    sinceBooking = sinceBooking + 1
    interval.i[t] = sinceBooking
    if (hotel.i$Booking[t] == 1) sinceBooking = 0
  }
  interval = c(interval, interval.i)
}
hotel$Interval = interval       

# Recuurent Hazard Model with Linear Base line with logit
hotel.glm1 = glm(Booking~Interval+Price+Promotion+Income+Gender, data=hotel, family=binomial(link="logit"))
summary(hotel.glm1)
AIC(hotel.glm1)
BIC(hotel.glm1)

# Recurrent Hazard Model with Random Effect
hotel.glmer1 = glmer(Booking~Interval+Price+Promotion+Income+Gender+(1|customerID), data=hotel, family=binomial(link="logit"))
summary(hotel.glmer1)

AIC(hotel.glmer1)
BIC(hotel.glmer1)

# Recuurent Hazard Model with Linear Base line with cloglog
hotel.glm2 = glm(Booking~Interval+Price+Promotion+Income+Gender, data=hotel, family=binomial(link="cloglog"))
summary(hotel.glm2)

hotel.glmer2 = glmer(Booking~Interval+Price+Promotion+Income+Gender+(1|customerID), data=hotel, family=binomial(link="cloglog"))
summary(hotel.glmer2)

AIC(hotel.glmer2)
BIC(hotel.glmer2)
