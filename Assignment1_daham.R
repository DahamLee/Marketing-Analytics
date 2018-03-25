rm(list=ls())
# 1
wm.data = read.csv("/Users/daham/Desktop/Marketing Analysis/assignment1/Walmart_Data.csv", header=T)
str(wm.data)
summary(wm.data)
wm.data$logSales = log(wm.data$Sales)
#wm.data$logSales


# 2
# Rearrange the variables with "Sales", "Feature", "Promotion"
wm.data.rearrange <- data.frame("Sales"=wm.data$Sales, "Promotion"=wm.data$Promotion, "Feature"=wm.data$Feature)
cor(x=wm.data.rearrange)
# Scatter plots for "'Sales'&'Promotion'" and "'Sales'&'Features'"
plot(wm.data$Sales,wm.data$Promotion, col="blue")
plot(wm.data$Sales,wm.data$Features, col="red")

# Histograms for 'Sales' and 'logSales'
hist(wm.data$Sales)
hist(wm.data$logSales)

# 3
# Multivariable Regression and Summary
wm.data$Walmart = as.factor(wm.data$Walmart)
wm.data$Holiday = as.factor(wm.data$Holiday)
wm.data$Walmart[1]
wm.lm = lm(logSales~Promotion+Feature+Walmart+Holiday, data=wm.data)
summary(wm.lm)

# 4
# OutlierTest
install.packages("car")
library("car")
outlierTest(wm.lm)

# If use plot with Linear Model, they showed 4 plots.
plot(wm.lm)

# plot(logSales~Promotion, data=wm.data)
# plot(logSales~Feature, data=wm.data)
# abline(da, col='green')
hist(wm.lm$residuals)
qqnorm(wm.lm$residuals)
qqline(wm.lm$residuals)
# fitted_value <- data.frame("Fitted value"=fitted(wm.lm))
# fitted_value

# wm.lm$residuals

# plot(fitted(wm.lm), wm.lm$residuals)
# abline(wm.lm, col='green')

# 5
wm.lm2 = lm(logSales~Promotion+Feature+Walmart+Holiday+Holiday:Walmart+Holiday*Promotion, data=wm.data)
summary(wm.lm2)

AIC(wm.lm)
BIC(wm.lm)

AIC(wm.lm2)
BIC(wm.lm2)

wm.lm3 =step(wm.lm2, direction="backward")
