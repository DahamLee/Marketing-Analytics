rm(list=ls())

library(mlogit)

mall_choice = read.csv("/Users/daham/Desktop/Marketing Analysis/Assignment5/Mall_choice_data.csv", header=T)
#head(mall_choice, 20)
#dim(mall_choice)
mall_choice
#head(mall_choice[4:9])

M.data = mlogit.data(mall_choice, shape="long", choice="choice", alt.levels=c("1","2","3","4","0") )
head(M.data, 20)
M.data

mall.m1 = mlogit(choice ~ discount + targeting + distance | income + gender , M.data, reflevel="0")
mall.m1

summary(mall.m1)
mall.fit = fitted(mall.m1, outcome=FALSE)
colMeans(mall.fit)

soda  = read.csv("/Users/daham/Desktop/Marketing Analysis/Assignment5/Soda_choice_data.csv", header=T)
soda
soda.ms = soda[soda$ProductID!=0,]
soda.ms
soda0 = soda$MarketShare[soda$ProductID==0]
soda0
soda0 = matrix(soda0, length(soda0), 11)
soda.ms$logMktShrRatio = log(soda.ms$MarketShare/as.vector(t(soda0)))

t(soda0)
as.vector(t(soda0))
head(soda.ms, 21)

results = lm(logMktShrRatio~Brand+Sugar+Caffeine+Promotion, data=soda.ms)
summary(results)
