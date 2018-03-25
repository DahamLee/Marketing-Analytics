rm(list=ls())
TravelMode= read.csv("/Users/daham/Desktop/Marketing Analysis/TravelMode_data.csv", header=T)

# BIC 할때 210으로 줘야한다. 관측개수가 840이 아니고 210이다.
# 모델에 관한 변수인지 individual 정보에 대한 변수 인지 먼저 확인해야한다.

#install.packages("mlogit")
#library(mlogit)
travel.logit = mlogit.data(TravelMode, shape="long", choice="choice", alt.levels=c("air", "train", "bus", "car"))

# BIC = AIC(travel.m1, k1=log(210)) 로 계산해야한다.
# BIC = AIC(travel.m1, k1=log(210))