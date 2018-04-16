library('maxLik')
library('miscTools')

library(sampleSelection)
data( "Mroz87" )
Mroz87$kids = ( Mroz87$kids5 + Mroz87$kids618 > 0 )

Mroz87.ml = selection( lfp ~ age + I(age^2) + faminc + kids + educ, wage ~ exper + I(exper^2 ) + educ + city, data = Mroz87, maxMethod = "BHHH", iterlim = 500 )
summary(Mroz87.ml)

install.packages("censReg")
library("censReg")
md.data = read.csv("/Users/daham/Desktop/Marketing Analysis/Mobile_data_usage.csv", header=T)
md.creg = censReg(DataUse~Quota+Day, data=md.data)
summary(md.creg)

md.lm = lm(DataUse~Quota+Day, data=md.data)
BIC(md.creg)
BIC(md.lm)

# Assignment 4
library("censReg")
cc = read.csv("/Users/daham/Desktop/Marketing Analysis/assignment4/CreditCard_SOW_data2.csv", header=T)
cc.creg = censReg(SOW~Promotion+Balance,left = 0, right = 1,data=cc)
#cc.creg = censReg(SOW~Promotion+Balance,data=cc)
summary(cc.creg)
