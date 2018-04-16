
# BIC 할때 210으로 줘야한다. 관측개수가 840이 아니고 210이다.
# 모델에 관한 변수인지 individual 정보에 대한 변수 인지 먼저 확인해야한다.

#install.packages("mlogit")

library(mlogit)
rm(list=ls())

TravelMode = read.csv("/Users/daham/Desktop/Marketing Analysis/TravelMode_data.csv", header=T)
TravelMode

# mlogit.data => data 가공
# alt.levels 은 선택지 인것 같다 choice내의 선택지
travel.logit = mlogit.data(TravelMode, shape="long", choice="choice", alt.levels=c("air", "train", "bus", "car"))
head(travel.logit)
# BIC = AIC(travel.m1, k1=log(210)) 로 계산해야한다.
# BIC = AIC(travel.m1, k1=log(210))

# A simple choice model for travel mode
travel.m1 = mlogit(choice ~ wait + vcost + travel, travel.logit, reflevel="bus")
travel.m1
summary(travel.m1)

# Another choice model for travel mode
travel.m2 = mlogit(choice ~ wait + vcost + travel|income, travel.logit, reflevel="bus")
summary(travel.m2)

#######################################################

# The data set "Heating" is about the choice of heating system in California house

# Five types of heating 
# gas central (gc), 
# gas room (gr), 
# electric central(ec), 
# electric room (er), 
# heat pump (hp)

# Idcase: id; Depvar: choice in {gc, gr, ec, er, hp}
# ic.alt: installation cost
# oc.alt: annual operating cost
# Income, agehed, rooms, region

# Fit the Model with "mlogit"
data("Heating", package = "mlogit")
head(Heating) #This dataset has the wide format
head(Heating[3:12])
# varing은 데이터 선택에 필요한 부분만 가져오는 듯
H.data = mlogit.data(Heating, shape="wide", choice="depvar", varying=c(3:12))
head(H.data)
summary(H.data)

# |0 이건 인터셉트를 없애고 출력하는 것을 나타내는 것 같다.
heating.m1 = mlogit(depvar~ic+oc|0, H.data, reflevel = 'hp')
summary(heating.m1)

# To see how the model fit the original data, let's look at the market shares
heating.fit1 = fitted(heating.m1, outcome=FALSE)
colMeans(heating.fit1)

# "Fit the Fixed Effect Model"
# The model with choice-specific intercepts
heating.m2 = mlogit(depvar~ic+oc, H.data, reflevel = 'hp')
summary (heating.m2)
heating.fit2 = fitted(heating.m2, outcome=FALSE)
colMeans(heating.fit2)

# A more complicated model
heating.m3 = mlogit(depvar~ic+oc|income+agehed+rooms+region, H.data, reflevel = 'hp')
summary(heating.m3)
heating.fit3 = fitted(heating.m3, outcome=FALSE)
colMeans(heating.fit3)

# Estimate Rank-Ordered Logit

data("Game", package = "mlogit")
head(Game)
Game

game.data = mlogit.data(Game, shape = "wide", choice ="ch", varying = 1:12, ranked = TRUE)
dim(game.data)
game.data
summary(game.data)

game.roc = mlogit(ch ~ own | hours + age, game.data, reflevel = "GameBoy")
summary(game.roc)


# Estimate Nested Logit
data("HC", package = "mlogit")
head(HC)
HC
dim(HC) # HC of New Data 
# 250 18
HC = mlogit.data(HC, varying = c(2:8, 10:16), choice="depvar", shape="wide")
dim(HC)
# 1750 8 (1750 = 250*7)
HC
HC$alt

cooling.modes = HC$alt %in% c("gcc", "ecc", "erc", "hpc")
HC
HC$alt
# Setting the cooling costs to zero for the options without cooling
cooling.modes
HC$icca[!cooling.modes] = 0
HC$occa[!cooling.modes] = 0

HC
cooling.modes

HC.m1 = mlogit(depvar ~ occa + icca + och + ich, HC)
HC.m2 = mlogit(depvar ~ occa + icca + och + ich, HC, nests = list(cooling = c("ecc", "erc", "gcc", "hpc"), noncool = c("ec", "gc", "er")))
summary(HC.m1)
summary(HC.m2)

HC.m1
HC.m2

# Random Effect Logit Model
data("Train", package = "mlogit")
head(Train)
# opposite은 그냥 양수 음수 바꿔서 출력하는 것
Train.data = mlogit.data(Train, shape = "wide", varying = 4:11, choice = "choice", sep = "", opposite = c("price", "time", "change", "comfort"), alt.levels = c("choice1", "choice2"), id = "id")
head(Train.data)

Train.ml = mlogit(choice ~ price + time + change + comfort, Train.data)
summary(Train.ml)

# "rpar" objects contain all the relevant information about the distribution of random parameters. 
# These functions enables to obtain easily descriptive 
# statistics, density, probability and quantiles of the distribution.
Train.mxl = mlogit(choice ~ price + time + change + comfort, Train.data, panel = TRUE, rpar = c(time = "n", change = "n", comfort = "n"), correlation = FALSE, R = 100, halton = NA)
summary(Train.mxl)

# 이건 위에서 설정한  rpar의 요약본 느낌 quantile 느낌
rpar(Train.mxl)

# Market Share Model

## example
data("Fishing", package = "mlogit")
head(Fishing)
Fish = mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
head(Fish)





