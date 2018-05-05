rm(list=ls())
library(truncnorm)
library(mnormt)
#Bayesian models fitting for truncated regression
#stage 1. read data into R and create columns for censored data
DataFile = "/Users/daham/Desktop/Marketing Analysis/Assignment6/CreditCard_LatePayment_data.csv"
LP.data = read.csv(DataFile, header=T)
head(LP.data)
#stage 1. subset the data for Latepay = 1 and =0 
#Please filled in the code below

LP.glm = glm(Latepay~Usage+Balance, data=LP.data, family=binomial(link="probit"))
#LP.lm = lm(Latepay~Usage+Balance, data=LP.data)
summary(LP.glm)
#summary(LP.lm)


head(LP.data)
head(LP.data$Latepay)

# slice left-censored and right-censored factors(called X variables in Regression)
LP.LC = cbind(1,as.matrix(LP.data[LP.data$Latepay==0, 3:4]))
LP.RC = cbind(1,as.matrix(LP.data[LP.data$Latepay==1, 3:4]))
head(LP.LC)
# slice only factors(called X variables in Regression)
LP.X = cbind(1, as.matrix(LP.data[, 3:4]))

head(LP.X)
head(LP.LC)
head(LP.RC)

# Left Censored: 0쪽, Right Censored: 1쪽
# 0,1 의 총 개수 = 전체 개수
nLC = dim(LP.LC)[1]
nRC = dim(LP.RC)[1]
nObs = dim(LP.data)[1]

#stage 2. Initial Setup for the algorithm
NIT = 10000       #num of interations
nBurn = 2000      #num of burn-ins  
NIT.eff = NIT - nBurn    #effective sample size
thin.step = 10           #thinning  
NIT.thin = floor(NIT.eff/thin.step)   #effective sample size after thinning

#stage 3. Record Posterior samples
beta.dim = 3
beta.pos = matrix(0, NIT.thin, beta.dim)
head(beta.pos)
# 0으로 채우고, NIT.thin X beta.dim 매트릭스
head(beta.pos)

###### adding ######
# tau.pos = rep(0, NIT.thin)
# dim(as.matrix(tau.pos))
# sigma.tau = 1
# curTau = 4
###### adding ######

#stage 4. priors
#for Beta: mNormal(mu.beta, sigma.beta)
mu.beta = rep(0,beta.dim) 
sigma.beta = 1E6 * diag(beta.dim)  
iSigma.beta = 1E-6 * diag(beta.dim)  #inverse prior covariance matrix 

# prior for precision: gamma(nu, nu)
# nu = 1/2

#stage 5. Gibbs sampler

#initialize the loop
curBeta = c(0.1, 0, 0)
g = 1
# X squared( X^T * X ) is necessary to solve sigma
LP.X_squared = t(LP.X)%*%LP.X
LP.X_squared


#main loop
for (m in 1:NIT){
	#step 1. sample the latent variable > 0 if Latepay=1, <0 if Latepay=0 
	#Please fill in the code 
  #mean(LP.LC%*%curBeta)
  
  #mean.logit.LC = mean(exp(LP.LC%*%curBeta)/(1+exp(LP.LC%*%curBeta)))
  #mean.logit.RC = mean(exp(LP.RC%*%curBeta)/(1+exp(LP.RC%*%curBeta)))
  #curLC = rtruncnorm(nLC, a=0, b=1/2, mean=mean.logit.LC, sd=sqrt(1/curTau))
  #curRC = rtruncnorm(nRC, a=1/2, b=1, mean=mean.logit.RC, sd=sqrt(1/curTau))
  
	curLC = rtruncnorm(nLC, a=-Inf, b=0, mean=LP.LC%*%curBeta, sd=1)
	#curLC
	curRC = rtruncnorm(nRC, a=0, b=Inf, mean=LP.RC%*%curBeta, sd=1)
	
  #curLCY = exp(rtruncnorm(nLC, a=-Inf, b=0, mean=LP.LC%*%curBeta, sd=sqrt(1/curTau)))
  #curRCY = exp(rtruncnorm(nRC, a=0, b=Inf, mean=LP.RC%*%curBeta, sd=sqrt(1/curTau)))
  
	#curLC = curLCY/(curRCY+curLCY)
	#curRC = curRCY/(curLCY+curRCY)
	
  #curLC = exp(rtruncnorm(nLC, a=-Inf, b=0, mean=LP.LC%*%curBeta, sd=sqrt(1/curTau)))
	#curRC = exp(rtruncnorm(nRC, a=0, b=Inf, mean=LP.RC%*%curBeta, sd=sqrt(1/curTau)))
  #mean(rtruncnorm(nLC, a=0, b=Inf, mean=1, sd=sqrt(1/curTau)))
  #mean(exp(LP.LC%*%curBeta)/(1+exp(LP.LC%*%curBeta)))
	
	#step 2 sample beta
	#Please fill in the code 
	head(LP.data)
	LP.Y = LP.data$Latepay
	
	#크기를 보고 싶으면 먼저 matrix를 만든 뒤 dim을 이용한다.
	#dim(as.matrix(LP.Y))
	
	#dim(as.matrix(curLC))
	#dim(as.matrix(LP.Y[LP.data$Latepay==0]))
	dim(as.matrix(curRC))
	dim(as.matrix(LP.Y[LP.data$Latepay==1]))
	
	LP.Y[LP.data$Latepay==0] = curLC
	LP.Y[LP.data$Latepay==1] = curRC
	
	# solve example (to calculate inverse function)
	#solve(matrix(c(2,0,0,2), nrow=2, ncol=2))
	
	sigma.beta.pos.n = solve(iSigma.beta+LP.X_squared)
	sigma.beta.pos.n
	
	beta.pos.n = sigma.beta.pos.n%*%(iSigma.beta%*%mu.beta + t(LP.X)%*%LP.Y)
	beta.pos.n
	
	# Conjugate priors:
	# beta
	curBeta = as.vector(rmnorm(1, mean=beta.pos.n, varcov=sigma.beta.pos.n))
	curBeta
	
	# (Y-Xbeta)^T * (Y-Xbeta)
	#LP.epsilon = t(LP.Y-LP.X%*%curBeta)%*%(LP.Y-LP.X%*%curBeta)
	#LP.epsilon
	
	# sigma(tau)
	#curTau = rgamma(1, 0.5*(nObs+nu), 0.5*(LP.epsilon+nu)+nu)
	#curTau
	
	# dimension check
	dim(sigma.beta.pos.n)
	dim(iSigma.beta)
	#dim(curTau)
	dim(t(LP.X))
	dim(as.matrix(LP.Y))
	dim(as.matrix(LP.X))
	dim(as.matrix(mu.beta))
	dim(curBeta)
	head(beta.pos.n)
	
	#save thinned samples after burn-ins
	if ((m > nBurn) & (m%%thin.step == 0)) {
		beta.pos[g,] = curBeta
		g = g+1
	}
}
#beta.pos
#tau.pos

mean(beta.pos[,1])
mean(beta.pos[,2])
mean(beta.pos[,3])
plot(beta.pos[,1], type="l", col="black", main = "intercept")
lines(lowess(beta.pos[,1]), col="red")
plot(beta.pos[,2], type="l", col="black", main = "Usage")
lines(lowess(beta.pos[,2]), col="red")
plot(beta.pos[,3], type="l", col="black", main = "Balance")
lines(lowess(beta.pos[,3]), col="red")
summary(LP.glm)

hist(beta.pos[,1], main = "intercept", breaks = 30)
hist(beta.pos[,2], main = "Usage", breaks = 30)
hist(beta.pos[,3], main = "Balance", breaks = 30)


quantile(beta.pos[,1], c(.025, .5, .975))
quantile(beta.pos[,2], c(.025, .5, .975))
quantile(beta.pos[,3], c(.025, .5, .975))
