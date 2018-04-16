#install.packages("truncnorm")
#install.packages("mnormt")
rm(list=ls())
library(truncnorm)
library(mnormt)
#Bayesian models fitting for truncated regression
#stage 1. read data into R and create columns for censored data
DataFile = "/Users/daham/Desktop/Marketing Analysis/assignment4/CreditCard_SOW_data2.csv"
sow.data = read.csv(DataFile, header=T)
sow.data$Cens0 = (sow.data$SOW==0)*1
sow.data$Cens1 = (sow.data$SOW==1)*1

#extract right and left censored data

sow.XRC = cbind(1, as.matrix(sow.data[sow.data$Cens0==1, 3:4]))
sow.XLC = cbind(1, as.matrix(sow.data[sow.data$Cens1==1, 3:4])) 
sow.X = cbind(1, as.matrix(sow.data[, 3:4]))
# sow.X %*% curBeta
sow.X2 = t(sow.X)%*%sow.X
#sow.X2
#t(sow.X) transpose
# %*% = matrix multiplication

# XRC 의 개수
nRC = dim(sow.XRC)[1]
#nRC
# XLC 의 개수
nLC = dim(sow.XLC)[1]
#nLC
nObs = dim(sow.data)[1]
#nObs

#stage 2. Initial Setup for the algorithm
NIT = 10000       #num of interations
nBurn = 2000      #num of burn-ins  
NIT.eff = NIT - nBurn    #effective sample size
thin.step = 10           #thinning  
NIT.thin = floor(NIT.eff/thin.step)   #effective sample size after thinning
#NIT.thin

#stage 3. Record Posterior samples
beta.dim = 3
beta.pos = matrix(0, NIT.thin, beta.dim)
# NIT.thin' value is 800
#beta.pos
tau.pos = rep(0, NIT.thin)
#tau.pos
#stage 4. priors
#for Beta: mNormal(mu.beta, sigma.beta)
mu.beta = rep(0,beta.dim) 
#mu.beta
sigma.beta = 1E6 * diag(beta.dim)  
iSigma.beta = 1E-6 * diag(beta.dim)  #inverse prior covariance matrix 


#prior for precision: Gamma(a.tau, b.tau)
a.tau = 1/2
b.tau = 1/2

#stage 5. Gibbs sampler

#initialize the loop
curBeta = c(0.5, 0, 0) 
curBeta
curTau = 4
g = 1

#sow.XRC
#sow.X%*%curBeta
#sow.XRC%*%curBeta
sow.XRC%*%curBeta
#main loop
for (m in 1:NIT){
	#step 1. sample latent SOW 
	#step 1.a. sample SOW right-censored at 0
	#Please fill in the blank below the code for sampling the latent SOW when the observed SOW=0
	#Please name your sampled latent SOW curYRC
  curYRC = rtruncnorm(nRC, a=-Inf, b=0, mean=sow.XRC%*%curBeta, sd=sqrt(1/curTau))
	curYRC
	#step 1.b sample SOW left-censored at 1
	#Please fill in the blank below the code for sampling the latent SOW when the observed SOW=1
	#Please name your sampled latent SOW curYLC
  curYLC = rtruncnorm(nLC, a=1, b=Inf, mean=sow.XLC%*%curBeta, sd=sqrt(1/curTau))
  #curYLC
	#step 2 sample beta
	#step 2.a impute the latent variables
	sow.Y = sow.data$SOW
	sow.Y[sow.data$Cens0==1] = curYRC
	sow.Y[sow.data$Cens1==1] = curYLC
	#step 2.b sample beta
	# solve 함수 설명: This generic function solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
	sigma.hat = solve(curTau*sow.X2 + iSigma.beta)
	#sigma.hat
	betaPos.mn = sigma.hat%*%(curTau*t(sow.X)%*%sow.Y + iSigma.beta%*%mu.beta)
	#betaPos.mn
	curBeta = as.vector(rmnorm(1, mean=betaPos.mn, varcov=sigma.hat)) 
	curBeta
	
	#step 3 sample tau (precision = 1/sigma^2)
	sowE.hat = sow.Y-sow.X%*%curBeta
	#sowE.hat
	curTau = rgamma(1, 0.5*nObs+a.tau, 0.5*t(sowE.hat)%*%sowE.hat+b.tau)
	#curTau
	#save thinned samples after burn-ins
	if ((m > nBurn) & (m%%thin.step == 0)) {
		beta.pos[g,] = curBeta
		tau.pos[g] = curTau
		g = g+1
	}
}

mean(beta.pos[,1])
mean(beta.pos[,2])
mean(beta.pos[,3])
log(sqrt(mean(tau.pos)))

par(mfrow=c(2, 2))
plot(beta.pos[,1], type="l", col="black", main = "intercept")
lines(lowess(beta.pos[,1]), col="red")
plot(beta.pos[,2], type="l", col="black", main = "Promotion")
lines(lowess(beta.pos[,2]), col="red")
plot(beta.pos[,3], type="l", col="black", main = "Balance")
lines(lowess(beta.pos[,3]), col="red")
plot(log(sqrt(tau.pos)), type="l", col="black", main = "logSigma")
lines(lowess(log(sqrt(tau.pos))), col="red")


par(mfrow=c(2, 2))
hist(beta.pos[,1], main = "intercept")
hist(beta.pos[,2], main = "Promotion")
hist(beta.pos[,3], main = "Balance")
hist(log(sqrt(tau.pos)), main = "logSigma")

plot(tau.pos, type="l", col="black", main = "Tau")
lines(lowess(tau.pos), col="red")
hist(tau.pos, main = "Tau")
quantile(tau.pos, c(.025, .5, .975))

quantile(beta.pos[,1], c(.025, .5, .975))
quantile(beta.pos[,2], c(.025, .5, .975))
quantile(beta.pos[,3], c(.025, .5, .975))
quantile(log(sqrt(tau.pos)), c(.025, .5, .975))

mean(beta.pos[,1])
mean(beta.pos[,2])
mean(beta.pos[,3])
log(sqrt(mean(tau.pos)))
beta.pos
#sow.Y[sow.data$Cens1==1]=c(1,2,3)
#sow.Y[sow.data$Cens0==1]
#sow.Y
#sow.Y[sow.data$Cens1==1]
#a=solve(sow.X2)
#a%*%sow.X2
#rtruncnorm(10, a=-0.1, b=0.1, mean = 0, sd = 1)
#rmnorm(1, mean=1, varcov=1)

#rtruncnorm(3, a=-Inf, b=0, mean=0, sd=1)
#rtruncnorm(3, a=1, b=Inf, mean=0, sd=1)

#rtruncnorm(nRC, a=-Inf, b=0, mean=curBeta, sd=1)
#mean(rtruncnorm(10000, a=-Inf, b=Inf, mean=curBeta, sd=1))
#hist(rtruncnorm(nRC, a=-Inf, b=Inf, mean=curBeta, sd=1), breaks=100)
#curBeta
#sow.X%*%curBeta
#rtruncnorm(nRC, a=-Inf, b=0, mean=mean(sow.X%*%curBeta), sd=sqrt(1/curTau))
mean(rtruncnorm(10000, a=-Inf, b=Inf, mean=matrix(c(1,2,1,3),nrow=2,ncol=2), sd=1))
matrix(c(1,2,1,2),nrow=2,ncol=2)
sqrt(1/4)
rtruncnorm(nRC, a=-Inf, b=0, mean=mean(sow.XRC%*%curBeta), sd=sqrt(1/curTau))

     