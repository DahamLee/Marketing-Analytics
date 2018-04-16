#Sample Normal Distribution########################
rnormal = function(n, mu=1, sigma=1) {
  n1 = ceiling(n/2)
  u1 = runif(n1)
  u2 = runif(n1)
  x1 = sqrt(-2*log(u1))*cos(2*pi*u2)
  x2 = sqrt(-2*log(u1))*sin(2*pi*u2)
  x = c(x1,x2)
  y = x[1:n]*sigma + mu
  y
}
hist(rnormal(1000, mu=5, sigma=2), nclass=20)
rnormal(1000, mu=5, sigma=2)
a=c(1,2)
a
a[1:100]
mean(runif(1000))

#Accept-Reject Methods#########################
rc1 = rcauchy(1000)
hist(rc1)


#Uniform Distribution in Circle#########################
x1=runif(1000,min=-1,max=1)
x2=runif(1000,min=-1,max=1)
plot(x1,x2)
xc = cbind(x1,x2)
head(xc)

xc = xc[(x1^2+x2^2<=1),]
dim(xc)
points(xc[,1], xc[,2], col="red")


# MCMC

x1 = rep(0, 1000)
x2 = rep(0, 1000)
rho = 0.7
for (m in 2:1000) {
  x1[m] = rnorm(1, mean=rho*x2[m-1], sd=sqrt(1-rho^2))
  x2[m] = rnorm(1, mean=rho*x1[m], sd=sqrt(1-rho^2))
}

result = lm(x1~x2)
summary(result)


