## Project

## Exercise 1


install.packages("plot3Drgl")
library(plot3Drgl)
library(emdbook)
install.packages("maxLik")
library(maxLik)
library(MASS)
install.packages("fitdistrplus")
library(fitdistrplus)
install.packages("evd")
library(evd)
install.packages("univariateML")
library(univariateML)
library(plot3D)

# Create a function to generate form kies-frechet distribution

rkif = function(n,alpha,beta) {

# Generate from Uniform(0,1)

# n = 100

X <- runif(n,min=0,max=1)

# parameters theta = (alpha,beta)

denom = (log(1/X))^(1/beta)
Y = (alpha/denom)*ifelse(X>0 & X<1,yes=1,no=0)

return(Y)
                              }

Y <- rkif(100,0.05,0.7)
hist(Y,freq=FALSE,ylim=c(0,3))
lines(density(Y))

# Graph of frechet distribution 
par(mfrow=c(1,2))
# PDF 

curve(dfrechet(x,scale = 1,shape = 1,loc = 0),seq(0,4,length=100),
      xlim = c(0,10),ylim = c(0,1.2),col="blue",ylab = "PDF",xlab = "data",
      main = " Probability Density Graphs",
      font=6,font.main=6,font.lab=6)
curve(dfrechet(x,scale = 1,shape = 2,loc = 0),seq(0,4,length=100),add = TRUE,col="red")
curve(dfrechet(x,scale = 2,shape = 1,loc = 0),seq(0,4,length=100),add = TRUE,col="green")
curve(dfrechet(x,scale = 2,shape = 2,loc = 0),seq(0,4,length=100),add = TRUE,col="black")
curve(dfrechet(x,scale = 3,shape = 2,loc = 0),seq(0,4,length=100),add = TRUE,col="yellow")
curve(dfrechet(x,scale = 4,shape = 3,loc = 0),seq(0,4,length=100),add = TRUE,col="pink")
legend("topright",legend = c("scale = 1 ; shape = 1",
                            "scale = 1 ; shape = 2",
                            "scale = 2 ; shape = 1",
                            "scale = 2 ; shape = 2",
                            "scale = 3 ; shape = 2",
                            "scale = 4 ; shape = 3"),col = c("blue",
                                                             "red",
                                                             "green",
                                                             "black",
                                                             "yellow",
                                                             "pink"), lty = rep(1,7),box.lty = 1,
                                                                                      pch = 16,cex = 0.75)

# CDF 

curve(pfrechet(x,scale = 1,shape = 1,loc = 0),seq(0,4,length=100),
      xlim = c(0,10),ylim = c(0,1),col="blue",ylab = "CDF",xlab = "data",
      main = " Cumulative Density Graphs",
      font=6,font.main=6,font.lab=6)
curve(pfrechet(x,scale = 1,shape = 2,loc = 0),seq(0,4,length=100),add = TRUE,col="red")
curve(pfrechet(x,scale = 2,shape = 1,loc = 0),seq(0,4,length=100),add = TRUE,col="green")
curve(pfrechet(x,scale = 2,shape = 2,loc = 0),seq(0,4,length=100),add = TRUE,col="black")
curve(pfrechet(x,scale = 3,shape = 2,loc = 0),seq(0,4,length=100),add = TRUE,col="yellow")
curve(pfrechet(x,scale = 4,shape = 3,loc = 0),seq(0,4,length=100),add = TRUE,col="pink")
legend("bottomright",legend = c("scale = 1 ; shape = 1",
                             "scale = 1 ; shape = 2",
                             "scale = 2 ; shape = 1",
                             "scale = 2 ; shape = 2",
                             "scale = 3 ; shape = 2",
                             "scale = 4 ; shape = 3"),col = c("blue",
                                                              "red",
                                                              "green",
                                                              "black",
                                                              "yellow",
                                                              "pink"), lty = rep(1,7),box.lty = 1,
                                                                                    pch = 16,cex = 0.75)

par(mfrow=c(1,2))
# Behaviour of shape in Frechet distribution
curve(dfrechet(x,scale = 2,shape = 3,loc = 0),seq(0,4,length=100),col="red",xlim = c(0,6),ylim=c(0,3),
      ylab = "f(y|theta)",main="Behaviour of changing the shape",font=6,font.main=6,font.axis=6,xlab = "data",
      font.lab=6)
curve(dfrechet(x,scale = 2,shape = 5,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
curve(dfrechet(x,scale = 2,shape = 7,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
curve(dfrechet(x,scale = 2,shape = 9,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
curve(dfrechet(x,scale = 2,shape = 11,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
curve(dfrechet(x,scale = 2,shape = 13,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
legend("topright",legend = c("beta=3","beta=5","beta=7","beta=9","beta=11","beta=13"),
       col = c("red","blue","blue","blue","blue","blue"),lty = rep(1,6), pch = 16,cex = 0.75)


#Behaviour of scale in Frechet distribution
curve(dfrechet(x,scale = 2,shape = 5,loc = 0),seq(0,4,length=100),col="red",xlim = c(0,40),ylim=c(0,1.2),
      ylab = "f(y|theta)",main="Behaviour of changing the scale",font=6,font.main=6,font.axis=6,
      xlab = "data",font.lab=6)
curve(dfrechet(x,scale = 4,shape = 5,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
curve(dfrechet(x,scale = 6,shape = 5,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
curve(dfrechet(x,scale = 8,shape = 5,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
curve(dfrechet(x,scale = 10,shape = 5,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
curve(dfrechet(x,scale = 12,shape = 5,loc = 0),seq(0,4,length=100),add=TRUE,col="blue")
legend("topright",legend = c("alpha=2","alpha=4","alpha=6","alpha=8","alpha=10","alpha=12"),
       col = c("red","blue","blue","blue","blue","blue"),lty = rep(1,6), pch =16,cex = 0.75)

#
par(mfrow=c(1,1))
k = rfrechet(100,scale=3,shape=5,loc=0)
m = sum(1/k)
l = sum(log(k))
n = 100
f <- function(x,y) {
    n*log(y) + n*y*log(x) - (x^y)*m - (y+1)*l
  
}
x <- y <- seq(0.1,4,len=20)
z <- outer(x, y, f)

persp3Drgl(z=z)

curve3d(n*log(y) + n*y*log(x) - (x^y)*m - (y+1)*l,from = c(1,1), to = c(4,4),
        font=6,font.main=6,font.lab=6,main="3-D graph of log-likelihood")
persp3D(x,y,z,theta=10, phi=10, axes=TRUE,scale=2, box=TRUE, nticks=5, 
        ticktype="detailed", xlab="X-value", ylab="Y-value", zlab="Z-value", 
        main="3-D graph of log-likelihood",font=6,font.main=6,font.axis=6)

#---------------------------------------#
#-------------- Frechet ----------------#
#---------------------------------------#
set.seed(4)
k = rfrechet(100,scale=3,shape=6,loc=0)

f.k <- fitdist(1/k,"weibull")

est.scale <- 1/f.k$est[2]
std.scale <- f.k$sd[2]*(est.scale^2)

A <- rbind(c(f.k$est[1],est.scale),c(f.k$sd[1],std.scale))
row.names(A) <- c("estimate","std error")
A

#-------- Confidence Interval ----------#

L.shape  <- f.k$estimate[1] - qnorm(0.975)*f.k$sd[1]
U.shape  <- f.k$estimate[1] + qnorm(0.975)*f.k$sd[1]

L.scale  <- est.scale - qnorm(0.975)*std.scale
U.scale  <- est.scale + qnorm(0.975)*std.scale

ci.shape <- matrix(c(L.shape,U.shape),1,2)
colnames(ci.shape)  <- c("Lower","Upper")
row.names(ci.shape) <- "C.I." 
ci.shape

ci.scale <- matrix(c(L.scale,U.scale),1,2)
colnames(ci.scale)  <- c("Lower","Upper")
row.names(ci.scale) <- "C.I." 
ci.scale

#---------------------------------------#
#-------------- Canada -----------------#
#---------------------------------------#

# The data represents a COVID-19 data belong to Canada of 36 days, from 10 April to 15 May 2020

canada <- c(3.1091,3.3825,3.1444,3.2135,2.4946,3.5146,4.9274,3.3769,6.8686,3.0914,4.9378,
            3.1091,3.2823,3.8594,4.0480, 4.1685, 3.6426, 3.2110, 2.8636, 3.2218, 2.9078, 
            3.6346, 2.7957, 4.2781, 4.2202, 1.5157, 2.6029, 3.3592, 2.8349, 3.1348, 2.5261,
            1.5806, 2.7704, 2.1901, 2.4141, 1.9048)

# maxLik package

Y = canada
N = length(canada)

logLikFun <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  sum1  <- sum((alpha/Y)^beta)
  N*log(beta) + N*beta*log(alpha) - sum1 - (beta+1)*sum(log(Y))
}

logLikGrad <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  logLikGradValues <- numeric(2)
  sum1  <- sum((alpha/Y)^beta)
  sum2  <- sum(log(alpha/Y)*(alpha/Y^beta))
  logLikGradValues[1] <- (beta/alpha)*(N-sum1)
  logLikGradValues[2] <- N/beta + N*log(alpha) - sum2 - sum(log(Y)) 
  return(logLikGradValues)
}
  
logLikHess <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  sum1  <- sum((alpha/Y)^beta)
  sum2  <- sum(log(alpha/Y)*(1/Y^beta))
  sum3  <- sum((log(alpha/Y))^2*((alpha/Y)^beta))
  logLikHessValues <- matrix(0, nrow = 2, ncol = 2)
  logLikHessValues[1, 1] <- - (beta/alpha^2)*(N+(beta-1)*sum1)
  logLikHessValues[1, 2] <- N/alpha - ((alpha)^(beta-1))*sum((1/Y)^beta) - beta*((alpha)^(beta-1))*sum2
  logLikHessValues[2, 1] <- logLikHessValues[1, 2]
  logLikHessValues[2, 2] <- - N/beta^2 - sum3
  return(logLikHessValues)
}

mleHess <- maxLik(logLik = logLikFun, grad = logLikGrad,
                  hess = logLikHess,start = c(alpha=1,beta=1))

summary(mleHess)

# Fitdistr package 

f.can <- fitdist(1/canada,"weibull")
f.can

plot(f.can)

plotdist(1/canada, histo = TRUE, demp = TRUE)

# univariateML package

mlinvweibull(canada)


#---------------------------------------#
#------------------ UK -----------------#
#---------------------------------------#

# The data represents a COVID-19 data belong 
# to The United Kingdom of 24 days, from 15 October to 7 November 2020

uk <- c(0.2240,0.2189, 0.2105, 0.2266, 0.0987, 0.1147, 0.3353, 0.2563, 0.2466, 0.2847, 0.2150, 
0.1821, 0.1200, 0.4206, 0.3456, 0.3045, 0.2903, 0.3377, 0.1639, 0.1350, 0.3866, 0.4678,
0.3515, 0.3232)

# maxLik package

Y = uk
N = length(uk)

logLikFun <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  sum1  <- sum((alpha/Y)^beta)
  N*log(beta) + N*beta*log(alpha) - sum1 - (beta+1)*sum(log(Y))
}

logLikGrad <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  logLikGradValues <- numeric(2)
  sum1  <- sum((alpha/Y)^beta)
  sum2  <- sum(log(alpha/Y)*(alpha/Y^beta))
  logLikGradValues[1] <- (beta/alpha)*(N-sum1)
  logLikGradValues[2] <- N/beta + N*log(alpha) - sum2 - sum(log(Y)) 
  return(logLikGradValues)
}

logLikHess <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  sum1  <- sum((alpha/Y)^beta)
  sum2  <- sum(log(alpha/Y)*(1/Y^beta))
  sum3  <- sum((log(alpha/Y))^2*((alpha/Y)^beta))
  logLikHessValues <- matrix(0, nrow = 2, ncol = 2)
  logLikHessValues[1, 1] <- - (beta/alpha^2)*(N+(beta-1)*sum1)
  logLikHessValues[1, 2] <- N/alpha - ((alpha)^(beta-1))*sum((1/Y)^beta) - beta*((alpha)^(beta-1))*sum2
  logLikHessValues[2, 1] <- logLikHessValues[1, 2]
  logLikHessValues[2, 2] <- - N/beta^2 - sum3
  return(logLikHessValues)
}

mleHess <- maxLik(logLik = logLikFun, grad = logLikGrad,
                  hess = logLikHess,start = c(alpha=1,beta=1))

summary(mleHess)

# Fitdistr package

f.uk <- fitdist(1/uk,"weibull")
f.uk

est.scale <- 1/f.uk$est[2]
std.scale <- f.uk$sd[2]*(est.scale^2)

B <- rbind(c(f.uk$est[1],est.scale),c(f.uk$sd[1],std.scale))
row.names(A) <- c("estimate","std error")
B

plot(f.uk)
plotdist(1/uk, histo = TRUE, demp = TRUE)

# univariateML

mlinvweibull(uk)

#-------- Confidence Interval ----------#

L.shape  <- f.k$estimate[1] - qnorm(0.975)*f.k$sd[1]
U.shape  <- f.k$estimate[1] + qnorm(0.975)*f.k$sd[1]
L.scale  <- est.scale - qnorm(0.975)*std.scale
U.scale  <- est.scale + qnorm(0.975)*std.scale
ci.shape <- c(L.shape,U.shape)
ci.scale <- c(L.scale,U.scale)

#---------------------------------------#
#------------- Netherlands -------------#
#---------------------------------------#

# The third data represents a COVID-19 data belonging to the Netherlands of 30 days, which recorded from 31 March to 30 April 2020. 
# This data formed of rough mortality rate. 

neth <- c(14.918,10.656, 12.274, 10.289, 10.832, 7.099, 5.928, 13.211, 
7.968, 7.584, 5.555, 6.027, 4.097, 3.611, 4.960, 7.498, 6.940, 
5.307, 5.048, 2.857, 2.254, 5.431, 4.462, 3.883, 3.461, 3.647, 
1.974, 1.273, 1.416, 4.235)

# maxLik package

Y = uk
N = length(uk)

logLikFun <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  sum1  <- sum((alpha/Y)^beta)
  N*log(beta) + N*beta*log(alpha) - sum1 - (beta+1)*sum(log(Y))
}

logLikGrad <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  logLikGradValues <- numeric(2)
  sum1  <- sum((alpha/Y)^beta)
  sum2  <- sum(log(alpha/Y)*(alpha/Y^beta))
  logLikGradValues[1] <- (beta/alpha)*(N-sum1)
  logLikGradValues[2] <- N/beta + N*log(alpha) - sum2 - sum(log(Y)) 
  return(logLikGradValues)
}

logLikHess <- function(param) {
  alpha <- param[1]
  beta  <- param[2]
  N     <- length(Y)
  sum1  <- sum((alpha/Y)^beta)
  sum2  <- sum(log(alpha/Y)*(1/Y^beta))
  sum3  <- sum((log(alpha/Y))^2*((alpha/Y)^beta))
  logLikHessValues <- matrix(0, nrow = 2, ncol = 2)
  logLikHessValues[1, 1] <- - (beta/alpha^2)*(N+(beta-1)*sum1)
  logLikHessValues[1, 2] <- N/alpha - ((alpha)^(beta-1))*sum((1/Y)^beta) - beta*((alpha)^(beta-1))*sum2
  logLikHessValues[2, 1] <- logLikHessValues[1, 2]
  logLikHessValues[2, 2] <- - N/beta^2 - sum3
  return(logLikHessValues)
}

mleHess <- maxLik(logLik = logLikFun, grad = logLikGrad,
                  hess = logLikHess,start = c(alpha=1,beta=1))

summary(mleHess)

# Fitdistr package

f.neth <- fitdist(1/neth,"weibull")
f.neth

est.scale <- 1/f.neth$est[2]
std.scale <- f.neth$sd[2]*(est.scale^2)

B <- rbind(c(f.neth$est[1],est.scale),c(f.neth$sd[1],std.scale))
row.names(B) <- c("estimate","std error")
B

#-------- Confidence Interval ----------#

L.shape  <- f.neth$estimate[1] - qnorm(0.975)*f.neth$sd[1]
U.shape  <- f.neth$estimate[1] + qnorm(0.975)*f.neth$sd[1]
L.scale  <- est.scale - qnorm(0.975)*std.scale
U.scale  <- est.scale + qnorm(0.975)*std.scale

ci.shape <- matrix(c(L.shape,U.shape),1,2)
colnames(ci.shape)  <- c("Lower","Upper")
row.names(ci.shape) <- "C.I." 
ci.shape

ci.scale <- matrix(c(L.scale,U.scale),1,2)
colnames(ci.scale) <- c("Lower","Upper")
row.names(ci.scale) <- "C.I." 
ci.scale

plot(f.neth,font.main=6,font.axis=6,col="blue",pch=20)

# univariateML

mlinvweibull(neth)