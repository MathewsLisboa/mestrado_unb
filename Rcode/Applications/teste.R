library(EnvStats)
library(evd)
qbgev   <- function(p, m, sigma, delta){
  # Compute distribution points according to their sign
  mu <- m - (-sigma*log(-log(0.5)))^(1/(delta+1)) # m is median
  quantile <- sign(qgevd(p, 0, sigma, 0))*(abs(qgevd(p, 0, sigma, 0))^(1/(delta + 1))) + mu
  # Return Value
  return(quantile)
}


rbgev <- function(n, m, sigma, delta){
  # Compute auxiliary variables:
  U <- runif(n)
  # Compute random numbers
  mu <- m - (-sigma*log(-log(0.5)))^(1/(delta+1)) # m is median
  rnumber <- qbgev(U, m, sigma, delta)
  # Return Value
  return(rnumber)
}

source("BGEV_MLE.r")
set.seed(1994)
n <- 40
x1 <- runif(n); x2 <- runif(n)
X <- matrix(c(rep(1,n),x1, x2),ncol=3,byrow=F); #regressor matrix for the median model
kk1 <- ncol(X); 
beta <- c(4,2,3)

eta <- X%*%beta
y <- rbgev(n=n, m=eta, sigma = 1, delta=1);hist(y)

fit_BGEV <- MLE_BGEV(y, X, method="BFGS", maxit=200)
fit_BGEV



#***********************Curvas **********************#

source("envelope_function.r")
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))



#**********density function**********#
dbgev <- function(y, m, sigma, delta){ #xi=0
  # Compute auxiliary variables:
  mu <- m - (-sigma*log(-log(0.5)))^(1/(delta+1)) # m is median
  T      <- (y-mu)*(abs(y-mu)^delta)
  derivate_T <- (delta + 1)*(abs(y-mu)^delta)
  # Compute density points
  pdf    <- dgev(T, loc=0, scale=sigma, shape=0)*derivate_T
  # Return Value
  return(pdf)
}



x=seq(5,10,by=0.001)
d1=dbgev (x, m=7.5, sigma=1, delta=0)
d2=dbgev(x, m=7.5, sigma=1, delta=1)
d3=dbgev(x, m=7.5, sigma=1, delta=2)
d4=dbgev(x, m=7.5, sigma=1, delta=3)
d5=dbgev(x, m=7.5, sigma=1, delta=4)
d6=dbgev(x, m=7.5, sigma=1, delta=5)

plot(x=NULL,xlim=c(5,10),ylim=c(0,1.6),xlab="y",ylab="f(y)",cex.axis=1.3,
cex.lab=1.4,cex=1)
lines(x,d1,lty=1,col=1,lwd=2)
lines(x,d2,lty=1,col=2,lwd=2)
lines(x,d3,lty=1,col=3,lwd=2)
lines(x,d4,lty=1,col=4,lwd=2)
lines(x,d5,lty=1,col=5,lwd=2)
lines(x,d6,lty=1,col=6,lwd=2)

legend(8.5,1.5,col=c(1,2,3,4,5,6)
,lwd=rep(2,6),lty=c(1,1,1,1,1,1,1),
legend = c("d=0","d=1","d=2","d=3",
"d=4","d=5"),box.lty=0,y.intersp = 1.5)














