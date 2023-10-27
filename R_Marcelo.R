require(stats) # for spl
###############################################################
library(evd)
library(EnvStats)
library(bgumbel)

###################
#iid
###################


dbgevd <- function(y, mu, sigma, xi, delta){
  T      <- sigma*y*(abs(y)^delta)
  Tlinha <- sigma*(delta + 1)*(abs(y)^delta)
  pdf    <- dgevd(T, mu, sigma, xi)*Tlinha
  return(pdf)
}

#########################################################################
## FIGURAS variando mu
# mu= -2, -1, 0,1 e xi=1 (vale para maior), delta=1
curve(dbgevd(x, mu = -2, sigma = 1, xi=1, delta = 1), xlim = c(-3, 3),
      ylim = c(0, 2), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = -1, sigma = 1, xi=1, delta = 1), xlim = c(-2.5, 2.5), 
      ylim = c(0, 2), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=1, delta = 1), xlim = c(-2.5, 2.5), 
      ylim = c(0, 2), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 1, sigma = 1, xi=1, delta = 1), xlim = c(-2.5, 2.5),
      ylim = c(0, 2), lwd = 1.5, add = T, col = "red")

legend(x=-4, y=1, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                               expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")

# mu= -2, -1, 0,1 e xi=-1 (vale para maior), delta=1
curve(dbgevd(x, mu = -2, sigma = 1, xi=-1, delta = 1), xlim = c(-3, 3),
      ylim = c(0, 1.7), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = -1, sigma = 1, xi=-1, delta = 1), xlim = c(-2.5, 2.5), 
      ylim = c(0, 2), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 1), xlim = c(-2.5, 2.5), 
      ylim = c(0, 2), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 1, sigma = 1, xi=-1, delta = 1), xlim = c(-2.5, 2.5),
      ylim = c(0, 2), lwd = 1.5, add = T, col = "red")
legend(x=1, y=1.7, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                              expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")


# mu= -2, -1, 0,1 e xi=0.45 (vale para menor), delta=1
curve(dbgevd(x, mu = -2, sigma = 1, xi=0.45, delta = 1), xlim = c(-3, 3.5),
      ylim = c(0, 1.2), xlab=expression(" "), ylab = expression(" "),lwd = 1)
curve(dbgevd(x, mu = -1, sigma = 1, xi=0.45, delta = 1), xlim = c(-3,3), 
      ylim = c(0, 1.2), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.45, delta = 1), xlim = c(-3,3), 
      ylim = c(0, 1.2), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 1, sigma = 1, xi=0.45, delta = 1), xlim = c(-3,3),
      ylim = c(0, 1.2), lwd = 1, add = T, col = "red")
legend(x=1.4, y=1.2, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                                 expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")


# mu= -2, -1, 0,1 e xi=-0.45 (vale para menor ), delta=1
curve(dbgevd(x, mu = -2, sigma = 1, xi=-1, delta = 1), xlim = c(-3, 3.5),
      ylim = c(0, 1.2), xlab=expression(" "), ylab = expression(" "),lwd = 1)
curve(dbgevd(x, mu = -1, sigma = 1, xi=-1, delta = 1), xlim = c(-3,3), 
      ylim = c(0, 1.2), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 1), xlim = c(-3,3), 
      ylim = c(0, 1.2), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 1, sigma = 1, xi=-1, delta = 1), xlim = c(-3,3),
      ylim = c(0, 1.2), lwd = 1, add = T, col = "red")
legend(x=1.4, y=1.2, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                                expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")
############################################################################
##### VAriando delta para qsi negativo %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55555
#  Negativo delta= -0.75, -0.5, 0.25, 0 e xi=-0.25 negat,, mu=0, sigma=1 
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = -0.75), xlim = c(-3, 4),
      ylim = c(0, 0.9), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = -0.5), xlim = c(-3, 4), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = -0.25), xlim = c(-3, 4), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = 0), xlim = c(-3, 4),
      ylim = c(0, 0.9), lwd = 1.5, add = T, col = "red")
legend(x=1, y=0.8, legend = c(expression(paste(delta, "= -0.75")), expression(paste(delta, "= -0.5")),
                              expression(paste(delta, "= -0.25")), expression(paste(delta, "= 0")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")


# Positivo delta= 0, 0.5,1,3 e xi=-0.25 negat, mu=0, sigma=1 
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = 0), xlim = c(-2, 3),
      ylim = c(0, 0.9), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = 0.5), xlim = c(-2, 3), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = 1), xlim = c(-2, 3), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = 3), xlim = c(-2, 3),
      ylim = c(0, 0.9), lwd = 1.5, add = T, col = "red")
legend(x=1.2, y=0.8, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 0.5")),
                              expression(paste(delta, "= 1")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")

#  Negativo delta= -0.75, -0.5, 0.25, 0 e xi=-1 negat,, mu=0, sigma=1 
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = -0.75), xlim = c(-1.5, 3),
      ylim = c(0, 1.8), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = -0.5), xlim = c(-1.5, 3), 
      ylim = c(0, 1.8), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = -0.25), xlim = c(-1.5, 3), 
      ylim = c(0, 1.8), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 0), xlim = c(-1.5, 3),
      ylim = c(0, 1.8), lwd = 1.5, add = T, col = "red")
legend(x=1, y=1.8, legend = c(expression(paste(delta, "= -0.75")), expression(paste(delta, "= -0.5")),
                              expression(paste(delta, "= -0.25")), expression(paste(delta, "= 0")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")

# Positivo delta= 0, 0.5,1,3 e xi=-0.25 negat, mu=0, sigma=1 
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 0), xlim = c(-2, 3),
      ylim = c(0, 1.2), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 0.5), xlim = c(-2, 3), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 1), xlim = c(-2, 3), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 3), xlim = c(-2, 3),
      ylim = c(0, 0.9), lwd = 1.5, add = T, col = "red")
legend(x=1.2, y=1.2, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 0.5")),
                                expression(paste(delta, "= 1")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### VAriando delta para qsi positivo %
#  Negativo delta= -0.75, -0.5, 0.25, 0 e xi=0.5 negat,, mu=0, sigma=1 
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.5, delta = -0.75), xlim = c(-3, 4),
      ylim = c(0, 0.9), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.5, delta = -0.5), xlim = c(-3, 4), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.5, delta = -0.25), xlim = c(-3, 4), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.5, delta = 0), xlim = c(-3, 4),
      ylim = c(0, 0.9), lwd = 1.5, add = T, col = "red")
legend(x=1, y=0.8, legend = c(expression(paste(delta, "= -0.75")), expression(paste(delta, "= -0.5")),
                              expression(paste(delta, "= -0.25")), expression(paste(delta, "= 0")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")


# Positivo delta= 0, 0.5,1,3 e xi=0.5 negat, mu=0, sigma=1 
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.5, delta = 0), xlim = c(-2, 3),
      ylim = c(0, 1.5), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.5, delta = 0.5), xlim = c(-2, 3), 
      ylim = c(0, 1.5), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.5, delta = 1), xlim = c(-2, 3), 
      ylim = c(0, 1.5), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 0, sigma = 1, xi=0.5, delta = 3), xlim = c(-2, 3),
      ylim = c(0, 1.5), lwd = 1.5, add = T, col = "red")
legend(x=1.2, y=0.8, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 0.5")),
                                expression(paste(delta, "= 1")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")

#  Negativo delta= -0.75, -0.5, 0.25, 0 e xi=1.5 negat,, mu=0, sigma=1 
curve(dbgevd(x, mu = 0, sigma = 1, xi=1.5, delta = -0.75), xlim = c(-1, 1.2),
      ylim = c(0, 2.5), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = 0, sigma = 1, xi=1.5, delta = -0.5), xlim = c(-1, 1.2), 
      ylim = c(0, 2.5), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=1.5, delta = -0.25), xlim = c(-1, 1.2), 
      ylim = c(0, 2.5), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 0, sigma = 1, xi=1.5, delta = 0), xlim = c(-1, 1.2),
      ylim = c(0, 2.5), lwd = 1.5, add = T, col = "red")
legend(x=-1, y=2.5, legend = c(expression(paste(delta, "= -0.75")), expression(paste(delta, "= -0.5")),
                              expression(paste(delta, "= -0.25")), expression(paste(delta, "= 0")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")

# Positivo delta= 0, 0.5,1,3 e xi=-0.25 negat, mu=0, sigma=1 
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 0), xlim = c(-2, 3),
      ylim = c(0, 1.2), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 0.5), xlim = c(-2, 3), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 1), xlim = c(-2, 3), 
      ylim = c(0, 0.9), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 0, sigma = 1, xi=-1, delta = 3), xlim = c(-2, 3),
      ylim = c(0, 0.9), lwd = 1.5, add = T, col = "red")
legend(x=1.2, y=1.2, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 0.5")),
                                expression(paste(delta, "= 1")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
qbgevd   <- function(p, mu, sigma, xi, delta){
  quantile <- sign(qgevd(p, mu, sigma, xi))*(abs(qgevd(p, mu, sigma, xi))/sigma)^(1/(delta + 1))
  return(quantile)
}


rdgevd <- function(n, mu, sigma, xi, delta){
  U <- runif(n)
  rnumber <- qbgevd(U, mu, sigma, xi, delta)
  return(rnumber)
}


n       <- 500
mu      <- 0
sigma   <- 10
xi      <- 0.5
delta   <- 4


Z <- rdgevd(n, mu, sigma, xi, delta)

x <- seq(min(Z), max(Z), 0.01)
hist(Z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
lines(x,dbgevd(x, mu, sigma, xi, delta),lty=1,lwd=2)
box()




lik1 <- function(theta, y){
  par_len <- length(theta)
  mu      <- theta[1]
  sigma   <- theta[2]
  xi      <- theta[3]
  delta   <- theta[4]
  
  T      <- sigma*y*(abs(y)^delta)
  Tlinha <- sigma*(delta + 1)*(abs(y)^delta)
  
  dbgevd <- dgevd(T, mu, sigma, xi)*Tlinha
  
  logl <- sum(log(dbgevd(y,mu, sigma, xi, delta)))
  return(-logl)
}



n       <- 500
mu      <- 0
sigma   <- 10
xi      <- 0.5
delta   <- 4



Z <- rdgevd(n, mu, sigma, xi, delta)

starts <- c(mu, sigma, xi, delta)

test <- optim(par= starts, fn = lik1, y=Z, method="BFGS")
test


###################
#Monte Carlo
###################
#mu = 1
#sigma = 10
#xi = -0.5 and 0.5
#delta -0.5, 0, 0.3, 1, 2




R       <- 500
n       <- 400
mu      <- 1
sigma   <- 10
xi      <- 0.3
delta   <- 2


muhat      = rep(NA, times = R)
sigmahat   = rep(NA, times = R)
xihat      = rep(NA, times = R)
deltahat   = rep(NA, times = R)

starts <- c(mu, sigma, xi, delta)


for(k in (1:R)){
  
  Z <- rdgevd(n, mu, sigma, xi, delta)
  
  esti <-  optim(par= starts, fn = lik1, y=Z, method="BFGS")
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}


estimate_xi     <-   mean(xihat)
estimate_mu     <-   mean(muhat)
estimate_sigma  <-   mean(sigmahat)
estimate_delta  <-   mean(deltahat) 


#estimate_xi 
#estimate_mu
#estimate_sigma 
#estimate_delta


#starts


RB_xi    <- (estimate_xi - xi)/xi
RB_mu    <- (estimate_mu - mu)/mu
RB_sigma <- (estimate_sigma - sigma)/sigma
RB_delta <- (estimate_delta - delta)/delta


RB_xi
RB_mu
RB_sigma
RB_delta


RMSE_xi    <- sqrt(var(xihat)     +  (estimate_xi - xi)^2   )
RMSE_mu    <- sqrt(var(muhat)     +  (estimate_mu - mu)^2   )
RMSE_sigma <- sqrt(var(sigmahat)  +  (estimate_sigma - sigma)^2   )
RMSE_delta <- sqrt(var(deltahat)  +  (estimate_delta - delta)^2  )


RMSE_xi 
RMSE_mu
RMSE_sigma
RMSE_delta





muhat50  <- muhat
muhat100 <- muhat
muhat200 <- muhat
muhat400 <- muhat


boxplot(muhat50, muhat100, muhat200, muhat400, xlab = "Sample size", ylab = "", names = c("50", "100", "200", "400"))  
abline(h=mu)

sigmahat50  <- sigmahat
sigmahat100 <- sigmahat
sigmahat200 <- sigmahat
sigmahat400 <- sigmahat



boxplot(sigmahat50, sigmahat100, sigmahat200, sigmahat400, xlab = "Sample size", ylab = "", names = c("50", "100", "200", "400"))
abline(h=sigma)

xihat50  <- xihat
xihat100 <- xihat
xihat200 <- xihat
xihat400 <- xihat



boxplot(xihat50, xihat100, xihat200, xihat400, xlab = "Sample size", ylab = "", names = c("50", "100", "200", "400"))  
abline(h=xi)

deltahatt50 <- deltahat
deltahat100 <- deltahat
deltahat200 <- deltahat
deltahat400 <- deltahat


boxplot(deltahatt50, deltahat100, deltahat200, deltahat400, xlab = "Sample size", ylab = "", names = c("50", "100", "200", "400"))
abline(h=delta)





















################################################# Weibull
all.equal(pweibull(x, 2.5, pi, lower.tail = FALSE, log.p = TRUE),
          -(x/pi)^2.5, tolerance = 1e-15)
all.equal(qweibull(x/11, shape = 1, scale = pi), qexp(x/11, rate = 1/pi))

x<-rweibull(100, 2, scale = 6.5)
x
hist(x, probability = T)
curve(dweibull(x, 2, scale = 6.5), add=TRUE, col="blue")

#_______________________________________________________
