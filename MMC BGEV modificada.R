require(stats) # for spl
###############################################################
library(evd)
library(EnvStats)
library(bgumbel)

###################
#iid
###################

dbgevd <- function(y, mu, sigma, xi, delta){
  T      <- (y-mu)*(abs(y-mu)^delta)#*sigma
  Tlinha <- (delta + 1)*(abs(y-mu)^delta)#*sigma
  pdf    <- dgevd(T, 0, scale=sigma, shape=xi)*Tlinha
  return(pdf)
}

#########################################################################http://127.0.0.1:16955/graphics/plot_zoom_png?width=1200&height=900
## FIGURAS variando mu
# mu= -2, -1, 0,1 e xi=1 (vale para maior), delta=1

curve(dbgevd(x, mu = -2, sigma = 1, xi=0, delta = 1), xlim = c(-3, 3),
      ylim = c(0, 1), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = -1, sigma = 1, xi=0, delta = 1), xlim = c(-2.5, 2.5), 
      ylim = c(0, 1), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=0, delta = 1), xlim = c(-2.5, 2.5), 
      ylim = c(0, 1), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 1, sigma = 1, xi=0, delta = 1), xlim = c(-2.5, 2.5),
      ylim = c(0, 1), lwd = 1.5, add = T, col = "red")
legend(x=-3, y=1, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                              expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n")

# mu= -2, -1, 0,1 e xi=-1 (vale para maior), delta=1
curve(dbgevd(x, mu = -2, sigma = 1, xi=0, delta = 1), xlim = c(-3, 3),
      ylim = c(0, 1.7), xlab=expression(" "), ylab = expression(" "),lwd = 1.5)
curve(dbgevd(x, mu = -1, sigma = 1, xi=0, delta = 1), xlim = c(-2.5, 2.5), 
      ylim = c(0, 2), lwd = 1, add = T, col = "green")
curve(dbgevd(x, mu = 0, sigma = 1, xi=0, delta = 1), xlim = c(-2.5, 2.5), 
      ylim = c(0, 2), lwd = 1, add = T, col = "blue")
curve(dbgevd(x, mu = 1, sigma = 1, xi=0, delta = 1), xlim = c(-2.5, 2.5),
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
curve(dbgevd(x, mu = 0, sigma = 1, xi=-0.25, delta = 2), xlim = c(-2, 3),
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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


###############################################################################

#QUANTIL
qbgevd   <- function(p, mu, sigma, xi, delta){
  quantile <- sign(qgevd(p, 0, sigma, xi))*(abs(qgevd(p, 0, sigma, xi)))^(1/(delta + 1)) + mu
  return(quantile)
}

# xi <- 0
# sigma <- 10
# ## qgevd essa função equivale a essa expressão gigante aqui em que 
# qbgevd(0.9, mu,sigma,xi,delta)
y <- 0.9
sign(-sigma*log(-log(y)))*(abs(-sigma*log(log(1/y)))^(1/(delta+1))) + mu
## essa é a função quantil da bigeb

rbgevd <- function(n, mu, sigma, xi, delta){
  U <- runif(n)
  rnumber <- qbgevd(U, mu, sigma, xi, delta)
  return(rnumber)
}


n       <- 10^3
mu      <- 0
sigma   <- 1
xi      <- 0.5
delta   <- 4

Z <- rbgevd(n, mu, sigma, xi, delta)

x <- seq(min(Z), max(Z), 0.01)
hist(Z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
lines(x,dbgevd(x, mu, sigma, xi, delta),lty=1,lwd=2)###########################

lik1 <- function(theta, y){
  
  par_len <- length(theta)
  mu      <- theta[1]
  sigma   <- ifelse(theta[2]>0,theta[2],1^-10)
  xi      <- theta[3]
  delta   <- theta[4]
 
  Tlinha <- (delta + 1)*(abs(y-mu)^delta)#*sigma
  T      <- (y-mu)*(abs(y-mu)^delta)#*sigma
  dbgevd_l <- dgevd(T, mu, sigma, xi)*Tlinha
  
  logl <- sum(log(dbgevd_l))####
  return(-logl)
}

n       <- 10^4
mu      <- 0
sigma   <- 2
xi      <- -1
delta   <- 1


Z <- rbgevd(n, mu, sigma, xi, delta)
hist(Z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
x <- seq(min(Z), max(Z), 0.01)
hist(Z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
lines(x,dbgevd(x, mu, sigma, xi, delta),lty=1,lwd=2)

###########################

starts <- c(mu,sigma, xi, delta)

test <-optim(par= starts, fn = lik1, y=Z, method="BFGS",)
#test$par
test$par

###################
#Monte Carlo
###################
#mu = 1
#sigma = 10
#xi = -0.5 and 0.5
#delta -0.5, 0, 0.3, 1, 2

R       <- 10^3
n       <- 10^2
mu      <- 0
sigma   <- 2
xi      <- -1
delta   <- 1

muhat      = rep(NA, times = R)
sigmahat   = rep(NA, times = R)
xihat      = rep(NA, times = R)
deltahat   = rep(NA, times = R)

starts <- c(mu, sigma, xi, delta)

for(k in (1:R)){
  Z <- rdgevd(n, mu, sigma, xi, delta)
  
  esti <- optim(par= starts, fn = lik1, y=Z, method="BFGS")
  
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}

warnings()
################################################################################################

show_condition <- function(code) {
  tryCatch(code,
           error = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message"
  )}
show_condition(optim(par= starts, fn = lik1, y=Z, method="BFGS"))=="error"

muhat      = rep(NA, times = R)
sigmahat   = rep(NA, times = R)
xihat      = rep(NA, times = R)
deltahat   = rep(NA, times = R)

starts <- c(mu, sigma, xi, delta)
starts <- c(1, 1, 1, 1)

for(k in (1:10)){
  Z <- rdgevd(n, mu, sigma, xi, delta)
  while(show_condition(suppressWarnings(optim(par= starts, fn = lik1, y=Z, method="BFGS")))[1]=="error"){
    Z <- rdgevd(n, mu, sigma, xi, delta)
  }
  
  esti <- optim(par= starts, fn = lik1, y=Z, method="BFGS")
  
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}


sum(is.na(muhat))

estimate_xi     <-   mean(xihat,na.rm = TRUE)#0.3104895
estimate_mu     <-   mean(muhat,na.rm=TRUE)#1.014193
estimate_sigma  <-   mean(sigmahat,na.rm=TRUE)#12.24339
estimate_delta  <-   mean(deltahat,na.rm=TRUE) #2.198627

se<-sd(xihat,na.rm = TRUE)

se_xihat <- sqrt(sum((xihat - estimate_xi)^2)/(R-1))

#IC Normal Padrão
alfa=0.95
IC_inf<- estimate_xi-qnorm((1-alfa/2),mean=0,sd=1)*se#0.3014822
IC_sup<-estimate_xi+qnorm((1-alfa/2),mean=0,sd=1)*se#0.3194968

#estimate_xi 
#estimate_mu
#estimate_sigma 
#estimate_delta

#starts

################################
RB_xi    <- (estimate_xi - xi)/xi
RB_mu    <- (estimate_mu - mu)/mu
RB_sigma <- (estimate_sigma - sigma)/sigma
RB_delta <- (estimate_delta - delta)/delta

RB_xi
RB_mu
RB_sigma
RB_delta
#################################
# RB_xi
# [1] 0.03496502
# > RB_mu
# [1] 0.01419334
# > RB_sigma
# [1] 0.2243395
# > RB_delta
# [1] 0.09931359
################################

RMSE_xi    <- sqrt(var(xihat)     +  (estimate_xi - xi)^2)
RMSE_mu    <- sqrt(var(muhat)     +  (estimate_mu - mu)^2)
RMSE_sigma <- sqrt(var(sigmahat)  +  (estimate_sigma - sigma)^2)
RMSE_delta <- sqrt(var(deltahat)  +  (estimate_delta - delta)^2)


RMSE_xi
RMSE_mu
RMSE_sigma
RMSE_delta

######################################
# RMSE_xi 
# [1] 0.1440237
# > RMSE_mu
# [1] 0.1025986
# > RMSE_sigma
# [1] 5.30026
# > RMSE_delta
# [1] 0.4615886
######################################

#teste de robustes
#bgumbel pacote
#install.packages("bgumbel")
library("bgumbel")

curve(pbgumbel(x,mu=-2,sigma=1,delta=1))
curve(dbgumbel(x, mu = -2, sigma = 2, delta = -0.7), xlim = c(-5, 10))

z<-as.vector(rbgumbel(500, mu = -2, sigma = 2, delta = -1))

x <- seq(-5, 15, 0.01)
hist(z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
lines(x,dbgumbel(x,mu = -2, sigma = 2, delta = -1))
box()


curve(dbgumbel(x, mu = -2, sigma = 2, delta = -.8), xlim = c(-5, 10))
curve(dbgevd(x, mu =-.5, sigma = 100, xi=1, delta = 2), xlim = c(-5, 10), lwd = 1, add = T, col = "blue")

R<- 50
n<-100

muhat      = rep(NA, times = R)
sigmahat   = rep(NA, times = R)
xihat      = rep(NA, times = R)
deltahat   = rep(NA, times = R)

starts <- c(-2, 1, 0.5, 1)
starts <- c(mu, sigma, xi, delta)
starts <- c(-.5, 2, 1, 2)

for(k in (1:R)){
  Z <- as.vector(rbgumbel(50, mu = -2, sigma = 2, delta = -1))
  while(show_condition(suppressWarnings(optim(par= starts, fn = lik1, y=Z, method="BFGS")))[1]=="error"){
    Z <- as.vector(rbgumbel(50, mu = -2, sigma = 2, delta = -1))
  }
  
  esti <- optim(par= starts, fn = lik1, y=Z, method="BFGS")
  
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}


estimate_xi     <-   mean(xihat,na.rm = TRUE)#0.3104895
estimate_mu     <-   mean(muhat,na.rm=TRUE)#1.014193
estimate_sigma  <-   mean(sigmahat,na.rm=TRUE)#12.24339
estimate_delta  <-   mean(deltahat,na.rm=TRUE) #2.198627

starts
##########################################
curve(dbgumbel(x, mu = -2, sigma = 1.25, delta = -100), xlim = c(-5, 10))
curve(dbgevd(x, mu =-.5, sigma = 100, xi=-11.5, delta =3 ), xlim = c(-5, 10), lwd = 1, add = T, col = "blue")

R<- 100
n<-100

muhat= rep(NA, times = R)
sigmahat = rep(NA, times = R)
xihat = rep(NA, times = R)
deltahat = rep(NA, times = R)

starts <- c(-.5, 100, -10, 3)
for(k in (1:R)){
  Z <- as.vector(rbgumbel(n, mu = -2, sigma = 1.25, delta = -3))
  while(show_condition(suppressWarnings(optim(par= starts, fn = lik1, y=Z, method="BFGS")))[1]=="error"){
    Z <- as.vector(rbgumbel(n, mu = -2, sigma = 1.25, delta = -3))
  }
  
  esti <- optim(par= starts, fn = lik1, y=Z, method="BFGS")
  
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}


#tentar weibull
estimate_xi     <-   mean(xihat,na.rm = TRUE)#0.3104895
estimate_mu     <-   mean(muhat,na.rm=TRUE)#1.014193
estimate_sigma  <-   mean(sigmahat,na.rm=TRUE)#12.24339
estimate_delta  <-   mean(deltahat,na.rm=TRUE) #2.198627


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



#################################################################################


R       <- 500
n       <- 50
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
  
  U <- runif(n)
  Z <- qbgevd(U, mu, sigma, xi, delta)
  
  
  esti <-  optim(par= starts, fn = lik1, y=Z, method="BFGS")
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}

sum(is.na(muhat))
estimate_xi     <-   mean(xihat,na.rm=T)
estimate_mu     <-   mean(muhat,na.rm=T)
estimate_sigma  <-   mean(sigmahat,na.rm=T)
estimate_delta  <-   mean(deltahat,na.rm=T) 


