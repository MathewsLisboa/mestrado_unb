require(stats) # for spl
###############################################################
library(evd)
library(EnvStats)
library(bgumbel)

###################
#Funções da BGEV
###################

dbgevd <- function(y, mu, sigma, xi, delta){
  T      <- sigma*y*(abs(y)^delta)#sigma*(y-mu)*(abs(y-mu)^delta)
  Tlinha <- sigma*(delta + 1)*(abs(y)^delta)#(delta + 1)*(abs(y-mu)^delta)
  pdf    <- dgevd(T, mu, sigma, xi)*Tlinha#dgevd(T, 0, sigma, xi)*Tlinha
  return(pdf)
}


qbgevd   <- function(p, mu, sigma, xi, delta){
  quantile <- sign(qgevd(p, mu, sigma, xi))*(abs(qgevd(p, mu, sigma, xi))/sigma)^(1/(delta + 1))
  return(quantile)
}
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

###################
#Funções da Weibull
###################

# Função adicionando um novo parâmetro m: (1.2.4)
fd_WBm <- function(y,beta,lambda,delta,mu,m){
  #m =  (-mu)^(1/(delta+1))
  f <- fd_WB(x=(y-m),beta,lambda,delta,mu)
  return(f)
}

# função quantil: com m=0
fquantil <- function(y,beta=1,lambda=1,delta=0,mu=0){
  m = 0#(-mu)^(1/(1+delta)) 
  cond <- (mu + lambda*((-log(1-y))^(1/beta)))
  a <- (mu + lambda*((-log(1-y))^(1/beta)))^(1/(delta+1)) -m
  b <- -(-mu -lambda*((-log(1-y))^(1/beta)))^(1/(delta+1)) -m 
  
  f <- ifelse(cond>=0,a,b)
  return(f)
}


################################################################
#Exemplo 1 beta=1,lambda=2,delta=3,mu=-.55,m=0
#Chute inicial

curve(fd_WBm(x,beta=1,lambda=2,delta=3,mu=-.55,m=0), xlim = c(-2, 3), ylim = c(0, 1.3), lwd = 2.5, col = "red")
curve(dbgevd(x, mu =.12, sigma = 3, xi=-3, delta =8), xlim = c(-5, 10), ylim = c(0, 1), lwd = 2.5, add = T, col = "blue")
legend("topright", c("BWeibull", "BGEV"), fill=c("red", "blue"))

#estimação dos parâmetros
R <- 100#repetições
n<-100

muhat      = rep(NA, times = R)
sigmahat   = rep(NA, times = R)
xihat      = rep(NA, times = R)
deltahat   = rep(NA, times = R)

lik1 <- function(theta, y){
  par_len <- length(theta)
  mu      <- theta[1]
  sigma   <- 3 #fixar devido ao problema com a função optim
  xi      <- theta[3]
  delta   <- theta[4]
  
  T      <- sigma*y*(abs(y)^delta)
  Tlinha <- sigma*(delta + 1)*(abs(y)^delta)
  
  dbgevd <- dgevd(T, mu, sigma, xi)*Tlinha
  
  logl <- sum(log(dbgevd(y,mu, sigma, xi, delta)))
  return(-logl)
}

starts <- c(0.12, 3, -3, 8)#parâmetros da BGEV
for(k in (1:R)){
  U <- runif(n)
  Z <- fquantil(U,beta=1,lambda=2,delta=3,mu=-.55)
  while(show_condition(suppressWarnings(optim(par= starts, fn = lik1, y=Z, method="BFGS")))[1]=="error"){
    Z <- fquantil(U,beta=2,lambda=2,delta=2,mu=-1)
  }
  
  esti <- optim(par= starts, fn = lik1, y=Z, method="BFGS")
  
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}
estimate_xi     <-   mean(xihat,na.rm = TRUE)
estimate_mu     <-   mean(muhat,na.rm=TRUE)
estimate_sigma  <-   mean(sigmahat,na.rm=TRUE)
estimate_delta  <-   mean(deltahat,na.rm=TRUE)


#Comparando uma amostra da Weibull com a distribuição da BGEV
#amostra de n=500
x <- seq(-10, 10, 0.01)
U <- runif(500)
Z <- fquantil(U,beta=1,lambda=2,delta=3,mu=-.55)

hist(Z,freq = FALSE, breaks = 50,main="",xlab="x",ylab="Densidade",lwd=2, col = 'white', border = "black", xlim = c(-2, 3), ylim = c(0, 1.3))
lines(x,fd_WBm(x,1,2,3,-.55,0),col="purple",lwd = 2.5)
lines(x,dbgevd(x, estimate_mu,estimate_sigma, estimate_xi, estimate_delta),col="black",lwd = 2.5)
legend("topright", c("BWeibull", "BGEV"), fill=c("purple", "black"))


################################################################
#Exemplo 2 beta=2,lambda=2,delta=2,mu=-1,m=0
#Chute inicial

curve(fd_WBm(x,beta=2,lambda=2,delta=2,mu=-1,m=0), xlim = c(-2, 5), ylim = c(0, 1.5), lwd = 2.5, col = "red")
curve(dbgevd(x, mu =1, sigma = 20, xi=-2, delta =9), xlim = c(-5, 10), ylim = c(0, 1), lwd = 2.5, add = T, col = "blue")
legend("topright", c("BWeibull", "BGEV"), fill=c("red", "blue"))


#estimação dos parâmetros
R <- 100
n<-100

muhat      = rep(NA, times = R)
sigmahat   = rep(NA, times = R)
xihat      = rep(NA, times = R)
deltahat   = rep(NA, times = R)

lik1 <- function(theta, y){
  par_len <- length(theta)
  mu      <- theta[1]
  sigma   <- 20 #fixar devido ao problema com a função optim
  xi      <- theta[3]
  delta   <- theta[4]
  
  T      <- sigma*y*(abs(y)^delta)
  Tlinha <- sigma*(delta + 1)*(abs(y)^delta)
  
  dbgevd <- dgevd(T, mu, sigma, xi)*Tlinha
  
  logl <- sum(log(dbgevd(y,mu, sigma, xi, delta)))
  return(-logl)
}

starts <- c(1, 20, -2, 9)#parâmetros da BGEV
for(k in (1:R)){
  U <- runif(n)
  Z <- fquantil(U,beta=1,lambda=2,delta=3,mu=-.55)
  while(show_condition(suppressWarnings(optim(par= starts, fn = lik1, y=Z, method="BFGS")))[1]=="error"){
    Z <- fquantil(U,beta=2,lambda=2,delta=2,mu=-1)
  }
  
  esti <- optim(par= starts, fn = lik1, y=Z, method="BFGS")
  
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]#não está sendo estimado
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}
estimate_xi     <-   mean(xihat,na.rm = TRUE)
estimate_mu     <-   mean(muhat,na.rm=TRUE)
estimate_sigma  <-   mean(sigmahat,na.rm=TRUE)
estimate_delta  <-   mean(deltahat,na.rm=TRUE)


#Comparando uma amostra da Weibull com a distribuição da BGEV
#amostra de n=500
x <- seq(-10, 10, 0.01)
U <- runif(500)
Z <- fquantil(U,beta=1,lambda=2,delta=3,mu=-.55)

hist(Z,freq = FALSE, breaks = 50,main="",xlab="x",ylab="Densidade",lwd=2, col = 'white', border = "black", xlim = c(-2, 3), ylim = c(0, 1.3))
lines(x,fd_WBm(x,1,2,3,-.55,0),col="purple",lwd = 2.5)
lines(x,dbgevd(x, estimate_mu,estimate_sigma, estimate_xi, estimate_delta),col="black",lwd = 2.5)
legend("topright", c("BWeibull", "BGEV"), fill=c("purple", "black"))

