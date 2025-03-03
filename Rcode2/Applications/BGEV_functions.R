
#retirar d do final do nome das funções

################################################################################
# FUNCTION:              Bimodal GEV distribution proposed in Cira EG Otiniano,
#                        Bianca S Paiva, Roberto Vila and Marcelo Bourguignon (2021)
#  dbgevd                Density for the bimodal generalized extreme value distribution.
#  qbgevd                Quantile function for the bimodal GEV distribution. 
#  rbgevd                Random generation for the bimodal GEV distribution.
#  likbgev               maximum likelihood (ML) estimators for the parameters of a BGEV
#                        distribution
#  ebgevd                Estimate the parameters of a BGEV and optionally construct 
#                        a confidence interval for the parameters.
################################################################################
library(EnvStats)
library(evd)

dbgevd <- function(y, mu, sigma, xi, delta){ 
  # Description:
  #Compute the density for the  bimodal generalized extreme value distribution.
  #Reference: Cira EG Otiniano et al (2021). A Bimodal Model for Extremes Data.
  #1Department of Statistics, University of Bras´ılia, Darcy Ribeiro,
  #Brasilia, 70910-900, DF, Brazil.
  #Department of Statistics, Federal University of Rio Grande do
  # Norte, Natal, 59078-970, RN, Brazil.
  #   Parameters: y in R; mu in R; sigma > 0; xi in R ;  delta > -1;
  
  # FUNCTION:
  
  # Error treatment of input parameters
  if(sigma <= 0  || delta <= -1 )
    stop("Failed to verify condition:
           sigma <= 0  || delta <= -1")
  
  # Compute auxiliary variables:
  Ti      <- (y-mu)*(abs(y-mu)^delta)
  derivate_T <- (delta + 1)*(abs(y-mu)^delta)
  # Compute density points
  pdf    <- dgev(Ti, loc=0, scale=sigma, shape=xi)*derivate_T
  # Return Value
  return(pdf)
}

dbgevd(c(.1,2),mu=1, sigma=10, xi=0.3, delta=2)

qbgevd   <- function(p, mu, sigma, xi, delta){
  # Description:
  #   Compute the quantile for the 
  #   Bimodal GEV distribution.
  #   Parameters: p in [0;1];  mu in R; sigma > 0; xi in R ;  delta > -1;
  
  # FUNCTION:
  
  # Error treatment of input parameters
  if(sigma <= 0  || delta <= -1 )
    stop("Failed to verify condition:
           sigma <= 0  || delta <= -1")
  
  # Compute distribution points according to their sign
  quantile <- sign(qgevd(p, 0, sigma, xi))*(abs(qgevd(p, 0, sigma, xi)))^(1/(delta + 1)) + mu
  # Return Value
  return(quantile)
}

rbgevd <- function(n, mu, sigma, xi, delta){
  # Description:
  #   random generator for the 
  #   Bimodal GEV distribution.
  #   Parameters: p in [0;1];  mu in R; sigma > 0; xi in R ;  delta > -1;
  
  # FUNCTION:
  
  # Error treatment of input parameters
  if(sigma <= 0  || delta <= -1 )
    stop("Failed to verify condition:
           sigma <= 0  || delta <= -1")
  # Compute auxiliary variables:
  U <- runif(n)
  # Compute random numbers
  rnumber <- qbgevd(U, mu, sigma, xi, delta)
  # Return Value
  return(rnumber)
}

likbgev <- function(theta,y){
  # Description:
  #  maximum likelihood (ML) estimators for the parameters of a BGEV distribution
  #   Parameters: y in R;  theta: vector with mu, sigma, xi and delta, respectively.
  #   mu in R; sigma > 0; xi in R ;  delta > -1;
  
  # FUNCTION:
  
  # Parameters:
  mu      <- theta[1]
  sigma   <- theta[2] 
  xi      <- theta[3]
  delta   <- theta[4]
  
  # Error treatment of input parameters
  if(length(theta)!=4){
  stop("vector of parameters needs to be of length 4.")}
  if(sigma <= 0  || delta <= -1 ){
    stop("Failed to verify condition:
           sigma <= 0  || delta <= -1")}
  
  # Compute auxiliary variables:
  T      <- (y-mu)*(abs(y-mu)^delta)
  derivate_T <- (delta + 1)*(abs(y-mu)^delta)
  # Compute density points
  dbgevd <- dgev(T, mu, sigma, xi)*derivate_T
  # Log:
  logl <- sum(log(dbgevd(y,mu, sigma, xi, delta)))
  # Return negative Value for maximization
  return(-logl)
}


################################
#Examples
# Density of a  bimodal generalized extreme value distribution with 
# mu=1, sigma=10, xi=0.3, delta=2){ 

dbgevd(.5,mu=1, sigma=10, xi=0.3, delta=2)
#[1] 0.02748573

# The 90'th percentile of a bimodal generalized extreme value distribution
qbgevd(.9,mu=1, sigma=10, xi=0.3, delta=2)
#[1] 3.538773

# Random sample of 4 observations from a bimodal generalized extreme value 
# distribution with mu=1, sigma=10, xi=0.3 and delta=2. 
# (Note: the call to set.seed simply allows you to reproduce this example.)

set.seed(20) 
rbgevd(4,mu=1, sigma=10, xi=0.3, delta=2) 
# [1]  3.4788506  3.2238194 -0.3633905  2.6166901


pbgevd <- function(y, mu, sigma, xi, delta){ 
  #Distribution Function
  # FUNCTION:
  
  # Error treatment of input parameters
  if(sigma <= 0  || delta <= -1 )
    stop("Failed to verify condition:
           sigma <= 0  || delta <= -1")
  
  # Compute auxiliary variables:
  Ti      <- (y-mu)*(abs(y-mu)^delta)
  # Compute 
  cdf    <- pgev(Ti, loc=0, scale=sigma, shape=xi)
  # Return Value
  return(cdf)
}


n       <- 10^3;mu<- 0
sigma   <- 2;xi<- 0.5
delta   <- 1

Z <- rbgevd(n, mu, sigma, xi, delta)

hist(Z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
x <- seq(min(Z), max(Z), 0.01)
hist(Z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
lines(x,dbgevd(x, mu, sigma, xi, delta),lty=1,lwd=2)

starts <- c(mu,sigma, xi, delta)

test <-optim(par= starts, fn = likbgev, y=Z, method="L-BFGS-B",lower = c(0,0.2,0,0))

lik1 <- function(theta, y){
  
  par_len <- length(theta)
  mu      <- theta[1]
  sigma   <- ifelse(theta[2]>0,theta[2],1^-100)
  xi      <- theta[3]
  delta   <- theta[4]
  
  Tlinha <- (delta + 1)*(abs(y-mu)^delta)#*sigma
  T      <- (y-mu)*(abs(y-mu)^delta)#*sigma
  dbgevd_l <- dgevd(T, mu, sigma, xi)*Tlinha
  
  logl <- sum(log(dbgevd(y,mu, sigma, xi, delta)))####
  return(-logl)
}

test <- optim(par = starts, fn = lik1, y=Z, method = 'BFGS')
starts
y <- Z
Tlinha <- (delta + 1)*(abs(y-mu)^delta)#*sigma
T      <- (y-mu)*(abs(y-mu)^delta)#*sigma
dbgevd_l <- dgevd(T, mu, sigma, xi)*Tlinha
sum(log(dbgevd_l))
sum(log(dgevd(T, mu, sigma, xi)*Tlinha))
