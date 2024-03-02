require(tidyverse)
require(stats) # for spl
###############################################################
library(evd)
library(EnvStats)
library(bgumbel)

################################
#Examples
# Density of a  bimodal generalized extreme value distribution with 
# mu=1, sigma=10, xi=0.3, delta=2){ 

dbgev(.5,mu=1, sigma=10, xi=0.3, delta=2)

#[1] 0.02748573
# The 90'th percentile of a bimodal generalized extreme value distribution

sigma <- 1
xi <- 0
delta <- 3
mu <- 2
y <- 0.3



qbgev(.9,mu=1, sigma=10, xi=xi, delta=2)
qbgevd(.9,mu=1, sigma=10, xi=-xi, delta=2)
qgev(0.9, loc = 0, sigma, xi)^(1/(delta+1))+mu
### xi==0
sign(-sigma*log(log(1/y)))*(abs(-sigma*log(log(1/y)))^(1/(delta+1))) + mu
## xi != 0
sign(sigma/xi*((log(1/y)**(-xi))-1))*abs(sigma/xi*((log(1/y)**(-xi))-1))^(1/(delta+1)) +mu


##log(1/y) == -1*log(y)
### caso curioso 

## 3.538773

# Random sample of 4 observations from a bimodal generalized extreme value 
# distribution with mu=1, sigma=10, xi=0.3 and delta=2. 
# (Note: the call to set.seed simply allows you to reproduce this example.)

set.seed(20)
rbgev(4,mu=1, sigma=10, xi=0.3, delta=2) 
rbgevd(4,mu=1, sigma=10, xi=0.3, delta=2) 
## 3.4788506  3.2238194 -0.3633905  2.6166901



################################
#Examples

#                parameter
#Estimative       mu                                     
#Ponctual       1.02279116583337                       
#Conf. Interval ( 1.01168448475556 ; 1.03389784691119 )
#parameter
#Estimative       sigma                                  
#Ponctual       16.9963296503858                       
#Conf. Interval ( 16.3867158696069 ; 17.6059434311648 )
#parameter
#Estimative       xi                                       
#Ponctual       0.139581984753769                        
#Conf. Interval ( 0.124364259639566 ; 0.154799709867972 )
#parameter
#Estimative       delta                                  
#Ponctual       2.49602823332784                       
#Conf. Interval ( 2.45331666498226 ; 2.53873980167342 )


##### Tentativa de Estimação #######

n       <- 10^3
mu      <- 0
sigma   <- 5
xi      <- -0.5
delta   <- 2


Z <- rbgev(n, mu, sigma, xi, delta)
hist(Z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
x <- seq(min(Z), max(Z), 0.01)
hist(Z,freq = FALSE,main="",xlab="x",ylab="Density",lwd=2)
lines(x,dbgev(x, mu, sigma, xi, delta),lty=1,lwd=2)

###########################
starts <- c(mu,sigma, xi, delta)
test <-optim(par= starts, fn = likbgev, y=Z, method="BFGS",)
#test$par
test$par


##### Testando Monte Carlo #######


R       <- 10^2
n       <- 10^3
mu      <- 1
sigma   <- 2
xi      <- 
delta   <- 2

muhat      = rep(NA, times = R)
sigmahat   = rep(NA, times = R)
xihat      = rep(NA, times = R)
deltahat   = rep(NA, times = R)

starts <- c(mu, sigma, xi, delta)

for(k in (1:R)){
  Z <- rbgev(n, mu, sigma, xi, delta)
  
  esti <- optim(par= starts, fn = likbgev, y=Z, method="BFGS")
  
  muhat[k]       <- esti$par[1]
  sigmahat[k]    <- esti$par[2]
  xihat[k]       <- esti$par[3]
  deltahat[k]    <- esti$par[4]
}

warnings() ## yep produced NA porém ainda assim está ok 

mean(muhat)
mean(sigmahat)
mean(xihat)
mean(deltahat)


## isso aqui ficou muito bom apesar dos warnings