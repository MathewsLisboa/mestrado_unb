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
test <-optim(par= starts, fn = likbgev, y=Z, method="BFGS")

test$par


# library(VGAM)
# library(fExtremes)


fit_gev <- gevFit(MI, type='mle')


summary(fit_gev)

########## Caso Petrbras ####
Z <- c(m,M)
Z1 <- Z[Z<=0.5]
hist(Z1,xlim = c(-0.1,0.1),ylim = c(0,10),
     breaks = 40,col = "white ",ylab = "Fitted density",xlab = "Y",main = "")
#plot(density(Z))
x <- seq(min(Z1), max(Z1), 0.01)
lines(x,dbgev(x, mu=-0.001251948  , sigma=0.009669227 , xi=0.047710727, delta=0.483654312 ),lty=1,lwd=2)
starts <- c(mu=0,sigma=0.5, xi=0.01, delta=1)
test <- optim(par = starts,fn =likbgev, y=Z1, method = 'BFGS',hessian = T)
test$par

##mean(Z1)
##var(Z1)


######### Caso Temperatura minima e máxima dos máximos #############
Z <- c(mi,MI)
hist(Z,probability = T)
x <- seq(min(Z), max(Z), 0.01)
lines(x,dbgev(x, mu=29.0267367, sigma=42.8933221, xi=-0.2577564, delta=0.9821110),lty=1,lwd=2)
starts <- c(mu=0,sigma=1.001,xi=2,delta=5.0001)
test <- optim(par=starts,fn=likbgev,y=Z,method = 'BFGS',hessian = T)
test$par

### refazendo estimação
#Z <- c(mi,MI)
hist(Z,probability = T)
x <- seq(min(Z), max(Z), 0.01)
lines(x,dbgev(x, mu=mean(Z), sigma=var(Z), xi=-0.4184734, delta=0.5),lty=1,lwd=2)
starts <- c(mu=mean(Z),sigma=var(Z),xi=-0.4,delta=0.5)
test <- optim(par=starts,fn=likbgev,y=Z,method = 'BFGS',hessian = T)
test$par

#plot(x,dbgev(x, mu=29.0267367, sigma=42.8933221, xi=-0.2577564, delta=0.9821110))

bgev.support(mean(Z),var(Z),-0.4184734,0.8)
min(Z)
max(Z)
mean(Z)
var(Z)

fit <- fExtremes::gevFit(Z, type='mle')
fit

############## Temperatura do Orvalho minima ###############

Z2 <- mi
hist(Z2, 20,probability = T)
x <- seq(min(Z2), max(Z2), 0.01)
lines(x,dbgev(x, mu=9 , sigma=11 , xi=-0.2,delta=0.25),lty=1,lwd=2)

starts <- c(mu=9,sigma=11 , xi=-0.2, delta=0.25)
test <- optim(par=starts,fn=likbgev,y=Z2,method = 'BFGS',hessian = T)
test$par


hist(Z2,20,probability = T)
x <- seq(min(Z2), max(Z2), 0.01)
lines(x, dbgev(x,mu=test$par[1],sigma=test$par[2], xi = test$par[3], delta= test$par[4]))
lines(x,dgev(x, loc=8.9407448, scale = 5.7027691, shape = -0.6542575), col='red')



starts <- c(mu=mean(Z2),sigma=var(Z2) , xi=fit@fit$par.ests[1], delta=0.5)
test <- optim(par=starts,fn=likbgev,y=Z2,method = 'BFGS',hessian = T)
test$par

bgev.support(mean(Z2),var(Z2),-0.6542575,0.5)
max(Z2)
min(Z2)
mean(Z2)
var(Z2)

fit <- fExtremes::gevFit(Z2, type = 'mle')



# invR<-solve(v1$hessian)
# variancia<-diag(invR)
# epp<-sqrt(variancia)

