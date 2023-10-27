
curve(dgev(x,0,1,-1.5), xlim = c(-3,suport_gev(-1,0,1)), xlab='x', ylab='f(x)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
  cex.axis=0.5)


curve(dbgev(x,0,1,-1,0), xlim = c(-3,1), xlab='x', ylab='h(x)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)      

F_ge <- function(y,mu=0,sigma=1,xi=-1){
  resultado <- c()
  for(i in 1:length(y)){
    if(y[i]>suport_gev(xi,mu, sigma)){
      res <- exp(- (1 + xi*((y[i]-mu)/sigma))^(-1/xi))
    }else(res <- 1)
    resultado[i] <- res
  }
return(resultado)
}


F_ge2 <- function(y,mu=0,sigma=1,xi=-1){
  exp(- (1 + xi*((y-mu)/sigma))^(-1/xi))
  
}

suport_gev <-function(xi, mu,sigma){
  ((-1*sigma)/xi)+mu
}

suport_gev(-1,0,1)

S_gev <- function(y,mu=0,sigma=1, xi=-1){
  1 - F_ge(y, mu, sigma, xi)
}

S_gev2 <- function(y,mu=0,sigma=1, xi=-1){
  1 - F_ge2(y, mu, sigma, xi)
}

h_gev <- function(y, mu = 0, sigma=1, xi=-1){
  dgevd(y,mu, sigma, xi) / S_gev(y, mu,sigma,xi)
}

h_gev2 <- function(y, mu=0, sigma=1, xi=-1){
  f <- dgevd(y, mu, sigma, xi)
  s <- S_gev2(y, mu, sigma, xi)
  res <- f/s
  return(res)
}


curve(F_ge(x,0,1,-1), xlim = c(-3,3), xlab='x', ylab='h(x)', ylim=c(0,2), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)


curve(F_ge2(x,0,1,-1), xlim = c(-3,suport_gev(-1,0,1)), xlab='x', ylab='h(x)', ylim=c(0,2), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)


curve(S_gev(x,0,1,-1), xlim = c(-3,suport_gev(-1,0,1)), xlab='x', ylab='h(x)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)


curve(S_gev2(x,0,1,-1), xlim = c(-3,3), xlab='x', ylab='h(x)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)


curve(h_gev(x,0,1,-1), xlim = c(-3,3), xlab='x', ylab='h(x)', ylim=c(0,2), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)

curve(h_gev2(x,0,1,-1)*S_gev2(x,0,1,-1), xlim = c(-3,suport_gev(-1,0,1)), xlab='x', ylab='h(x)', ylim=c(0,2), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)


curve(dgev(x,0,1,1), xlim = c(-3,3), xlab='x', ylab='h(x)', ylim=c(0,2), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)