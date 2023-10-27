require(stats)

library(evd)
library(EnvStats)
library(bgumbel)
library(tidyverse)

### Gráficos de exemplo 

### como salvar gráficos sem ser ggplot de forma eficiente

setwd('C:\\Users\\Usuario\\Documents\\UNB_mestrado\\Orientação\\projeto_1')

#setwd("D:\\Users\\Mathews\\Documents\\UNB_mestrado\\projeto_1")


png('imagens\\teste3.png', width = 158, height = 103, units = "mm", bg = 'white', res=1080)

curve(drweibull(x,loc = 0,scale = 1,shape = 1), xlim = c(-5,5), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5, cex.lab=1, cex.axis=0.75)
curve(dgumbel(x,loc = 0,scale = 1), xlim = c(-5,5), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5,add = T, col='blue')
curve(dfrechet(x,loc = 0,scale = 1,shape = 1), xlim = c(-5,5), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=2, y=1, legend = c('Weibull', 'Gumbel', 'Frechet'),
       lwd = 1, col = c("black", "blue","red"), cex=0.75, bty = "n", y.intersp=1)
dev.off()


png('imagens\\gev.png',width = 158, height = 103, units = "mm", bg = 'white', res=1080)

curve(dgev(x,-1,1,-1), xlim = c(-5,5), xlab='x', ylab='f(x)', ylim=c(0,1), lwd=1.5, cex.lab=1, cex.axis=0.75)
curve(dgev(x,0,1,0), xlim = c(-5,5), xlab='x', ylab='f(x)', ylim=c(0,1), lwd=1.5,add = T, col='blue')
curve(dgev(x,1,1,1), xlim = c(-5,5), xlab='x', ylab='f(x)', ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=2, y=1, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= 0")),
                            expression(paste(xi, "= 1"))),
       lwd = 1, col = c("black", "blue","red"), cex=0.75, bty = "n", y.intersp=1)
dev.off()


#### funções

### Suporte

suport <- function(sigma, xi, delta, mu){
 round(sign(-sigma/xi)*abs(sigma/xi)^(1/(delta+1)),4) 
}


### BGEV 

dbgevd <- function(y, mu, sigma, xi, delta){
  T_mod    <- (y-mu)*(abs(y-mu)^delta) #*sigma
  Tlinha <- (delta + 1)*(abs(y-mu)^delta) #*sigma
  pdf<- dgev(T_mod,loc = 0, scale=sigma, shape=xi)*Tlinha
  return(pdf)
}

F_Bgev <- function(y,mu=0,sigma=1, xi=0, delta=1){
  resultado <- c()
  for(i in 1:length(y)){
    T_mod    <- (y[i]-mu)*(abs(y[i]-mu)^delta)
    if(xi == 0){
      res <- exp(-exp(-T_mod/sigma))
    
    }else if(xi<0 & y[i]<= suport(sigma,xi,delta,mu)){
      res <- exp( - ( 1 + xi*(T_mod/sigma))^(-1/xi))
    }else if(xi<0 & y[i]> suport(sigma,xi,delta,mu)){
     res <- 1
   }else if(xi>0 & y[i]>= suport(sigma,xi,delta,mu) ){
      res <- exp( - ( 1 + xi*(T_mod/sigma))^(-1/xi))
   }else{res <- 0
    }
    resultado[i] <- res 
  }
  
  return(resultado)
}

S_Bgev <- function(y,mu,sigma,xi,delta){
  1 - F_Bgev(y,mu,sigma, xi, delta)
}


h_Bgev <- function(y,mu,sigma,xi,delta){ 
  resultado <-c() 
  for(i in 1:length(y)){
    if(S_Bgev(y[i],mu,sigma,xi,delta) != 0 ){
      res <- dbgevd(y[i], mu, sigma, xi, delta) / S_Bgev(y[i],mu,sigma,xi,delta)
    
    }else{res <- 10^5}
      
    resultado[i] <- res
  }
  return(resultado)
}  


### função quantile ####

qbgevd   <- function(p, mu, sigma, xi, delta){
  quantile <- sign(qgev(p, mu, sigma, xi))*(abs(qgev(p, 0, sigma, xi)))^(1/(delta + 1)) + mu
  return(quantile)
}

### gerador pseudo aleatório de função 

rdgevd <- function(n, mu, sigma, xi, delta){
  U <- runif(n)
  rnumber <- qbgevd(U, mu, sigma, xi, delta)
  return(rnumber)
}



### função de log-likehood 

lik_l_meu <- function(theta, y){
  par_len <- length(theta)
  mu      <- theta[1]
  sigma   <- theta[2]
  xi      <- theta[3]
  delta   <- theta[4]
  
  logl <- sum(log(dbgevd(y,mu, sigma, xi, delta)))####
  return(-logl)
}










#### tentando fazer os gráficos de maneira que fique descente... 

#### mundando a média  ######

png('imagens\\bgev_muda_mu.png', width = 178 , height=124 , units = 'mm', bg = 'white', res=1080)
par(mai=c(0.35,0.7,0.3,0.1),mfrow=c(2,2))

curve(dbgevd(x,mu=-2,sigma=1, delta=1, xi=0), xlim = c(-3,3), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(dbgevd(x,mu=-1,sigma=1, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(dbgevd(x,mu=0, sigma=1, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(dbgevd(x,mu=1, sigma=1, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                             expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

curve(F_Bgev(x,mu=-2,sigma=1, delta=1,xi=-0), xlim = c(-3,3), xlab='y', ylab='F(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(F_Bgev(x,mu=-1,sigma=1, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(F_Bgev(x,mu=0, sigma=1, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(F_Bgev(x,mu=1, sigma=1, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')

legend(x=-3, y=1, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                             expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(S_Bgev(x,mu=-2,sigma=1, delta=1,xi = 0), xlim = c(-3,3), xlab='y', ylab='S(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(S_Bgev(x,mu=-1,sigma=1, delta=1,xi = 0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(S_Bgev(x,mu=0, sigma=1, delta=1,xi = 0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(S_Bgev(x,mu=1, sigma=1, delta=1,xi = 0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=1.7, y=1, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                             expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(h_Bgev(x,mu=-2,sigma=1, delta=1,xi=0), xlim = c(-3,3), xlab='y', ylab='h(y)',ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(h_Bgev(x,mu=-1,sigma=1, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(h_Bgev(x,mu=0, sigma=1, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(h_Bgev(x,mu=1, sigma=1, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=1.7, y=1, legend = c(expression(paste(mu, "= -2")), expression(paste(mu, "= -1")),
                             expression(paste(mu, "= 0")), expression(paste(mu, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

dev.off()
### mundando o sigma #######

png('imagens\\bgev_muda_sigma.png', width = 178 , height=124  , units = 'mm', bg = 'white', res=1080)

par(mai=c(0.35,0.7,0.3,0.1),mfrow=c(2,2))

curve(dbgevd(x,mu=0,sigma=1, delta=1, xi=0), xlim = c(-3,3), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(dbgevd(x,mu=0,sigma=1.5, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(dbgevd(x,mu=0, sigma=2, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(dbgevd(x,mu=0, sigma=3, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')

legend(x=-3, y=1, legend = c(expression(paste(sigma, "= 1")), expression(paste(sigma, "= 1.5")),
                             expression(paste(sigma, "= 2")), expression(paste(sigma, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(F_Bgev(x,mu=0,sigma=1, delta=1,xi=-0), xlim = c(-3,3), xlab='y', ylab='F(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(F_Bgev(x,mu=0,sigma=1.5, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(F_Bgev(x,mu=0, sigma=2, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(F_Bgev(x,mu=0, sigma=3, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')

legend(x=-3, y=1, legend = c(expression(paste(sigma, "= 1")), expression(paste(sigma, "= 1.5")),
                             expression(paste(sigma, "= 2")), expression(paste(sigma, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(S_Bgev(x,mu=0,sigma=1, delta=1,xi=-0), xlim = c(-3,3), xlab='y', ylab='S(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(S_Bgev(x,mu=0,sigma=1.5, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(S_Bgev(x,mu=0, sigma=2, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(S_Bgev(x,mu=0, sigma=3, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=1.5, y=1, legend = c(expression(paste(sigma, "= 1")), expression(paste(sigma, "= 1.5")),
                             expression(paste(sigma, "= 2")), expression(paste(sigma, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(h_Bgev(x,mu=0,sigma=1, delta=1,xi=0), xlim = c(-3,3), xlab='y', ylab='h(y)',ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(h_Bgev(x,mu=0,sigma=1.5, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(h_Bgev(x,mu=0, sigma=2, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(h_Bgev(x,mu=0, sigma=3, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(sigma, "= 1")), expression(paste(sigma, "= 1.5")),
                             expression(paste(sigma, "= 2")), expression(paste(sigma, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

dev.off()


### mundando o delta #### 


#par(mfrow=c(1,1))
png('imagens\\bgev_muda_delta.png', width = 178 , height=124  , units = 'mm', bg = 'white', res=1080)

par(mai=c(0.35,0.7,0.3,0.1),mfrow=c(2,2))

curve(dbgevd(x,mu=0,sigma=1, delta=0, xi=0), xlim = c(-3,3), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(dbgevd(x,mu=0,sigma=1, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(dbgevd(x,mu=0, sigma=1, delta=2, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(dbgevd(x,mu=0, sigma=1, delta=3, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')

legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(F_Bgev(x,mu=0,sigma=1, delta=0,xi=0), xlim = c(-3,3), xlab='y', ylab='F(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(F_Bgev(x,mu=0,sigma=1, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(F_Bgev(x,mu=0, sigma=1, delta=2, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(F_Bgev(x,mu=0, sigma=1, delta=3, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)



curve(S_Bgev(x,mu=0,sigma=1, delta=0,xi=0), xlim = c(-3,3), xlab='y', ylab='S(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(S_Bgev(x,mu=0,sigma=1, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(S_Bgev(x,mu=0, sigma=1, delta=2, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(S_Bgev(x,mu=0, sigma=1, delta=3, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=1.5, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                            expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(h_Bgev(x,mu=0,sigma=1, delta=0,xi=0), xlim = c(-3,3), xlab='y', ylab='h(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(h_Bgev(x,mu=0,sigma=1, delta=1,xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(h_Bgev(x,mu=0, sigma=1, delta=2, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(h_Bgev(x,mu=0, sigma=1, delta=3, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

dev.off()

### Mudando Xi #### 


png('imagens\\bgev_muda_xi.png',width = 178 , height=124, units = 'mm', bg = 'white', res=1080)

par(mai=c(0.35,0.7,0.3,0.1),mfrow=c(2,2))

curve(dbgevd(x,mu=0,sigma=1, delta=1, xi=-1), xlim = c(-3,3), xlab='y', ylab='f(y)', ylim=c(0,2), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(dbgevd(x,mu=0,sigma=1, delta=1, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(dbgevd(x,mu=0, sigma=1, delta=1, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(dbgevd(x,mu=0, sigma=1, delta=1, xi=1), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=2, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= -0.5")),
                             expression(paste(xi, "= 0.5")), expression(paste(xi, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(F_Bgev(x,mu=0,sigma=1, delta=1,xi=-1), xlim = c(-3,3), xlab='y', ylab='F(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(F_Bgev(x,mu=0,sigma=1, delta=1,xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(F_Bgev(x,mu=0, sigma=1, delta=1, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(F_Bgev(x,mu=0, sigma=1, delta=1, xi=1), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= -0.5")),
                             expression(paste(xi, "= 0.5")), expression(paste(xi, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(S_Bgev(x,mu=0,sigma=1, delta=1,xi=-1), xlim = c(-3,3), xlab='y', ylab='S(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(S_Bgev(x,mu=0,sigma=1, delta=1,xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(S_Bgev(x,mu=0, sigma=1, delta=1, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(S_Bgev(x,mu=0, sigma=1, delta=1, xi=1), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=0.8, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= -0.5")),
                             expression(paste(xi, "= 0.5")), expression(paste(xi, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

curve(h_Bgev(x,mu=0,sigma=1, delta=1,xi=-1), xlim = c(-3,3), xlab='y', ylab='h(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(h_Bgev(x,mu=0,sigma=1, delta=1,xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(h_Bgev(x,mu=0, sigma=1, delta=1, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(h_Bgev(x,mu=0, sigma=1, delta=1, xi=1), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= -0.5")),
                            expression(paste(xi, "= 0.5")), expression(paste(xi, "= 1")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


dev.off()


###### Mudando xi negativo = - 0.5 ##### 

png('imagens\\bgev_xi_negativo.png',width = 178 , height=124, units = 'mm', bg = 'white', res=1080)

par(mai=c(0.35,0.7,0.3,0.1),mfrow=c(2,2))

curve(dbgevd(x,mu=0,sigma=1, delta=0, xi=-0.5), xlim = c(-3,3), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(dbgevd(x,mu=0,sigma=1, delta=1, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(dbgevd(x,mu=0, sigma=1, delta=2, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(dbgevd(x,mu=0, sigma=1, delta=3, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')

legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(F_Bgev(x,mu=0,sigma=1, delta=0,xi=-0.5), xlim = c(-3,3), xlab='y', ylab='F(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(F_Bgev(x,mu=0,sigma=1, delta=1,xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(F_Bgev(x,mu=0, sigma=1, delta=2, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(F_Bgev(x,mu=0, sigma=1, delta=3, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)



curve(S_Bgev(x,mu=0,sigma=1, delta=0,xi=-0.5), xlim = c(-3,3), xlab='y', ylab='S(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(S_Bgev(x,mu=0,sigma=1, delta=1,xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(S_Bgev(x,mu=0, sigma=1, delta=2, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(S_Bgev(x,mu=0, sigma=1, delta=3, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=1.5, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                              expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

curve(h_Bgev(x,mu=0,sigma=1, delta=0,xi=-0.5), xlim = c(-3,3), xlab='y', ylab='h(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(h_Bgev(x,mu=0,sigma=1, delta=1,xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(h_Bgev(x,mu=0, sigma=1, delta=2, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(h_Bgev(x,mu=0, sigma=1, delta=3, xi=-0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

dev.off()




###### Mudando xi negativo = 0.5 ##### 7

png('imagens\\bgev_xi_positivo.png',width = 178 , height=124, units = 'mm', bg = 'white', res=1080)

par(mai=c(0.35,0.7,0.3,0.1),mfrow=c(2,2))

curve(dbgevd(x,mu=0,sigma=1, delta=0, xi=0.5), xlim = c(-3,3), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(dbgevd(x,mu=0,sigma=1, delta=1, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(dbgevd(x,mu=0, sigma=1, delta=2, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(dbgevd(x,mu=0, sigma=1, delta=3, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')

legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)


curve(F_Bgev(x,mu=0,sigma=1, delta=0,xi=0.5), xlim = c(-3,3), xlab='y', ylab='F(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(F_Bgev(x,mu=0,sigma=1, delta=1,xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(F_Bgev(x,mu=0, sigma=1, delta=2, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(F_Bgev(x,mu=0, sigma=1, delta=3, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)



curve(S_Bgev(x,mu=0,sigma=1, delta=0,xi=0.5), xlim = c(-3,3), xlab='y', ylab='S(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(S_Bgev(x,mu=0,sigma=1, delta=1,xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(S_Bgev(x,mu=0, sigma=1, delta=2, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(S_Bgev(x,mu=0, sigma=1, delta=3, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=1.5, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                              expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

curve(h_Bgev(x,mu=0,sigma=1, delta=0,xi=0.5), xlim = c(-3,3), xlab='y', ylab='h(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(h_Bgev(x,mu=0,sigma=1, delta=1,xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='green')
curve(h_Bgev(x,mu=0, sigma=1, delta=2, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(h_Bgev(x,mu=0, sigma=1, delta=3, xi=0.5), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(delta, "= 0")), expression(paste(delta, "= 1")),
                             expression(paste(delta, "= 2")), expression(paste(delta, "= 3")) )
       ,lwd = 1, col = c("black", "green", "blue","red"), cex=0.7, bty = "n", y.intersp=1)

dev.off()


###### Mudando delta= 0 variando xi #######

png('imagens\\delta_zero.png',width = 178 , height=124, units = 'mm', bg = 'white', res=1080)

par(mai=c(0.35,0.7,0.3,0.1),mfrow=c(2,2))

curve(dbgevd(x,mu=0,sigma=1, delta=0, xi=-1), xlim = c(-3,3), xlab='y', ylab='f(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(dbgevd(x,mu=0,sigma=1, delta=0, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='blue')
curve(dbgevd(x,mu=0, sigma=1, delta=0, xi=1), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="red")

legend(x=-3, y=1, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= 0")),
                             expression(paste(xi, "= 1")))
       ,lwd = 1, col = c("black", "blue", "red"), cex=0.7, bty = "n", y.intersp=1)


curve(F_Bgev(x,mu=0,sigma=1, delta=0,xi=-1), xlim = c(-3,3), xlab='y', ylab='F(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(F_Bgev(x,mu=0, sigma=1, delta=0, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(F_Bgev(x,mu=0, sigma=1, delta=0, xi=1), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= 0")),
                             expression(paste(xi, "= 1")))
       ,lwd = 1, col = c("black", "blue", "red"), cex=0.7, bty = "n", y.intersp=1)



curve(S_Bgev(x,mu=0,sigma=1, delta=0,xi=-1), xlim = c(-3,3), xlab='y', ylab='S(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(S_Bgev(x,mu=0, sigma=1, delta=0, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(S_Bgev(x,mu=0, sigma=1, delta=0, xi=1), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=0.7, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= 0")),
                             expression(paste(xi, "= 1")))
       ,lwd = 1, col = c("black", "blue", "red"), cex=0.7, bty = "n", y.intersp=1)


curve(h_Bgev(x,mu=0,sigma=1, delta=0,xi=0), xlim = c(-5,20), xlab='y', ylab='h(y)', ylim=c(0,1), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(h_Bgev(x,mu=0, sigma=2, delta=0, xi=0), xlim = c(-5,20),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(h_Bgev(x,mu=0, sigma=3, delta=0, xi=0), xlim = c(-5,20),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-5, y=1, legend = c(expression(paste(sigma, "= 1")), expression(paste(sigma, "= 2")),
                             expression(paste(sigma, "= 3")))
       ,lwd = 1, col = c("black", "blue", "red"), cex=0.7, bty = "n", y.intersp=1)

dev.off()



par(mfrow=c(1,1))


suport(3,1,10,0)

curve(h_Bgev(x,mu=0,sigma=1, delta=1,xi=-1), xlim = c(-3,3), xlab='x', ylab='h(x)', ylim=c(0,2), lwd=1.5,cex.lab=1, 
      cex.axis=0.5)
curve(h_Bgev(x,mu=0, sigma=1, delta=1, xi=0), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col="blue")
curve(h_Bgev(x,mu=0, sigma=1, delta=1, xi=1), xlim = c(-3,3),ylim=c(0,1), lwd=1.5, add = T, col='red')
legend(x=-3, y=1, legend = c(expression(paste(xi, "= -1")), expression(paste(xi, "= 0")),
                             expression(paste(xi, "= 1")))
       ,lwd = 1, col = c("black", "blue", "red"), cex=0.7, bty = "n", y.intersp=1)


###############

