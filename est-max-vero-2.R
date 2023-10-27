

library(evd)
#gera números aleatórios da distribuição gumbel bivariada
xy <- function (n=1000,M1=0,M2=0,D=c(0,0),sigma1=1,sigma2=1,xi1=0,xi2=0,
                modelo="log"){ 
  x <- rbvevd(n=n, dep=0.7, model =modelo,#dep=r
              mar1 = c(0, sigma1, xi1),mar2 = c(0, sigma2, xi2))
  y1 <- (x[,1]-M1)*((abs(x[,1]-M1))**D[1])
  y2 <- (x[,2]-M2)*((abs(x[,2]-M2))**D[2])
 y <- matrix(c(y1,y2), ncol= 2, nrow =n,
              dimnames=list(c(),c("y1", "y2")))
  return(y)
}
Y<-xy()
plot(y)
##############
fbvevd(x, model = "log")


library(tidyverse)
ggplot(y, aes(V1, V2)) + 
  geom_jitter()+
  geom_density_2d(color="orange" )+ 
  labs(subtitle="", 
       y="", 
       x="", 
       title="", 
       caption = "")
g3d(xi=c(0,0.5),sigma=c(1,1),media=c(0,0),delta=c(0,0),alpha=1,modelo="log")


bgumbel<-function(x,sigma1,sigma2,xi1,xi2){
  a<-dbvevd(x, dep=0.7, model ="log",
            mar1 = c(0, sigma1, xi1),mar2 = c(0, sigma2, xi2))
  return(a)
}
#### teste de estimação 
library("mvtnorm")
library("microbenchmark") 
library("parallel") 
library("doParallel") 
library("foreach")


estimador <- function (theta,N){
  MFH <- function(theta,Y){
    M1 <- theta[1] 
    M2 <- theta[2]
    D<- theta[3:4]
    sigma1 <- theta [5] 
    sigma2 <- theta[6] 
    xi1=theta[7] 
    xi2=theta[8]
    
    X1 = Y[,1]; X2 = Y[,2]
    
    t1 <- (X1-M1)*((abs(X1-M1))**D[1])
    t2 <- (X2-M2)*((abs(X2-M2))**D[2])
    
    #psi diferente de 0
    #u_t1= (1+(xi1/sigma1)*t1)^((-1/xi1))
    #v_t2= (1+(xi2/sigma2)*t2)^((-1/xi2))
    #u_linha=(-1/xi1)*(1+(xi1/sigma1)*t1)^((-1/xi1)-1)*(xi1/sigma1)
    #v_linha=(-1/xi2)*(1+(xi2/sigma2)*t2)^((-1/xi2)-1)*(xi2/sigma2)

    #psi=0
    u_t1=exp(-t1/sigma1)
    v_t2=exp(-t2/sigma2)
    u_linha= -(1/sigma1)*exp(-t1/sigma1)
    v_linha=-(1/sigma2)*exp(-t2/sigma2)
    tm = matrix(c(u_t1, v_t2), ncol=2)
    bdgdelta <- dbvevd(x=tm,dep=0.7, model ="log",
                       mar1 = c(0, sigma1, xi1),mar2 = c(0, sigma2, xi2))*######
    ((D[1]+1)*(D[2]+1))*(abs(X1-M1)**D[1])*(abs(X2-M2)**D[2])*u_linha*v_linha
    
    blogl <- sum(log(bdgdelta),na.rm =TRUE)
    return(-blogl)
  }#fun. a ser maximizada
  Z <- foreach::foreach(i = 1:100) %do% {
    xy(n=N,M1=theta[1],M2=theta[2], D=theta[3:4],sigma1=theta [5],sigma2=theta[6],
       xi1=theta[7],xi2=theta[8],modelo="log")
    
  }#lista com 1000 amostras aleatorias de tamanho N
  oi <- function (Z, theta) {
    optim(par = theta1, fn= MFH, Y=Z[[1]], method="BFGS")$par
  }# maximizador
  
  #  cl <- parallel::makeCluster(detectCores())
  #  doParallel::registerDoParallel(cl)
  #  zfoi <- foreach::foreach(i = 1:10, .combine = 'c')%dopar%{oi(Z[[i]], theta)}
  #  parallel::stopCluster (cl) #para cada amostra calcula os estimadores
  zfoi <-  foreach(i = 1:10, .combine = 'c')%do%{oi(Z[[i]], theta)}
  dados_e <- matrix(zfoi, ncol=8,byrow=T)
  colnames(dados_e) = c("M1", "M2","D1","D2", "Sigma1", "sigma2", "xi1", "xi2")
  return(dados_e) #1000 valores estimados para cada parametros
}


N=100#tamanho da amostra
theta = c(M1=0,
          M2=0,
          D1=0,
          D2=0,
          sigma1=1,
          sigma2=1,
          xi1=0,
          xi2=0)

theta1 = c(M1=0.2,
          M2=1,
          D1=0,
          D2=0,
          sigma1=1,
          sigma2=3,
          xi1=0.1,
          xi2=0.2)
start_time <- Sys.time()
est_dados=estimador(theta, N)
end_time <- Sys.time()
theta_est <- apply(est_dados, 2, mean) 
theta_res <-(theta_est - theta)^2
theta_sd <- apply(est_dados,2,sd)
(tempo =end_time - start_time)
data.frame(theta, theta_est, theta_res, theta_sd)
