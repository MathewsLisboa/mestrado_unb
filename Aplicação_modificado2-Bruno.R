library('tidyverse')
library('lubridate')
library("fBasics")
#install.packages("StableEstim")

#dadosfinais1<-read.csv(file.choose())
#dadosfinais1

library("readxl")#### instalar o pacote "readxl" para abrir arquivos xlsx ####
dadosfinais<-read_xlsx(file.choose())

head(dadosfinais)

Dolar<-dadosfinais$Dolar

plot(Dolar, type="l", )

#dadosfinais1$retorno<-log(dadosfinais1$Dolar/dplyr::lag(dadosfinais1$Dolar))
#head(dadosfinais1$retorno)

#(dadosfinais1<-dadosfinais1[is.finite(dadosfinais1$retorno), ])
#(ret<-dadosfinais1$retorno)

dadosfinais$retorno<-log(dadosfinais$Dolar/dplyr::lag(dadosfinais$Dolar))
head(dadosfinais$retorno)

(dadosfinais<-dadosfinais[is.finite(dadosfinais$retorno), ])
(ret<-dadosfinais$retorno)


##################################
plot(ret,type="l")
###  HISTOGRAMA #########
hist(ret, n = 50, probability = TRUE, border = "white",
     col = "steelblue", main="", xlim=c(-0.1, 0.1), ylim=c(0,40), xlab="log Retorno diario dadosfinais",
     ylab="Hist")


################## Sub amostra de Bloco M?ximo  ###########

N<-length(ret)
n<-10
tau<-floor(N/n)
M<-matrix(0,tau,1)
j<-1
for (i in 1:tau){
  M[i]<-max(ret[j:(j+n-1)])
  j<-j+n}
head(M)
sum(is.na(M))
hist(M,n=35, prob=T,ylim=c(0,50))
lines(density(M),lwd=2)

M<- M[1:35]
length(M)

################## Sub amostra de Bloco m?nimo  ###########

N<-length(ret)
n<-18
tau<-floor(N/n)
m<-matrix(0,tau,1)
j<-1
for (i in 1:tau){
  m[i]<-min(ret[j:(j+n-1)])
  j<-j+n}
head(m)
sum(is.na(m))
hist(m,n=35, prob=T, xlim=c(-0.1,0.01),ylim=c(0,50))
lines(density(m),lwd=2)

m<- m[1:35]
length(m)

########## Grafico duplo de maximos e minimos e histograma ######
lim.inf = min(c(m,M))
lim.sup = max(c(m,M))

plot(M, type = "l", lty=1, lwd = 1,
     ylim = c(lim.inf,lim.sup), ylab = "Y",xlab="t")#grafico de maximo

# ylim: limites de y especificado para visualizar os valores de min e max

lines(m, type = "l", lty=2, lwd = 1)#colocando os minimos

abline(h=0,lwd=1) #linha horizontal

legend(x = "right",
       legend = c("Maxima", "Minima"),
       lty = c(1, 2),
       lwd = c(1,2))  

################# hist and fit dolar #############
gumbel <-  function(x, mu, sigma){
  densidade <- 1/sigma * exp(-(x - mu)/sigma - exp(-(x - mu)/sigma))
  return(densidade)
}

dgumbim <- function(x, mu, sigma, delta){
  pdf <- function(x,mu,sigma,delta){
    obj <- (1/sigma) * (delta + 1) * abs(x)^delta * 
      exp(- (x * abs(x)^delta + mu)/sigma - exp(- (x * abs(x)^delta + mu)/sigma) )
    return(obj)
  }
  aux.pdf <- mapply(FUN = pdf, x = x, mu = mu, sigma = sigma, delta = delta)
  return(aux.pdf)
}

################ dólar #############
hist(x,xlim = c(-0.1,0.1),ylim = c(0,21),
     breaks = 35,col = "grey ",ylab = "Frequency",xlab = "Y",main = "")

x<- c(m,M)
hist(x,xlim = c(-0.1,0.1),ylim = c(0,100),
     breaks = 40,col = "grey",ylab = "Density",xlab = "Y",main = "")


#lines(density(c(m,M)),lwd=2,col = "blue")
curve(dgumbim(x, mu = 0.000016486, sigma = 0.000079908,
              delta = 1.31295954), from = -0.5, to = 0.5,
      add = T, lwd = 1.8, col = "blue")

################# para aplicação dolar fit
x<- c(m,M)

hist_info <- hist(x,xlim = c(-0.1,0.1), plot = FALSE,
                  breaks = 40,col = "white ",ylab = "Fitted density",xlab = "Y",
                  main = "")

# Store output of hist function
hist_info$density <- hist_info$counts/sum(hist_info$counts)
plot(hist_info, freq = FALSE, xlim = c(-0.1,0.1),
     ylab = "Density",xlab = "Y", main = "")
############# branco

hist_info$density <- hist_info$counts/sum(hist_info$counts)
plot(hist_info, freq = FALSE, xlim = c(-0.1,0.1),
     ylab = "Fitted density",xlab = "Y", main = "", col = "white ")


#__________________________________________________________________
#PETROLEO
library("readxl")#### instalar o pacote "readxl" para abrir arquivos xlsx ####
dadosfinais<-read_xlsx(file.choose())

head(dadosfinais)

Petr<-dadosfinais$Petr

plot(Petr, type="l")

#dadosfinais1$retorno<-log(dadosfinais1$Dolar/dplyr::lag(dadosfinais1$Dolar))
#head(dadosfinais1$retorno)

#(dadosfinais1<-dadosfinais1[is.finite(dadosfinais1$retorno), ])
#(ret<-dadosfinais1$retorno)

dadosfinais$retorno1<-log(dadosfinais$Petr/dplyr::lag(dadosfinais$Petr))
head(dadosfinais$retorno1)

(dadosfinais<-dadosfinais[is.finite(dadosfinais$Petr), ])
(ret<-dadosfinais$retorno1)
ret

##################################
plot(ret,type="l")
###  HISTOGRAMA #########
hist(ret, n = 50, probability = TRUE, border = "white",
     col = "grey", xlim=c(-0.1, 0.1),  ylim=c(0,18), main="", xlab="",
     ylab="Hist")


################## Sub amostra de Bloco M?ximo  ###########

N<-length(ret)
n<-18
tau<-floor(N/n)
M<-matrix(0,tau,1)
j<-1
for (i in 1:tau){
  M[i]<-max(ret[j:(j+n-1)])
  j<-j+n}
head(M)
sum(is.na(M))
hist(M,n=18, prob=T,  ylim=c(0,20), xlim=c(0,0.3))
lines(density(M),lwd=2)
M
M<- M[2:35]
length(M)

################## Sub amostra de Bloco minimo  ###########

N<-length(ret)
n<-15
tau<-floor(N/n)
m<-matrix(0,tau,1)
j<-1
for (i in 1:tau){
  m[i]<-min(ret[j:(j+n-1)])
  j<-j+n}
head(m)
sum(is.na(m))
hist(m,n=15, prob=T)
lines(density(m),lwd=2)

m<- m[2:35]
length(m)

########## Grafico duplo de maximos e minimos e histograma PRT4 ######
lim.inf = min(c(m,M))
lim.sup = max(c(m,M))

plot(M, type = "l", lty=1, lwd = 1, ylab = "Y",xlab="t", ylim = c(-0.2,0.8))#grafico de maximo

# ylim: limites de y especificado para visualizar os valores de min e max

lines(m, type = "l", lty=2, lwd = 1)#colocando os minimos

abline(h=0,lwd=1) #linha horizontal

legend(x = "right",
       legend = c("Maxima", "Minima"),
       lty = c(1,2),
       lwd = c(1,2))  


hist(c(m,M), xlim = c(-0.2,0.2),ylim = c(0,20),breaks = 40,
     col = "grey",ylab = "Frequency",xlab = "Y",main = "")
lines(density(c(m,M)),lwd=2)
################## para aplicação PTR4

x<- c(m,M)
hist(x,xlim = c(-0.2,0.2),ylim = c(0,21),
     breaks = 40,col = "white ",ylab = "Fitted density",xlab = "Y",main = "")
#lines(density(c(m,M)),lwd=2, xlim = c(-0.01,0.01), col = "blue")
curve(dgumbim(x, mu = 0.000019962, sigma = 0.00009877,
              delta = 1.236255323), from = -0.5, to = 0.5,
      add = T, lwd = 1.8, col = "blue")

############ histogram de probab PTR4 #######
x<- c(m,M)
hist_info <- hist(x,xlim = c(-0.4,0.4), plot = FALSE,
                  breaks = 40,col = "white ",ylab = "Density",xlab = "Y",
                  main = "")
# Store output of hist function
hist_info$density <- hist_info$counts/sum(hist_info$counts)
plot(hist_info, freq = FALSE, xlim = c(-0.4,0.4),
     ylab = "Density",xlab = "Y", main = "")

################################ branco
hist_info$density <- hist_info$counts/sum(hist_info$counts)
plot(hist_info, freq = FALSE, xlim = c(-0.4,0.4),
     ylab = "Fitted density",xlab = "Y",col = "white", main = "")

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

curve(dbgevd(x, mu = 0.000016486, sigma = 0.000099908, xi=0.000005, delta = 0.31295954),
      xlim = c(-3, 3),  xlab=expression(" "), ylab = expression(" "),lwd = 1.5)

hist(100*x,xlim = c(-4,4),ylim = c(0,21),
     breaks = 35,col = "grey ",ylab = "Frequency",xlab = "Y",main = "")
######################################################################
x<- c(m,M)
hist_info <- hist(x,xlim = c(-0.1,0.1), plot = FALSE,
                  breaks = 40,col = "white ",ylab = "Fitted density",xlab = "Y",
                  main = "")
# Store output of hist function
hist_info$density <- hist_info$counts/sum(hist_info$counts)
plot(hist_info, freq = FALSE, xlim = c(-0.1,0.1),
     ylab = "Fitted density",xlab = "Y", main = "")
#############
#lines(density(c(m,M)),lwd=2,col = "blue")
curve(dbgevd(x, mu = 0.000016486, sigma = 0.000099908, xi=1, delta = 10.31295954),
      xlim = c(-0.2, 0.2),  xlab=expression(" "), ylab = expression(" "),
      lwd = 1.5, add = T, col = "red" )

