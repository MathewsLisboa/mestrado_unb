source("BGEV2_MLE.r")
source("envelope_function2.r")

library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

tabela_regressao <- read_rds(file = 'dados_resumidos/tabela_regressao.rds')
df <- read_rds("dados_resumidos/dados_originais.rds")

tabela_regressao$Precipitacao_media[which(is.na(tabela_regressao$Precipitacao_media))] <- 0
tabela_regressao$estacoes <- ifelse(month(tabela_regressao$Data)%in%c(10,11,12,1,2,3,4),1,0)
tabela_regressao$chuva <- ifelse(tabela_regressao$Precipitacao_media>0,1,0)

###### Tentando incluir primeiro umidade ######### 

### umidade nem entra no modelo pq não ajusta
x1 <- tabela_regressao$Umidade_rel
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


#### umidade- mean(umidade)

x1 <- tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2
#AIC
ll2 <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))
AIC2 <- -2*ll2 + 2*ncol(X) 
####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


#### Estacoes 

x1 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


#### Pressão, essa pressão ficou muito boa, acho que vou colcoar ela com estações
x1 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


#### Temperatura bulbo
x1 <- tabela_regressao$Temperatura_bulbo
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


#### Tentativas com duas variáveis #######


### Umidade  e Estacoes

x1 <- tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


### Estacoes e Pressao

x1 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)
x2 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X,c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


### Estacoes e Temperatura

x1 <- tabela_regressao$Temperatura_bulbo
x2 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X,c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


##### Umidade e Pressão 
x1 <- tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2
####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))




##### Umidade e Velocidade do Vento 
x1 <- tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Vento_velocidade - mean(tabela_regressao$Vento_velocidade)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2
####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))



### Tentativa com 3 variáveis ####

x1 <- tabela_regressao$Pressao_media- mean(tabela_regressao$Pressao_media)
x2 <- tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
x3 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3),ncol=4,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X,c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))
