source("BGEV_MLE.r")
source("envelope_function.r")

library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

cor(tabela_regressao$Temperatura_orvalho, tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel))

cor(tabela_regressao$Umidade_rel, tabela_regressao$Pressao_media)




## lendo os dados da tabela regressão 

tabela_regressao <- read_rds(file = 'dados_resumidos/tabela_regressao.rds')
df <- read_rds("dados_resumidos/dados_originais.rds")


tabela_regressao$Precipitacao_media[which(is.na(tabela_regressao$Precipitacao_media))] <- 0

tabela_regressao$estacoes <- ifelse(month(tabela_regressao$Data)%in%c(10,11,12,1,2,3,4),1,0)
tabela_regressao$chuva <- ifelse(tabela_regressao$Precipitacao_media>0,1,0)


## Umidade_rel
x1 <- tabela_regressao$Umidade_rel
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


## Umidade_rel - mean(umidade_real)
x1 <- tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
#### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

## estacoes
x1 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


### Temperatura Bulbo

x1 <- tabela_regressao$Temperatura_bulbo
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Temperatura - mean(temperatura_bulbo) ##

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Pressão 

x1 <- tabela_regressao$Pressao_media
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Pressão - mean(pressão)

x1 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Velocidade do vento Rajada 

x1 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


### radiação

x1 <- tabela_regressao$Radiacao_global
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


##### Tentando fazer com duas variáveis #####





