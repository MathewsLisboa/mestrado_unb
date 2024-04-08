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

x1 <- tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$xi, fit_BGEV$delta))

###### Estacoes ######### 
x1 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho

#hist(y)
fit_BGEV <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV2(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$xi, fit_BGEV$delta))

###### Umidade Relativa e Estacoes ######### 

x1 <- tabela_regressao$estacoes
x2 <- tabela_regressao$Umidade_rel
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho

#hist(y)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))


###### Umidade Relativa e Estacoes ######### 

x1 <- tabela_regressao$estacoes
x2 <-  tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho

#hist(y)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV2

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))