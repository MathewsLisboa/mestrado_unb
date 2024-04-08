source("BGEV_MLE.r")
source("envelope_function.r")

library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

tabela_regressao <- read_rds(file = 'dados_resumidos/tabela_regressao.rds')
df <- read_rds("dados_resumidos/dados_originais.rds")


## estacoes, umidade, pressao
x1 <- tabela_regressao$estacoes
x2 <- tabela_regressao$Pressao_media
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope

envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


## Umidade_rel
x1 <- tabela_regressao$Umidade_rel
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

## estacoes
x1 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


## pressao
x1 <- tabela_regressao$Pressao_media
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

## Estacoes e Chuva

x1 <- tabela_regressao$estacoes
x2 <- tabela_regressao$chuva
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
