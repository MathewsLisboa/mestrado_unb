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

####### envelope
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi, fit_BGEV2$delta))
