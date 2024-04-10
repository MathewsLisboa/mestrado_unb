### codigo de aplicação com umidade sendo variavel resposta
source("BGEV_MLE.r")
source("envelope_function.r")

library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
## lendo os dados da tabela regressão 
tabela_regressao <- read_rds(file = 'dados_resumidos/tabela_regressao_umidade.rds')

df <- read_rds("dados_resumidos/dados_originais.rds")

### criando variável chuva e variável estação 
tabela_regressao$Precipitacao_media[which(is.na(tabela_regressao$Precipitacao_media))] <- 0
tabela_regressao$estacoes <- ifelse(month(tabela_regressao$Data)%in%c(10,11,12,1,2,3,4),1,0)
tabela_regressao$chuva <- ifelse(tabela_regressao$Precipitacao_media>0,1,0)

###

plot(tabela_regressao$Umidade_rel, tabela_regressao$Temperatura_bulbo)

plot(tabela_regressao$Umidade_rel, tabela_regressao$Temperatura_orvalho)

plot(tabela_regressao$Umidade_rel, tabela_regressao$Pressao_media)

plot(tabela_regressao$Umidade_rel, tabela_regressao$Radiacao_global)

plot(tabela_regressao$Umidade_rel, tabela_regressao$Vento_velocidade)

plot(tabela_regressao$Umidade_rel, tabela_regressao$Vento_rajada_max)

plot(as.factor(tabela_regressao$estacoes), tabela_regressao$Umidade_rel)


#### Modelos Xi = 0 ###

#### Temperatura bulbo
x1 <- tabela_regressao$Temperatura_bulbo
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
####### envelope

envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Temperatura orvalho 

x1 <- tabela_regressao$Temperatura_orvalho
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Pressão atmosférica 

x1 <- tabela_regressao$Pressao_media
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


### Estações 

x1 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Radiação 

x1 <- tabela_regressao$Radiacao_global
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


### Velocidade do vento 

x1 <- tabela_regressao$Vento_rajada_max
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))



### Colocando duas variáveis Temperatura e Estações

### Temperatura e Estacoes, fica mais ou menos

x1 <- tabela_regressao$Temperatura_bulbo
x2 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Temperatura e Pressao nem estima, então vamos pra próxima


### Temperatura e Rajada_max

x1 <- tabela_regressao$Temperatura_bulbo
x2 <- tabela_regressao$Vento_rajada_max
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

### Temperatura e Rajada_max

### Temperatura do bulbo e Temperatura Orvalho
##( unico problema é ter duas temperaturas, 
#mas acho que bem explicadinho rola)
x1 <- tabela_regressao$Temperatura_bulbo
x2 <- tabela_regressao$Temperatura_orvalho
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


### Temperatura e Chuva
x1 <- tabela_regressao$Temperatura_bulbo
x2 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


### 3 variaveis, Temperatura, estacoes e rajada do vento
x1 <- tabela_regressao$Temperatura_bulbo
x2 <- tabela_regressao$Temperatura_orvalho
x3 <- tabela_regressao$Precipitacao_media
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3),ncol=4,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
