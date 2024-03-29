source("BGEV_MLE.r")
source("envelope_function.r")


library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

tabela_regressao <- read_rds(file = 'D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/tabela_regressao.rds')
df <- read_rds("D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/dados_originais.rds")

tabela_regressao$Precipitacao_media[which(is.na(tabela_regressao$Precipitacao_media))] <- 0

tabela_regressao$estacoes <- ifelse(month(tabela_regressao$Data)%in%c(10,11,12,1,2,3,4),1,0)
tabela_regressao$chuva <- ifelse(tabela_regressao$Precipitacao_media>0,1,0)

cor(tabela_regressao$Temperatura_bulbo, tabela_regressao$Umidade_rel)


cor(tabela_regressao$Vento_velocidade, tabela_regressao$Umidade_rel)

chisq.test(as_factor(tabela_regressao$estacoes), tabela_regressao$Umidade_rel)
chisq.test(as.factor(tabela_regressao$estacoes), as.factor(tabela_regressao$chuva) )

plot(tabela_regressao$Umidade_rel,tabela_regressao$Temperatura_orvalho)
plot(tabela_regressao$Temperatura_bulbo,tabela_regressao$Temperatura_orvalho)
plot(tabela_regressao$Pressao_media,tabela_regressao$Temperatura_orvalho)
plot(as.factor(tabela_regressao$chuva),tabela_regressao$Temperatura_orvalho)
plot(as.factor(tabela_regressao$estacoes),tabela_regressao$Temperatura_orvalho)
plot(tabela_regressao$Pressao_media, tabela_regressao$Temperatura_orvalho)


### chuva veio direto do banco df, somando todos os militros.
x1 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho

#hist(y)
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

######  ESTAÇÕES FOI A PRIMEIRA VARIÁVEL ######

plot(as_factor(tabela_regressao$estacoes), tabela_regressao$Umidade_rel)
##chisq.test(as_factor(tabela_regressao$estacoes), tabela_regressao$Umidade_rel)

plot(as_factor(tabela_regressao$estacoes), tabela_regressao$Temperatura_bulbo)
##chisq.test(as_factor(tabela_regressao$estacoes), tabela_regressao$Temperatura_bulbo)
chisq.test(as_factor(tabela_regressao$estacoes), as_factor(tabela_regressao$chuva))


plot(as.factor(tabela_regressao$estacoes), tabela_regressao$Vento_velocidade)
plot(as.factor(tabela_regressao$estacoes), tabela_regressao$Pressao_media)


x1 <- tabela_regressao$estacoes
x2 <- tabela_regressao$chuva

n <- nrow(tabela_regressao)

X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
X
y <- tabela_regressao$Temperatura_orvalho

#hist(y)

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

#### Chuva como segunda variável ####


x1 <- tabela_regressao$estacoes
x2 <- tabela_regressao$chuva

n <- nrow(tabela_regressao)

X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho

#hist(y)

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
salvo_est_chuva_delta_0
salvo_est_chuva_delta_01


###### FAZENDO COMBINAÇÕES DOIS A DOIS ####

## umidade e temperatura do ar
x1 <- tabela_regressao$Umidade_rel
x2 <- tabela_regressao$Temperatura_bulbo
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model

y <- tabela_regressao$Temperatura_orvalho

#hist(y)

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV
#salvo_est_chuva_delta_0<- fit_BGEV


####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))



## umidade e pressão atosférica
x1 <- tabela_regressao$Umidade_rel
x2 <- tabela_regressao$Pressao_media
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model

y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

## umidade e velocidade do ventor
x1 <- tabela_regressao$Umidade_rel
x2 <- tabela_regressao$Vento_velocidade
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model

y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


## umidade rel max e Temperatra min
x1 <- tabela_regressao$Umidade_rel_max
x2 <- tabela_regressao$Temperatura_min_bulbo
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model

y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


#Umidade rel max
x1 <- tabela_regressao$Umidade_rel_max
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model

y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))





## umidade max e Temperatra min

x1 <- tabela_regressao$Umidade_rel_max
x2 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model

y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV

####### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))


## umidade min e Temperatra min

# x1 <- tabela_regressao$estacoes
# x2 <- tabela_regressao$chuva
# x3 <- tabela_regressao$Umidade_rel_max
# x4 <- tabela_regressao$Temperatura_bulbo
# n <- nrow(tabela_regressao)
# 
# X<- matrix(c(rep(1,n),x1,x3,x4),ncol=4,byrow=F); #regressor matrix for the median model
# 
# y <- tabela_regressao$Temperatura_orvalho
# 
# fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
# fit_BGEV
# 
# ####### envelope
# envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
# 
