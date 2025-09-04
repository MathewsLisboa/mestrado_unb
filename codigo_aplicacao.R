getwd()
setwd('D:\\Users\\Mathews\\Documents\\Git\\mestrado_unb')


source("BGEV_MLE.r")
source("envelope_function.r")
source("BGEV2_MLE.r")
source("envelope_function2.r")


library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

## lendo os dados da tabela regressão 
tabela_regressao <- read_rds(file = 'dados_resumidos/tabela_regressao.rds')
df <- read_rds("dados_resumidos/dados_originais.rds")

tabela_regressao$Precipitacao_media[which(is.na(tabela_regressao$Precipitacao_media))] <- 0
tabela_regressao$estacoes <- ifelse(month(tabela_regressao$Data)%in%c(10,11,12,1,2,3,4),1,0) ### 1 está para meses de verão
                                                                                             ### 0 está para meses de invero
tabela_regressao$chuva <- ifelse(tabela_regressao$Precipitacao_media>0,1,0)

###### correlacao #####
par(mfrow=c(1,1))


png("D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/correcoes_cira/TO_VS_U.png",width = 800, height = 500 )

plot(tabela_regressao$Umidade_rel, tabela_regressao$Temperatura_orvalho ,
     pch=16, cex=2.2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5,xlab = '', ylab='',width = 800, height = 500)

dev.off()

ggplot(tabela_regressao, aes(x=Umidade_rel, y=Temperatura_orvalho)) +
  geom_point(size=2, shape=19)+
  xlab("")+ylab("")+
  theme_bw()+
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))
ggsave("D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/correcoes_cira/TO_VS_U.png",
       width = 800, height = 500, units = 'px' )


png("D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/correcoes_cira/TO_VS_P.png",width = 800, height = 500)
plot(tabela_regressao$Pressao_media, tabela_regressao$Temperatura_orvalho ,
     pch=16, cex=2.2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5,xlab = '', ylab='',width = 800, height = 500)
dev.off()

ggplot(tabela_regressao, aes(x=Pressao_media, y=Temperatura_orvalho)) +
  geom_point(size=2, shape=19)+
  xlab("")+ylab("")+
  theme_bw()+
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))
ggsave("D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/correcoes_cira/TO_VS_P.png",width = 800, height = 500, 
       units='px')






png("D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/correcoes_cira/TO_VS_vvr.png",width = 800, height = 500)
plot(tabela_regressao$Vento_velocidade , tabela_regressao$Temperatura_orvalho, 
     pch=16, cex=2.2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5,xlab = '', ylab='')
dev.off()

ggplot(tabela_regressao, aes(x=Vento_velocidade, y=Temperatura_orvalho)) +
  geom_point(size=2, shape=19)+
  xlab("")+ylab("")+
  theme_bw()+
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))
ggsave("D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/correcoes_cira/TO_VS_vvr.png",width = 800, height = 500, 
       units='px')



tabela_regressao$labels <- labels <- tabela_regressao$estacoes %>% as.character()

tabela_regressao$labels <- ifelse(test = tabela_regressao$labels== '1', 'Rainy', 'Dry')


ggplot(tabela_regressao, aes(x=as.factor(labels), y=Temperatura_orvalho)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) +
    xlab("")+ylab("")+
  theme_bw()+
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))
ggsave("D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/correcoes_cira/TO_VS_E.png",
       width = 800, height = 500, units = 'px' )











mean(tabela_regressao$Umidade_rel)
mean(tabela_regressao$Pressao_media)

cor(tabela_regressao$Temperatura_orvalho, tabela_regressao$Umidade_rel) ## temperatura também não deve está no 
                                                                        ## no modelo com umidade
cor(tabela_regressao$Umidade_rel, tabela_regressao$Pressao_media) ### pressão e umidade podem compor
cor(tabela_regressao$Pressao_media, tabela_regressao$Temperatura_bulbo) ## Temperatura e Pressão não devem
                                                                        #compor o memso modelo


# envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))

#### Modelos simples ######

##### Umidade Relativa #####

x1 <- tabela_regressao$Umidade_rel - mean(tabela_regressao$Umidade_rel)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

## AIC xi  = 0 

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))

AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)

## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


##### Estacoes ######

x1 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,2))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

#### Temperatura Bulbo ####

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,2))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


#### Pressão #####
x1 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
AICc
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,2))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


#### Velocidade do vento Rajada #####
x1 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,2))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


#### Radiação #####

x1 <- tabela_regressao$Radiacao_global -  mean(tabela_regressao$Radiacao_global)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV2$xi,fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,2))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))



#### O modelo mais completo possível ####
x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
x3 <- tabela_regressao$Temperatura_bulbo -  mean(tabela_regressao$Temperatura_bulbo)
x4 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)
x5 <- tabela_regressao$Radiacao_global -  mean(tabela_regressao$Radiacao_global)
x6 <- tabela_regressao$estacoes


n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3,x4,x5,x6),ncol=7,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho


fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV2$xi,fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

#### Retirar radiação do modelo, obviamente 

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
x3 <- tabela_regressao$Temperatura_bulbo -  mean(tabela_regressao$Temperatura_bulbo)
x4 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)
x5 <- tabela_regressao$estacoes


n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3,x4,x5),ncol=6,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Temperatura_orvalho


fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV2$xi,fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


### Seguindo o mesmo criétio, eu vou tirar pressão do modelo ####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
x3 <- tabela_regressao$Temperatura_bulbo -  mean(tabela_regressao$Temperatura_bulbo)
x4 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)
x5 <- tabela_regressao$estacoes

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x3,x4,x5),ncol=4,byrow=F); #retirei pressão
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## até que ficou razoável 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta)) ## até que ficou razoável


## percebi que temperatura estava com muita correlçção com umidade e pressão, então decidi tirar do modelo
### Se provou uma decisão melhor retirar Temperatura, e recolocar pressão no modelo
### Melhor modelo com 4 variáveis ####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$estacoes
x3 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
x4 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max) ###vento rajada fica melhor

## E se esse for o modelo final ??
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3,x4),ncol=5,byrow=F); #modelo com pressão e sem temperatura 
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1)
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope

envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

### Modelos com 3 variáveis ###

### Retirando Estações ####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
x3 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)
x4 <- tabela_regressao$estacoes

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3),ncol=4,byrow=F); #retirando estações
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2 ## Sem estações isso aqui não converge, melhor voltar come estações
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)

### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


### Retirando Pressão ####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
x3 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)
x4 <- tabela_regressao$estacoes

## E se esse for o modelo final ??
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x3,x4),ncol=4,byrow=F); #retirando pressão
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2 ## nice
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta)) ## 2 envolopes bem feios tbm 


### Retirando Umidade relativa ####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
x3 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)
x4 <- tabela_regressao$estacoes

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x2,x3,x4),ncol=4,byrow=F); #retirando umidade
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2 ## nice, aqui ele simplesmente nçao gostou de ter pressão na brincadeira
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta)) ## até que ficou legalzinho 


### Retirando velocidade do vento rajada max####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$estacoes
x3 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
#x3 <- tabela_regressao$Vento_rajada_max -  mean(tabela_regressao$Vento_rajada_max)

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3),ncol=4,byrow=F); #retirando velocidade
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2 ## nice, aqui ele simplesmente nçao gostou de ter pressão na brincadeira
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
par(mfrow=c(1,2))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ##
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta)) ## até que ficou legalzinho 


#### Modelos 2 a 2 ####

### Pressão e Umidade ####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #retirando velocidade
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2 ## aparece um nan no pvalor, acho q nem vale a pena fazer gráfico disso 
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta)) ## até que ficou legalzinho 



### Umidade e Vento #####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel)
x2 <- tabela_regressao$Vento_velocidade -  mean(tabela_regressao$Vento_velocidade) ## parece que vento não é bom

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #retirando velocidade
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)

fit_BGEV ## ainda não deu significativo no vento 
fit_BGEV2 ## Não é significativo no vento
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))
## realmente perde-se mtoas amostras, não considero que tenha sido bom 
## a imgagem fica boa, mas não justifcia 


### Pressao e Vento #####

x1 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)
x2 <- tabela_regressao$Vento_velocidade -  mean(tabela_regressao$Vento_velocidade) ## parece que vento não é bom

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #retirando velocidade
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)

fit_BGEV ## vento não significativo 
fit_BGEV2 ## vento não significativo
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV2$xi,fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))
## realmente perde-se mtoas amostras, não considero que tenha sido bom 
## a imgagem fica boa, mas não justifcia 


### Estacoes e Vento ####

x1 <- tabela_regressao$estacoes
x2 <- tabela_regressao$Vento_velocidade -  mean(tabela_regressao$Vento_velocidade) ## parece que vento não é bom

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #retirando velocidade
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)

fit_BGEV ## 
fit_BGEV2 ##
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

### ESTACOES E UMIDADE ####

x1 <- tabela_regressao$Umidade_rel -  mean(tabela_regressao$Umidade_rel) ## parece que vento não é bom
x2 <- tabela_regressao$estacoes

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #retirando velocidade
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)

fit_BGEV ## 
fit_BGEV2 ##
## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
#par(mfrow=c(1,2))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

#### ESTACOES E PRESSÃO #### 
x1 <- tabela_regressao$estacoes
x2 <- tabela_regressao$Pressao_media -  mean(tabela_regressao$Pressao_media)

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #retirando velocidade
y <- tabela_regressao$Temperatura_orvalho

fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV ## 
fit_BGEV2 ##

## AIC xi  = 0 
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+2)*((ncol(X)+2)+1))/(nrow(X)-(ncol(X)+2)-1) ### embora o AICc seja melhor
## AIC xi != 0
ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3)
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X)))
AICc <- AIC + (2*(ncol(X)+3)*((ncol(X)+3)+1))/(nrow(X)-(ncol(X)+3)-1)
## aic pior do que com pressão
### envelope
par(mfrow=c(1,1))
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) ## envolope tbm ficou feio q doi 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

