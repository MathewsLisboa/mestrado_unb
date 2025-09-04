### codigo de aplicação com umidade sendo variavel resposta
getwd()

source("BGEV_MLE.r")
source("envelope_function.r")
source("BGEV2_MLE.r")
source("envelope_function2.r")


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



mean(tabela_regressao$Temperatura_bulbo)
mean(tabela_regressao$Vento_rajada_max)

##### Testando com apenas 1 variável ##### 

### Temperatura do ar seco, Pressão e Velocidade do vento - Estacoes 

#### Temperatura do ar seco #####

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)

fit_BGEV

fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #560.0423
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #569.2041

## AIC xi != 0

ll2 <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC2 <- -2*ll2+2*(ncol(X)+3) #553.6306
BIC2 <- -2*ll2+(ncol(X)+3)*log((nrow(X))) #565.0829

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta)) # xi = 0 ##### 
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


### Estações ####
x1 <- tabela_regressao$estacoes
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #581.6012
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #590.763


## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3) #576.3922
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X))) #587.8445

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


### Pressão ###

x1 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #568.7894
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #577.9513

## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3) #574.3449
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X))) #585.7972

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

### Velocidade do Vento Rajada ###

x1 <- tabela_regressao$Vento_rajada_max - mean(tabela_regressao$Vento_rajada_max)
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1),ncol=2,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #561.2527
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #570.4146


## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3) #556.1674
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X))) #567.6197

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

#### Duas Variáveis ####
### Temperatura ar Seco e Velocidade do Vento ####

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo)
x2 <- tabela_regressao$Vento_rajada_max - mean(tabela_regressao$Vento_rajada_max)

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #556.3689
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #567.8212

## AIC xi != 0

ll2 <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC2 <- -2*ll2+2*(ncol(X)+3) #544.1218
BIC2 <- -2*ll2+(ncol(X)+3)*log((nrow(X))) #557.8646

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

## Não acho que ficou legal misturar velocidade do vento e temperatura

### Temperatura ar Seco e Pressão ####

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo)
x2 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #557.0448
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #568.4971


## AIC xi != 0

ll2 <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC2 <- -2*ll2+2*(ncol(X)+3) #556.1428
BIC2 <- -2*ll2+(ncol(X)+3)*log((nrow(X))) #569.8855

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


#### Temperatura e Estacoes ####

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo)
x2 <- tabela_regressao$estacoes

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #559.2956
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #570.7479

## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3) #401
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X))) #405

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

#### Temperatura e Vento Rajada #######

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo)
x2 <- tabela_regressao$Vento_rajada_max - mean(tabela_regressao$Vento_rajada_max)

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #556.3689
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #567.8212

## AIC xi != 0

ll2 <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC2 <- -2*ll2+2*(ncol(X)+3) #544.1218
BIC2 <- -2*ll2+(ncol(X)+3)*log((nrow(X))) #557.8646

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))




#### Velocidade do vento rajada e pressão #######

x1 <- tabela_regressao$Vento_rajada_max - mean(tabela_regressao$Vento_rajada_max)
x2 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) 

## AIC xi != 0

ll2 <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC2 <- -2*ll2+2*(ncol(X)+3) #555.1618
BIC2 <- -2*ll2+(ncol(X)+3)*log((nrow(X))) #568.9046

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))

### velocidade do vento e estações #####


x1 <- tabela_regressao$Vento_rajada_max - mean(tabela_regressao$Vento_rajada_max)
x2 <- tabela_regressao$estacoes
  
n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) 

## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3) #401
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X))) #405

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


#### PRESSÃO E ESTACOES ####

x1 <- tabela_regressao$estacoes
x2 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2),ncol=3,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2)
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) 

## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3) #401
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X))) #405

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))



#### Usando 3 variáveis ####

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo) 
x2 <- tabela_regressao$Vento_rajada_max - mean(tabela_regressao$Vento_rajada_max)
x3 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media) 

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3),ncol=4,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2


# AIC xi  = 0
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #552.9676
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #566.7104

## AIC xi != 0

ll2 <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC2 <- -2*ll2+2*(ncol(X)+3) #551.1549
BIC2 <- -2*ll2+(ncol(X)+3)*log((nrow(X))) #567.1881

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))









#### 3 VARIÁVEIS COM ESTACOES #####

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo) 
x2 <- tabela_regressao$Vento_rajada_max - mean(tabela_regressao$Vento_rajada_max)
x3 <- tabela_regressao$estacoes

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3),ncol=4,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #545.1023
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #558.845

## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3) #551.1549
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X))) #567.1881

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


#### 3 VARIÁVEIS COM ESTACOES #####

x1 <- tabela_regressao$Temperatura_bulbo - mean(tabela_regressao$Temperatura_bulbo) 
x2 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media)
x3 <- tabela_regressao$estacoes

n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3),ncol=4,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0
ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #552.0327
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #565.7754

## AIC xi != 0

ll2 <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC2 <- -2*ll2+2*(ncol(X)+3) #534.7794
BIC2 <- -2*ll2+(ncol(X)+3)*log((nrow(X))) #550.8126

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))




### 4 variáveis #####

x1 <- tabela_regressao$Temperatura_bulbo 
x2 <- tabela_regressao$Vento_rajada_max - mean(tabela_regressao$Vento_rajada_max)
x3 <- tabela_regressao$Pressao_media - mean(tabela_regressao$Pressao_media) 
x4 <- tabela_regressao$estacoes


n <- nrow(tabela_regressao)
X<- matrix(c(rep(1,n),x1,x2,x3,x4),ncol=5,byrow=F); #regressor matrix for the median model
y <- tabela_regressao$Umidade_rel
fit_BGEV <- MLE_BGEV(y,X, method="BFGS", maxit=200)
fit_BGEV2 <- MLE_BGEV2(y,X, method="BFGS", maxit=200)
fit_BGEV
fit_BGEV2

# AIC xi  = 0

ll <- log_likelihood(c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
AIC <- -2*ll+2*(ncol(X)+2) #416
BIC <- -2*ll+(ncol(X)+2)*log((nrow(X))) #425

## AIC xi != 0

ll <- log_likelihood2(c(fit_BGEV2$beta, fit_BGEV2$sigma, fit_BGEV2$xi,fit_BGEV2$delta))
AIC <- -2*ll+2*(ncol(X)+3) #401
BIC <- -2*ll+(ncol(X)+3)*log((nrow(X))) #405

### envelope
envelope_BGEV(y, X, c(fit_BGEV$beta, fit_BGEV$sigma, fit_BGEV$delta))
envelope_BGEV2(y, X, c(fit_BGEV2$beta, fit_BGEV2$sigma,fit_BGEV2$xi ,fit_BGEV2$delta))


