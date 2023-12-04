library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

ler_inmet <- function(x){
  read.csv2(x,skip = 8) %>% add_column(UF = str_split(x, '_')[[1]][3],
                                       ESTACAO = str_split(x, '_')[[1]][4],
                                       CIDADE = str_split(x, '_')[[1]][5]) %>% subset(select=-20)}

##S704 === EStação de BONITO ms
##A002 GOIÂNIA 
##A001 Brasília
##A899 Santa Palma RS (centro Eólico)
##A426 guanambi (centro eólico)

setwd('D:\\Users\\Mathews\\Documents\\UNB_mestrado\\Copulas\\dados_inmet')

ESTACAO <- 'A002' ### escolhe a estação que você quer ler 
anos <- c(2022:2011)
df <- data.frame()
nomes <- c("Data","Hora.UTC","PRECIPITAÇÃO.TOTAL..HORÁRIO..mm.","PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB.","PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB." 
           ,"PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB.","RADIACAO.GLOBAL..Kj.m²."                              
           ,"TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.","TEMPERATURA.DO.PONTO.DE.ORVALHO...C."                 
           ,"TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C.","TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C."           
           ,"TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C.","TEMPERATURA.ORVALHO.MIN..NA.HORA.ANT...AUT....C."     
           ,"UMIDADE.REL..MAX..NA.HORA.ANT...AUT.....","UMIDADE.REL..MIN..NA.HORA.ANT...AUT....."             
           ,"UMIDADE.RELATIVA.DO.AR..HORARIA....","VENTO..DIREÇÃO.HORARIA..gr......gr..","VENTO..RAJADA.MAXIMA..m.s."                           
           ,"VENTO..VELOCIDADE.HORARIA..m.s."                      
           ,"UF","ESTACAO","CIDADE")


for(i in 1:length(anos)){
lista_files <- list.files(path = str_c("D:\\Users\\Mathews\\Documents\\UNB_mestrado\\Copulas\\dados_inmet\\",anos[i]), pattern = ESTACAO)
lista <- lapply(str_c(anos[i],'\\',lista_files),ler_inmet)
temp <- do.call(rbind,lista)
names(temp) <- nomes
temp$Data <- as.Date(temp$Data)
df <- rbind(df, temp)
}

# coloca_na <- function(x){
#   x <- ifelse(x==-9999.0,NA, x)
#   x
# }

df$Data %>% year() %>% unique()

df$PERIODO_DIA <- df$Hora.UTC


df$PRECIPITAÇÃO.TOTAL..HORÁRIO..mm.[df$PRECIPITAÇÃO.TOTAL..HORÁRIO..mm. == -9999.0] <- NA
df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB.[df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB. == -9999.0] <- NA
df$PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB.[df$PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB. == -9999.0] <- NA
df$PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB.[df$PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB. == -9999.0] <- NA

df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C.[df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C. == -9999.0] <- NA
df$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C.[df$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C. == -9999.0] <- NA
df$TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C.[df$TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C. == -9999.0] <- NA
df$TEMPERATURA.ORVALHO.MIN..NA.HORA.ANT...AUT....C.[df$TEMPERATURA.ORVALHO.MIN..NA.HORA.ANT...AUT....C. == -9999.0] <- NA

df$UMIDADE.REL..MAX..NA.HORA.ANT...AUT.....[df$UMIDADE.REL..MAX..NA.HORA.ANT...AUT..... == -9999.0] <- NA
df$UMIDADE.REL..MIN..NA.HORA.ANT...AUT.....[df$UMIDADE.REL..MIN..NA.HORA.ANT...AUT.....== -9999.0] <- NA
df$UMIDADE.RELATIVA.DO.AR..HORARIA....[df$UMIDADE.RELATIVA.DO.AR..HORARIA.... == -9999.0] <- NA
df$VENTO..DIREÇÃO.HORARIA..gr......gr..[df$VENTO..DIREÇÃO.HORARIA..gr......gr..== -9999.0] <- NA
df$VENTO..RAJADA.MAXIMA..m.s.[df$VENTO..RAJADA.MAXIMA..m.s. == -9999.0] <- NA

df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C.[df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C. == -9999.0] <- NA
df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.[df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C. == -9999.0] <- NA
df$`RADIACAO.GLOBAL..Kj.m².`[df$`RADIACAO.GLOBAL..Kj.m².` == -9999.0] <- NA
df$VENTO..VELOCIDADE.HORARIA..m.s.[df$VENTO..VELOCIDADE.HORARIA..m.s. == -9999.0] <- NA

which(is.na(df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C.)==T)

df$TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C. %>% hist()

df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C. %>% hist()

df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C. %>% hist()

df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C. %>% hist()

# df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C. %>% density() %>% plot()

df$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C. %>% hist(breaks=seq(-10,50,1))

df$`RADIACAO.GLOBAL..Kj.m².` %>% hist()

df$VENTO..VELOCIDADE.HORARIA..m.s. %>% hist()

df$UMIDADE.REL..MAX..NA.HORA.ANT...AUT..... %>% hist()

df$UMIDADE.REL..MIN..NA.HORA.ANT...AUT..... %>% hist()

df$VENTO..DIREÇÃO.HORARIA..gr......gr.. %>% hist(30)

df$VENTO..RAJADA.MAXIMA..m.s. %>% hist(20)

df$VENTO..VELOCIDADE.HORARIA..m.s. %>% hist()

df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB. %>% hist(30)

df$PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB. %>% hist(30)

df$PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB. %>% hist(30)

############################# TENTATIVA DENSIDADE DO AR ###################################
Temperatura <- df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C.

#### Equação de Tentens para Densidade do ar #### 
p1 <- 6.1078*10^(7.5*Temperatura/ (Temperatura + 237.3))

pv <- p1*df$UMIDADE.RELATIVA.DO.AR..HORARIA..../100

pd <- df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB. - pv

Temp_kelvin <- Temperatura+273.15
RD <- 287.058 
RV <- 461.495 
air_density <- (pd/(RD*Temp_kelvin))+(pv/(RV*Temp_kelvin))

(pd/(RD*Temp_kelvin))+(pv/(RV*Temp_kelvin)) %>% hist()
(pd/(RD*Temp_kelvin))+(pv/(RV*Temp_kelvin)) %>% density(na.rm=T) %>% plot()


df$DENSIDADE_DO_AR <- air_density

temp2 <- df  %>% group_by(Data) %>% summarise(max_orvalho = max(DENSIDADE_DO_AR, na.rm = T))

temp2$max_orvalho %>% hist(20)

temp2$max_orvalho %>% density() %>% plot()

#####################################################################################################
####################### VELOCIDADE DO AR ####################

hist(df$UMIDADE.RELATIVA.DO.AR..HORARIA....)
temp2 <- df  %>% group_by(Data) %>% summarise(max_orvalho = max(UMIDADE.REL..MAX..NA.HORA.ANT...AUT....., na.rm = T))
temp2$max_orvalho %>% hist(20)
temp2$max_orvalho %>% density() %>% plot()

############# tEMPERATUA MÁXIMA ######### 

temp2 <- df  %>% group_by(Data) %>% summarise(max_orvalho = max(TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C., na.rm = T))

temp2$max_orvalho %>% hist(20)

which(result$X0>0.05) %>% min()

n <- 2
N <- length(temp2$max_orvalho)
High <- temp2$max_orvalho
N<-length(High)
result <- data.frame() 

for (k in 1:100) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(High[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}

result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")


High <- temp2$max_orvalho
N<-length(High)  ; n<-65
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)])
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
pacf(MI)
hist(MI)
plot(density(MI))

##### Isso aqui é temperatura máxima na hora ###### 
result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")

High <- df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C.
N<-length(High)  ; n<-1360
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)])
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
pacf(MI)
hist(MI,20)
plot(density(MI))
#plot(MI, type="l",main="Some Extremes")

temp3 <- df  %>%  group_by(mes = month(Data)) %>% summarise(sum_pre= sum(PRECIPITAÇÃO.TOTAL..HORÁRIO..mm., na.rm = T),
                                                   max_pre= max(PRECIPITAÇÃO.TOTAL..HORÁRIO..mm., na.rm = T))
temp3 <- temp3[ - which(temp3$sum_pre<=5),]

hist(temp3$sum_pre)
hist(temp3$max_pre)
plot(temp3$sum_pre)
barplot(temp3$sum_pre)

#################### Trabalhando com Retornos da Temperatura ######### 


#df$TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C.

variacao <- temp2$max_orvalho/lag(temp2$max_orvalho) 
plot(variacao, type = 'l')
hist(variacao)

temp2 <- df  %>% group_by(Data) %>% summarise(max_orvalho = max(TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C., na.rm = T))




n <- 2
N <- length(temp2$max_orvalho)
High <- temp2$max_orvalho
result <- data.frame() 

for (k in 1:100) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(High[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}

result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")


High <- temp2$max_orvalho
N<-length(High)  ; n<-65
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)])
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
pacf(MI)
hist(MI,20)
plot(density(MI))


HIGH <- temp2$max_orvalho
N<-length(High)  ; n<-65
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)])
  j<-j+n }

hist(mi,20)
plot(density(mi))

hist(c(mi,MI))
plot(density(c(mi,MI)))

max(MI)
min(mi)

############ Temperatura Média depois tirando os blocos máximos e mínimos ###############

temp_media <- df  %>% group_by(Data) %>% summarise(media_temp = mean(TEMPERATURA.DO.PONTO.DE.ORVALHO...C., na.rm = T))
n <- 2
N <- length(temp_media$media_temp)
High <- temp_media$media_temp
result <- data.frame() 

for (k in 1:100) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(High[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}

result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")


High <- temp_media$media_temp
N<-length(High)  ; n<-60
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)])
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
pacf(MI)
hist(MI,20)
plot(density(MI))


HIGH <- temp_media$media_temp
N<-length(HIGH)  ; n<-60
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)])
  j<-j+n }

hist(mi)
plot(density(mi))

hist(c(mi,MI))
plot(density(c(mi,MI)))
max(MI)
min(mi)

##################### Usando Temperaturas sem fazer o diário ####################################

temp_orvalho <- df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C.
HIGH <- temp_orvalho
N<-length(HIGH);n<-60*24
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
plot(density(mi))


temp_orvalho <- df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C.
HIGH <- temp_orvalho
N<-length(HIGH)  ; n<-60*24
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI)
plot(density(MI))

hist(c(mi,MI))
plot(density(c(mi,MI)))

################ Outras Temp Bulbo Seco blocos 1440  ##################

require(forecast)

hist(df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.)
# plot(density(df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.,na.rm = T))
# temp_max <- df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.
# ndiffs(temp_max)
#d_temp <- diff(temp_max)
# is.na(d_temp) %>% sum()
# d_temp <- d_temp[is.na(d_temp)==FALSE]
# 
# hist(d_temp)
# acf(d_temp)
# plot(d_temp, type='l')
# d_temp <- diff(d_temp)
# acf(d_temp)

HIGH <- d_temp
N<-length(HIGH);n<-24
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

acf(mi)
hist(mi)
plot(density(mi))

HIGH <- d_temp
N<-length(HIGH)  ; n<-24
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

acf(MI)
hist(MI)
plot(density(MI))

hist(c(mi,MI),20)
plot(density(c(mi,MI)))

######## Velocidade Do Vento Horário, agrupando primeiramente ##############


vento <- df$VENTO..VELOCIDADE.HORARIA..m.s.

### media
vento <- df %>% group_by(dia = Data) %>% summarise(velocidade = mean(VENTO..VELOCIDADE.HORARIA..m.s., na.rm = T))

n <- 2
N <- length(vento$velocidade)
High <- vento$velocidade
result <- data.frame() 

for (k in 1:100) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(High[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}

result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")

N<-length(HIGH);n<-75
tau<-floor(N/n)
HIGH <- vento$velocidade
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))


N<-length(HIGH);n<-60
tau<-floor(N/n)
HIGH <- vento$velocidade
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
acf(MI)
plot(density(MI))

#### misturando máximo e mínimo na média 
hist(c(mi,MI))
plot(density(c(mi,MI)))


### usando velocidade do vento diretamente da variável horária 
vento <- df$VENTO..VELOCIDADE.HORARIA..m.s.

N<-length(HIGH);n<-60*24
tau<-floor(N/n)
HIGH <- vento
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))


N<-length(HIGH);n<-60*24
tau<-floor(N/n)
HIGH <- vento
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
acf(MI)
plot(density(MI))

#### mistura, ficou mto ruim 
hist(c(mi,MI))

############ Vento rajada máxima agora #########

vento <- df$VENTO..RAJADA.MAXIMA..m.s.

vento <- df %>% group_by(dia  = Data) %>% summarise(rajada = max(VENTO..RAJADA.MAXIMA..m.s.,na.rm = T))

n <- 2
N <- length(vento$rajada)
High <- vento$rajada
result <- data.frame() 

for (k in 1:100) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(High[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}

result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")

N<-length(HIGH);n<-60
tau<-floor(N/n)
HIGH <- vento$rajada
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))


N<-length(HIGH);n<-60
tau<-floor(N/n)
HIGH <- vento$rajada
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
acf(MI)
plot(density(MI))

#### misturando máximo e mínimo na média 
hist(c(mi,MI))
plot(density(c(mi,MI)))


### usando velocidade do vento diretamente da variável horária 
vento <- df$VENTO..RAJADA.MAXIMA..m.s.

N<-length(HIGH);n<-60*24
tau<-floor(N/n)
HIGH <- vento
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))


N<-length(HIGH);n<-60*24
tau<-floor(N/n)
HIGH <- vento
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
acf(MI)
plot(density(MI))

#### mistura, ficou mto ruim 
hist(c(mi,MI)) #### fica horrível 




##### Umidade relativa do AR, detalhe está em % ########

#### Media / horárria 
umidade <- df$UMIDADE.RELATIVA.DO.AR..HORARIA....
temp <- df %>% group_by(dia = Data) %>% summarise(umidade = mean(UMIDADE.RELATIVA.DO.AR..HORARIA...., na.rm = T))

n <- 2
N <- length(temp$umidade)
High <- temp$umidade
result <- data.frame() 

for (k in 1:100) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(High[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}

result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")

HIGH <- temp$umidade
N<-length(HIGH);n<-60
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))


HIGH <- temp$umidade
N<-length(HIGH);n<-60
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
acf(MI)
plot(density(MI))


#### mistura de max e minimo

hist(c(mi,MI))
plot(density(c(mi,MI)))
#### ainda não entendi como fariamos pra modelar esse gráfico

#### umnidade diretamente da variável horária 


HIGH <- umidade
N<-length(HIGH);n<-60*24
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))


HIGH <- umidade
N<-length(HIGH);n<-60*24
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }


###ficou horr´vel isso aqui 
hist(MI,10)
acf(MI)
plot(density(MI))


#### mistura de max e minimo

hist(c(mi,MI)) #### nada a ver
plot(density(c(mi,MI)))


### importante tentar ver o que ocorre com o umiodade relativa máxiam e mínima

temp <- df %>% group_by(dia = Data) %>% summarise(umidade = min(UMIDADE.REL..MIN..NA.HORA.ANT...AUT....., na.rm = T))

HIGH <- temp$umidade
N<-length(HIGH);n<-60
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
plot(density(mi))


#### relativa máxima 
temp <- df %>% group_by(dia = Data) %>% summarise(umidade = max(UMIDADE.REL..MAX..NA.HORA.ANT...AUT....., na.rm = T))

HIGH <- temp$umidade
N<-length(HIGH);n<-60
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
plot(density(MI))

### Pressão atmosférica #######


pressao <- df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB.
temp <- df %>% group_by(dia = Data) %>% summarise(pressao = mean(PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB., na.rm = T))

HIGH <- temp$pressao
N <- length(HIGH); n <- 60
tau<-floor(N/n)
mi<-numeric(tau);j<-1
for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))

HIGH <- temp$pressao
N<-length(HIGH);n<-60
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
plot(density(MI))
acf(MI)


hist(c(mi,MI))
plot(density(c(mi,MI)))


####### Pressão atmosfera direto do df horário

pressao <- df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB.

HIGH <- pressao
N <- length(HIGH); n <- 60*24
tau<-floor(N/n)
mi<-numeric(tau);j<-1
for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))


HIGH <- pressao
N<-length(HIGH);n<-60*24
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
plot(density(MI))
acf(MI)


########## Pressão atmosféricas mínima

temp <- df %>% group_by(dia = Data) %>% summarise(pressao = min(PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB., na.rm = T))

HIGH <- temp$pressao
N <- length(HIGH); n <- 60
tau<-floor(N/n)
mi<-numeric(tau);j<-1
for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
acf(mi)
plot(density(mi))




temp <- df %>% group_by(dia = Data) %>% summarise(pressao = max(PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB., na.rm = T))

HIGH <- temp$pressao
N <- length(HIGH); n <- 60
tau<-floor(N/n)
MI<-numeric(tau);j<-1
for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
plot(density(MI))
acf(MI)



