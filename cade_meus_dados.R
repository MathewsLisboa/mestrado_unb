library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

ler_inmet <- function(x){
  read.csv2(x,skip = 8) %>% add_column(UF = str_split(x, '_')[[1]][3],
                                       ESTACAO = str_split(x, '_')[[1]][4],
                                       CIDADE = str_split(x, '_')[[1]][5]) %>% subset(select=-20)}

##S704 === Estação de BONITO ms
##A002 GOIÂNIA 
##A001 Brasília
##A899 Santa Palma RS (centro Eólico)
##A426 Guanambi (centro eólico)


#### leitura de dados ####

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




#### Equação de Tentens para Densidade do ar ####
Temperatura <- df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C.
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

saveRDS(df, file = 'D:\\Users\\Mathews\\Documents\\Git\\mestrado_unb/dados_resumidos/dados_originais.rds')


#### TEMPERATURA BULBO SECO MÉDIA SEM DIÁRIA ANTES ####

hist(df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.)

High <- df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.
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


High <- df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.
N<-length(High)  ; n<-1360
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)])
  j<-j+n }

mi <- mi[is.na(mi)==F]
hist(mi)
plot(density(mi), main = '')

## mistura
hist(c(mi,MI))
plot(density(c(mi,MI))) ## acho que ficou bem ruim

#### TEMPERATURA BULBO SECO MÉDIO COM DIÁRIA ANTES ####

temp <- df %>% group_by(Data= Data) %>% summarise(temp = mean(TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C., na.rm = T))

n <- 2
N <- length(temp$temp)
High <- temp$temp
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



High <- temp$temp
N<-length(High)  ; n<-65
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)])
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
hist(MI,20)
plot(density(MI))

High <- temp$temp
N<-length(High)  ; n<-65
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)])
  j<-j+n }

mi <- mi[is.na(mi)==F]
acf(mi)
hist(mi)
plot(density(mi))

### mistura 
hist(c(mi,MI))
plot(density(c(mi, MI)))

#### TEMPERATURA BULBO SECO MÁXIMA SEM FAZER DIÁRIA ANTES #### 
hist(df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C.)
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


High <- df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C.
N<-length(High)  ; n<-1360
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)])
  j<-j+n }

mi <- mi[is.na(mi)==F]
acf(mi)
hist(mi)
plot(density(mi))

#### mistura
hist(c(mi,MI))
plot(density(c(mi,MI))) ### acho que ficou ruim 

#### TEMPERATURA BULBO SECO MÁXIMA COM DIÁRIA ####

temp <- df %>% group_by(Data) %>% summarise(temp = max(TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C.,na.rm = T))

High <- temp$temp
N<-length(High)  ; n<-60
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)])
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
hist(MI)
plot(density(MI))



High <- temp$temp
N<-length(High)  ; n<-60
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)])
  j<-j+n }

mi <- mi[is.na(mi)==F]
acf(mi)
hist(mi)
plot(density(mi))

#### mistura
hist(c(mi,MI))
plot(density(c(mi,MI))) ### acho que ficou ruim 



#### TEMPERATURA BULBO SECO MÍNIMA SEM FAZER DIÁRIA ANTES ####

hist(df$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C.)

High <- df$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C.
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


High <- df$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C.
N<-length(High)  ; n<-1360
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)])
  j<-j+n }

mi <- mi[is.na(mi)==F]
acf(mi)
par(mfrow=c(2,1))
hist(mi)
plot(density(mi))

#### mistura
hist(c(mi,MI))
plot(density(c(mi,MI))) ### acho que ficou ruim 


#### TEMPERATURA BULBO SECO MINIMA COM DIÁRIA ####
temp <- df %>% group_by(Data) %>% summarise(temp = min(TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C., na.rm = T))

hist(temp$temp)

High <- temp$temp
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


High <- temp$temp
N<-length(High)  ; n<-60
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)])
  j<-j+n }

mi <- mi[is.na(mi)==F]
acf(mi)

hist(mi)
plot(density(mi))

#### mistura
hist(c(mi,MI))
plot(density(c(mi,MI))) ### acho que ficou ruim 




#### TEMPERATURA DO ORVARLHO SEM FAZER DIÁRIA ####


hist(temp_orvalho)
plot(temp_orvalho, type='l')
acf(temp_orvalho[is.na(temp_orvalho)==F])


temp_orvalho <- df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C.
HIGH <- temp_orvalho
N<-length(HIGH);n<-1360
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

acf(mi)
hist(mi,10, main = 'TEMP MÍNIMA DO ORVALHO DIÁRIA BLOCOS DE 1360')
plot(density(mi), main='')


temp_orvalho <- df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C.
HIGH <- temp_orvalho
N<-length(HIGH)  ; n<-1360
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI)
plot(density(MI))

hist(c(mi,MI))
plot(density(c(mi,MI)))



#### TEMPERATURA PONTO DO ORVALHO MÉDIA COM DIÁRIA ####


preciptacao_media <- df %>% group_by(Data) %>% summarise(preciptacao_media = mean(PRECIPITAÇÃO.TOTAL..HORÁRIO..mm.))

temp_media <- df  %>% group_by(Data) %>% summarise(media_temp = mean(TEMPERATURA.DO.PONTO.DE.ORVALHO...C., na.rm = T))

saveRDS(temp_media, file = 'D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/temp_media_diaria.rds')

hist(temp_media$media_temp)
plot(temp_media$media_temp, type = 'l')
acf(temp_media$media_temp)


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

temp_min_orvalho <- mi
saveRDS(mi, file = 'D:\\Users\\Mathews\\Documents\\Git\\mestrado_unb\\dados_resumidos\\temperatura_min_orvalho.rds')

hist(mi, main = 'TEMP MÍNIMA ORVALHO DO MÍNIMO DIÁRIO BLOCO 60', probability = T)
acf(mi)
plot(density(mi), main='')

hist(c(mi,MI))
plot(density(c(mi,MI)))
max(MI)
min(mi)


#### TEMPERATURA PONTO DO ORVALHO MÁXIMA SEM DIÁRIA ####

High <- df$TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C.

N<-length(High)  ; n<-1360
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)], na.rm = T)
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
hist(MI,20)
plot(density(MI))


High <- df$TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C.
N<-length(High)  ; n<-1360
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)], na.rm = T)
  j<-j+n }

hist(mi,20)
plot(density(mi))


hist(c(mi,MI))
plot(density(c(mi,MI)))

max(MI)
min(mi)

#### TEMPERATURA PONTO DO ORVALHO MÁXIMO COM DIÁRIA ####

temp <- df  %>% group_by(Data) %>% summarise(max_orvalho = max(TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C., na.rm = T))
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
#### TEMPERATURA PONTO DO ORVALHO MÍNIMO SEM DIÁRIA ####

High <- df$TEMPERATURA.ORVALHO.MIN..NA.HORA.ANT...AUT....C.

N<-length(High)  ; n<-1360
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)], na.rm = T)
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
hist(MI,20)
plot(density(MI))

High <- df$TEMPERATURA.ORVALHO.MIN..NA.HORA.ANT...AUT....C.
N<-length(High)  ; n<-1360
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)], na.rm = T)
  j<-j+n }

hist(mi,20, main = 'MÍNIMO DA TEMP MÍNIMA DO ORVALHO BLOCOS DE 1360')
plot(density(mi))

hist(c(mi,MI))
plot(density(c(mi,MI)))

#### TEMPERATURA PONTO DO ORVALHO MÍNIMO COM DIÁRIA ####

temp <- df %>% group_by(Data) %>% summarise(temp = min(TEMPERATURA.ORVALHO.MIN..NA.HORA.ANT...AUT....C., na.rm = T))

High <- temp$temp
  
N<-length(High)  ; n<-60
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)], na.rm = T)
  j<-j+n }

MI <- MI[is.na(MI)==F]
acf(MI)
hist(MI,20)
plot(density(MI))

High <- temp$temp
N<-length(High)  ; n<-60
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)], na.rm = T)
  j<-j+n }

hist(mi,20)
plot(density(mi))

hist(c(mi,MI))
plot(density(c(mi,MI)))

#### VELOCIDA DO VENTO M/S COM DIÁRIA ####

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

N<-length(High);n<-90
tau<-floor(N/n)
High <- vento$velocidade
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(High[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


N<-length(High);n<-90
tau<-floor(N/n)
High <- vento$velocidade
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(High[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
plot(density(MI))

#### VELOCIDADE DO VENTO SEM DIÁRIA ####

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

#### VENTO RAJADA MÁXIMA COM DIÁRIA ####

vento <- df %>% group_by(dia  = Data) %>% summarise(rajada = max(VENTO..RAJADA.MAXIMA..m.s.,na.rm = T))

saveRDS(vento, file='D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/vento_diario.rds')


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

N<-length(HIGH);n<-56
tau<-floor(N/n)
HIGH <- vento$rajada
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi)
acf(mi)
plot(density(mi))


N<-length(HIGH);n<-56
tau<-floor(N/n)
HIGH <- vento$rajada
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

acf(MI)
hist(MI)
plot(density(MI))

saveRDS(MI, file='D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/vento_max.rds')

#### misturando máximo e mínimo na média 
hist(c(mi,MI))
plot(density(c(mi,MI)))


#### VENTO RAJADA SEM DIÁRIA ####
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









#### UMIDADE DO AR COM DIÁRIO ####

temp <- df %>% group_by(dia = Data) %>% summarise(umidade = mean(UMIDADE.RELATIVA.DO.AR..HORARIA...., na.rm = T))

saveRDS(temp, file='D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/umidade_diaria.rds')

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


#saveRDS(mi, file = 'D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/umidade_minima.rds')
hist(mi,10, main = 'BLOCOS MÍNIMO UMIDADE RELATIVA DO AR MÉDIA DIÁRIA')
#acf(mi)
plot(density(mi), main = '')

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
#### UMIDADE DO AR SEM DIÁRIO ####
HIGH <- df$UMIDADE.RELATIVA.DO.AR..HORARIA....
N<-length(HIGH);n<-60*24
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10, main = 'BLOCOS MÍNIMO UMIDADE RELATIVA DO AR NO HORARIO')
#acf(mi)
plot(density(mi), main='')


HIGH <- umidade
N<-length(HIGH);n<-60*24
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }


###ficou horr´vel isso aqui 
hist(MI,10)
#acf(MI)
plot(density(MI))
#### mistura de max e minimo

hist(c(mi,MI)) #### nada a ver
plot(density(c(mi,MI)))



#### UMIDADE DO AR MÁXIMA COM DIÁRIO ####

temp <- df %>% group_by(Data) %>% summarise(umidade=max(UMIDADE.REL..MAX..NA.HORA.ANT...AUT....., na.rm = T))

HIGH <- temp$umidade
N<-length(HIGH);n<-60
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


HIGH <- temp$umidade
N<-length(HIGH);n<-60
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
#acf(MI)
plot(density(MI))
#### mistura de max e minimo

hist(c(mi,MI))
plot(density(c(mi,MI)))





#### UMIDADE DO AR MÁXIMA SEM DIÁRIO ####

HIGH <- df$UMIDADE.REL..MAX..NA.HORA.ANT...AUT.....
N<-length(HIGH);n<-1360
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10, main = 'BLOCOS MÍNIMO DA UMIDADE REL MAX DIÁRIA')
#acf(mi)
plot(density(mi))


HIGH <- df$UMIDADE.REL..MAX..NA.HORA.ANT...AUT.....
N<-length(HIGH) ; n<-1360
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






#### UMIDADE DO AR MÍNIMA COM DIÁRIO ####

temp <- df %>% group_by(Data) %>% summarise(umidade = min(UMIDADE.REL..MIN..NA.HORA.ANT...AUT....., na.rm = T))

HIGH <- temp$umidade
N<-length(HIGH);n<-60
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
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

hist(c(mi,MI), main='MISTURA ENTRE BLOCOS MAX E MIN UMIDADE REL DIÁRIA')
plot(density(c(mi,MI)), main = '')

#### UMIDADE DO AR MÍNIMA SEM DIÁRIO ####

HIGH <- df$UMIDADE.REL..MIN..NA.HORA.ANT...AUT.....
N<-length(HIGH);n<-60*24
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


HIGH <- df$UMIDADE.REL..MIN..NA.HORA.ANT...AUT.....
N<-length(HIGH);n<-1360
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
#acf(MI)
plot(density(MI))


#### mistura de max e minimo

hist(c(mi,MI))
plot(density(c(mi,MI)))






#### PRESSÃO ATMOSFÉRICA COM DIÁRIO ####

temp <- df %>% group_by(Data) %>% summarise(pressao = mean(PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB., na.rm = T))

saveRDS(temp, file = 'D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/pressao_diaria.rds')

HIGH <- temp$pressao
N<-length(HIGH);n<-70
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


saveRDS(mi,file = 'D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/pressao_min.rds')

HIGH <- temp$pressao
N<-length(HIGH);n<-70
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
#acf(MI)
plot(density(MI))
#### mistura de max e minimo

hist(c(mi,MI),20)
plot(density(c(mi,MI)))




#### PRESSÃO ATMOSFÉRICA SEM DIÁRIO ####

HIGH <- df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB.
N<-length(HIGH);n<-1360
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


HIGH <- df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB.
N<-length(HIGH);n<-1360
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
#acf(MI)
plot(density(MI))
#### mistura de max e minimo

#hist(c(mi,MI), main = 'MISTURA ENTRE BLOCOS MAX E MIN PRESSAO ATMOSFERICA')
#plot(density(c(mi,MI)), main = '')

#### PRESSÃO ATMOSFÉRICA MÁXIMA COM DIÁRIO ####
temp <- df %>% group_by(Data) %>% summarise(pressao = max(PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB., na.rm = T))


HIGH <- temp$pressao
N<-length(HIGH);n<-60
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


HIGH <- temp$pressao
N<-length(HIGH);n<-60
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
acf(MI)
plot(density(MI))

hist(c(mi,MI))
plot(density(c(mi,MI)))

#### PRESSÃO ATMOSFÉRICA MÁXIMA SEM DIÁRIO ####

HIGH <- df$PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB.
N<-length(HIGH);n<-1360
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


HIGH <- df$PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB.
N<-length(HIGH);n<-1360
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


##### PRESSÃO ATMOSFÉRICA MÍNIMA COM DIÁRIO #### 

temp <- df %>% group_by(Data) %>% summarise(pressao = min(PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB., na.rm = T))

HIGH <- temp$pressao
N<-length(HIGH);n<-60
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


HIGH <- temp$pressao
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

#### PRESSÃO ATMOSFÉRICA MINIMA SEM DIÁRIO ####

HIGH <- df$PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB.
N<-length(HIGH);n<-1360
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
#acf(mi)
plot(density(mi))


HIGH <- df$PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB.
N<-length(HIGH);n<-1360
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


#### Radiação ####### 

radiacao <- df %>% group_by(Data) %>% summarise(radiacao = mean(`RADIACAO.GLOBAL..Kj.m².`, na.rm = T))

hist(as.numeric(radiacao$radiacao) )



n <- 2
N <- length(radiacao$radiacao)
High <- radiacao$radiacao
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


HIGH <- radiacao$radiacao
N<-length(HIGH);n<-60
tau<-floor(N/n)
mi<-numeric(tau);j<-1

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(mi,10)
plot(density(mi))


HIGH <- radiacao$radiacao
N<-length(HIGH);n<-60
tau<-floor(N/n)
MI<-numeric(tau);j<-1

for (i in 1:tau){
  MI[i]<-max(HIGH[j:(j+n-1)],na.rm = T)
  j<-j+n }

hist(MI,10)
plot(density(MI))


#### mistura de max e minimo

hist(c(mi,MI))
plot(density(c(mi,MI)))
  
  