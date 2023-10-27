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

madrugada <- c('0300 UTC','0400 UTC','0500 UTC', '0600 UTC','0700 UTC','0800 UTC')
manha <- c('0900 UTC', '1000 UTC', '1100 UTC', '1200 UTC', '1300UTC', '1400 UTC')
tarde <- c('1500 UTC', '1600 UTC', '1700 UTC', '1800 UTC', '1900 UTC', '2000 UTC')
noite <- c('2100 UTC', '2200 UTC', '2300 UTC', '0100 UTC', '0200 UTC')
df$Hora.UTC %>% unique() %>% length()

df[which(df$Hora.UTC %>% str_extract(':') %>% is.na()==F),1] %>% year() %>% unique()

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

df$TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C. %>% hist(breaks = seq(-5,25,1))

df$TEMPERATURA.DO.PONTO.DE.ORVALHO...C. %>% hist(breaks= seq(-10,30,1))

df$TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C. %>% hist(breaks=seq(-10,50,1))

df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C. %>% hist(breaks=seq(-10,50,1))

df$TEMPERATURA.MÍNIMA.NA.HORA.ANT...AUT....C. %>% hist(breaks=seq(-10,50,1))

df$`RADIACAO.GLOBAL..Kj.m².` %>% hist(30)

df$VENTO..VELOCIDADE.HORARIA..m.s. %>% hist()

df$UMIDADE.REL..MAX..NA.HORA.ANT...AUT..... %>% hist()

df$UMIDADE.REL..MIN..NA.HORA.ANT...AUT..... %>% hist()

df$VENTO..DIREÇÃO.HORARIA..gr......gr.. %>% hist(30)

df$VENTO..RAJADA.MAXIMA..m.s. %>% hist(20)

df$VENTO..VELOCIDADE.HORARIA..m.s. %>% hist()

df$PRESSAO.ATMOSFERICA.AO.NIVEL.DA.ESTACAO..HORARIA..mB. %>% hist(30)

df$PRESSÃO.ATMOSFERICA.MIN..NA.HORA.ANT...AUT...mB. %>% hist(30)

df$PRESSÃO.ATMOSFERICA.MAX.NA.HORA.ANT...AUT...mB. %>% hist(30)

temp2 <- df  %>% group_by(Data) %>% summarise(max_orvalho = max(TEMPERATURA.ORVALHO.MAX..NA.HORA.ANT...AUT....C., na.rm = T))

temp2$max_orvalho %>% hist(20)

temp2 <- df  %>% group_by(Data) %>% summarise(max_orvalho = max(TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C., na.rm = T))

temp2$max_orvalho %>% hist(20)



which(result$X0>0.05) %>% min()

n <- 2
N <- length(temp2$max_orvalho)
High <- df$TEMPERATURA.MÁXIMA.NA.HORA.ANT...AUT....C.
N<-length(High)
result <- data.frame() 

for (k in 1:10000) {  
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
plot(MI, type="l",main="Some Extremes")

temp3 <- df  %>%  group_by(mes = month(Data)) %>% summarise(sum_pre= sum(PRECIPITAÇÃO.TOTAL..HORÁRIO..mm., na.rm = T),
                                                   max_pre= max(PRECIPITAÇÃO.TOTAL..HORÁRIO..mm., na.rm = T))
temp3 <- temp3[ - which(temp3$sum_pre<=5),]

hist(temp3$sum_pre)
hist(temp3$max_pre)
plot(temp3$sum_pre)
barplot(temp3$sum_pre)

