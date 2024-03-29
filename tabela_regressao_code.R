library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

## leitura dos dados originais ##### 
df <- readRDS(file = 'D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/dados_originais.rds')

pct_nas <- function(x){
  return(sum(is.na(x))/length(x))
}


sapply(df, pct_nas)
temp_media <- df  %>% group_by(Data) %>% summarise(media_temp = mean(TEMPERATURA.DO.PONTO.DE.ORVALHO...C., na.rm = T))
chuva_diaria <- df %>% group_by(Data) %>% summarise(chuva = sum(PRECIPITAÇÃO.TOTAL..HORÁRIO..mm., na.rm=T))
radiacao_total <- df %>% group_by(Data) %>% summarise(radiacao= sum(`RADIACAO.GLOBAL..Kj.m².`, na.rm = T))


### triando os blocos mínimos de tamanho 60

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

HIGH <- temp_media$media_temp
N<-length(HIGH)  ; n<-60
tau<-floor(N/n)
mi<-numeric(tau) ; j<-1
pos<-numeric(tau)

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)])
  pos[i] <- which(HIGH[j:(j+n-1)]==min(HIGH[j:(j+n-1)]))
  j<-j+n 
}


tabela_media_diaria <- data.frame(Data = sort(unique(df$Data)))

for(j in 3:19){
 aux <-  df  %>% group_by(Data) %>% summarise(mean(eval(as.symbol(names(df)[j])), na.rm = T))
 tabela_media_diaria[,j-1] <- aux[,2]
  
}

nomes <- names(df)[3:19]

nomes[1] <- 'Precipitacao_media';nomes[2] <- 'Pressao_media';nomes[3] <- 'Pressao_max';nomes[4] <- 'Pressao_min'
nomes[5] <- 'Radiacao_global';nomes[6] <- 'Temperatura_bulbo';nomes[7] <- 'Temperatura_orvalho';nomes[8] <- 'Temperatura_max_bulbo'
nomes[9] <- 'Temperatura_min_bulbo';nomes[10] <- 'Temperatura_max_orvalho';nomes[11] <- 'Temperatura_min_orvalho';nomes[12] <- 'Umidade_rel_max'
nomes[13] <- 'Umidade_rel_min';nomes[14] <- 'Umidade_rel';nomes[15] <- 'Ventro_direcao';nomes[16] <- 'Vento_rajada_max'
nomes[17] <- 'Vento_velocidade'


names(tabela_media_diaria)[2:18] <- nomes

pos2 <- pos+seq(0,(n*length(pos))-1,by=n)

chuva <- chuva_diaria$chuva[pos2]

tabela_regressao %>% nrow()

tabela_regressao <- tabela_media_diaria[pos2,]

tabela_regressao <-  tabela_regressao %>% select(Data,Temperatura_orvalho,Precipitacao_media,Pressao_media,Pressao_max,
                                                 Pressao_min,Radiacao_global,Temperatura_bulbo,Temperatura_orvalho,Temperatura_max_bulbo,
                                                 Temperatura_min_bulbo, Temperatura_max_orvalho,Temperatura_min_orvalho,Umidade_rel_max,
                                                                                                  Umidade_rel_min,Umidade_rel,Ventro_direcao,Vento_rajada_max,Vento_velocidade)
names(tabela_regressao)

### Salvando media diária e tambéma tabela de regressão com todas as 18 covariáveis 

saveRDS(tabela_media_diaria,file = 'D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/tabela_media_diaria.rds')

saveRDS(tabela_regressao,file = 'D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/tabela_regressao.rds')

write.csv2(tabela_regressao,file = 'D://Users/Mathews/Documents/UNB_mestrado/tabela_regressao.csv')

tabela_regressao$Temperatura_orvalho %>% summary()
