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

### fazendo a media diaria da umidadade relativa
temp <- df %>% group_by(dia = Data) %>% summarise(umidade = mean(UMIDADE.RELATIVA.DO.AR..HORARIA...., na.rm = T))

#### Tirando os mínimos quadrados para umidade relativa 
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
pos<-numeric(tau)

for (i in 1:tau){
  mi[i]<-min(HIGH[j:(j+n-1)],na.rm = T)
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


names(tabela_media_diaria)[2:18] <- nomes

pos2 <- pos+seq(0,(n*length(pos))-1,by=n)

tabela_regressao %>% nrow()

tabela_regressao <- tabela_media_diaria[pos2,]

tabela_regressao_umidade <-  tabela_regressao %>% select(Data,Temperatura_orvalho,Precipitacao_media,Pressao_media,Pressao_max,
                                                 Pressao_min,Radiacao_global,Temperatura_bulbo,Temperatura_orvalho,Temperatura_max_bulbo,
                                                 Temperatura_min_bulbo, Temperatura_max_orvalho,Temperatura_min_orvalho,Umidade_rel_max,
                                                 Umidade_rel_min,Umidade_rel,Ventro_direcao,Vento_rajada_max,Vento_velocidade)
names(tabela_regressao_umidade)


## finalmente salvando o trabalho final

saveRDS(tabela_regressao_umidade,file = 'D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/tabela_regressao_umidade.rds')

write.csv2(tabela_regressao_umidade,file = 'D://Users/Mathews/Documents/UNB_mestrado/tabela_regressao_umidade.csv')

tabela_regressao_umidade$Temperatura_orvalho %>% summary()



