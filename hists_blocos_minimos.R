library(tidyverse)
library(readr)
library(lubridate)
library(stringr)

source("BGEV_functions.R")


### Leitura dos dados originais 

df <- readRDS('D:\\Users\\Mathews\\Documents\\Git\\mestrado_unb/dados_resumidos/dados_originais.rds')

### criação dos dados por média de tempo de orvalho 

temp_media <- df  %>% group_by(Data) %>% summarise(media_temp = mean(TEMPERATURA.DO.PONTO.DE.ORVALHO...C., na.rm = T))

### bloco minimos, definindop tamanho do bloco 

hist(temp_media$media_temp , main = 'Dados diários sem bloco', probability = T)
lines(density(temp_media$media_temp))
plot(temp_media$media_temp, type = 'l')
acf(temp_media$media_temp)


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


### histogramas para diversos tamanhos de blocos minimos de fato 

fns <- c(15,30,60,90,120,150,180,360)


for( i in 1:length(fns)) {
  
  n<-fns[i]
  N<-length(HIGH)
  
  HIGH <- temp_media$media_temp
  N<-length(HIGH)  ; 
  tau<-floor(N/n)
  mi<-numeric(tau) ; j<-1
  for (i in 1:tau){
    mi[i]<-min(HIGH[j:(j+n-1)])
    j<-j+n }
  
  hist(mi, probability = T)
  lines(density(mi))
  
  
  bin <- 0 
  if(n<90){
    bin <- 30
  }else if(n>=90 & n<180){
    bin <- 15
  }else{
    bin <- 5
  }
  
  df <- data.frame(x=mi)
  
  #temp_min_orvalho <- mi
  # saveRDS(mi, file = 'D:\\Users\\Mathews\\Documents\\Git\\mestrado_unb\\dados_resumidos\\temperatura_min_orvalho.rds')
  
  ggplot(df,aes(x=x))+
    geom_histogram(aes(y = ..density..),colour='white',fill='#696969',bins = bin)+
    geom_density(lwd=1, 
                 linetype = 1, 
                 colour = 'black')+
    labs(x='m',y='', color='')+
    scale_color_manual(values=colors)+
    theme_bw()+
    theme(axis.title.y=element_text(colour='black',size=12),
          axis.title.x=element_text(colour='black',size=12),
          axis.text=element_text(colour='black',size=9.5),
          panel.border=element_blank(),
          axis.line=element_line(colour='black'),
          legend.position = 'top')
  
  ggsave(paste0('imagens_artigo\\histogramas_cira\\fn',n,'.png'),
         width=158,height=93,units='mm')
}

