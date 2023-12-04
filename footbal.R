library(tidyverse)
dados_2023 <- read.csv('D:/Users/Mathews/Documents/UNB_mestrado/dados_premier/E0.csv')
dados_2022 <- read.csv('D:/Users/Mathews/Documents/UNB_mestrado/dados_premier/E0 (1).csv')
dados_2021 <- read.csv('D:/Users/Mathews/Documents/UNB_mestrado/dados_premier/E0 (2).csv')



dados <- dados_2023

dados$B365A %>% hist()

dados$B365A %>% density() %>% plot()

dados$B365D %>% density() %>% plot()

dados$MaxD %>% density() %>% plot()
dados$MaxH %>% density() %>% plot()
dados$MaxA %>% density() %>% plot()

c(dados$MaxH,dados$MaxA,dados$MaxD) %>% density() %>% plot()
c(dados$MaxH,dados$MaxA,dados$MaxD) %>% hist(40)



dados$MaxCAHH %>% hist()
dados$MaxCAHA %>% hist()
dados$MaxAHA %>% hist()

dados$MaxCAHH[is.na(dados$MaxCAHH)==F] %>% density() %>% plot()

dados$MaxCAHA[is.na(dados$MaxCAHA)==F] %>% density() %>% plot()




dados$MaxAHH %>% hist()

dados$MaxAHH %>% density() %>% plot()

dados$MaxD %>% hist()

dados$MaxAHH %>% hist()

dados$MaxCAHH %>% hist()

dados$PSCD %>% hist()

dados$MaxCAHA %>% hist()

dados$MaxCAHA %>% density() %>% plot()

dados$B365AHH %>% hist() 

dados$B365AHH %>% density() %>% plot() 

dados$B365AHA %>% density() %>% plot()

dados$B365AHA %>% hist()

dados$MaxCAHA %>% hist()
dados$MaxCAHA %>% density() %>% plot()

