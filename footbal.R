library(tidyverse)
dados <- read.csv('D:/Users/Mathews/Downloads/E0.csv')

dados$MaxCAHH %>% hist()
dados$MaxCAHH[is.na(dados$MaxCAHH)==F] %>% density() %>% plot()

dados$MaxCAHA[is.na(dados$MaxCAHA)==F] %>% density() %>% plot()

dados$MaxAHH %>% hist()

dados$MaxD %>% hist()

dados$MaxAHH %>% hist()

dados$MaxCAHH %>% hist()

dados$PSCD %>% hist()

dados$MaxCAHA %>% hist()
