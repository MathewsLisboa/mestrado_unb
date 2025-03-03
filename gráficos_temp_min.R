## Temperatura média ponto do orvalho tirando mínimo####
## vou trabalhar com ela nesse código e espero conseguir inclusive um bom ajuste do modelo ##
source("BGEV_functions.R")
### Primeiros analisamos 

temp_min_orvalho <- readRDS("D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/temperatura_min_orvalho.rds")

temp_media <- readRDS("D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/temp_media_diaria.rds")


max(temp_media$media_temp)
min(temp_media$media_temp)

library(cowplot)
library(tidyverse)
library(evd)


H1<- ggplot(temp_media,aes(x=media_temp))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(0,22,1))+
  labs(x='x',y='')+
  #labs(y='Densidade',x='Temp. Orvalho média diária')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))


temp_min <- data.frame(x = temp_min_orvalho)  

ggplot(temp_min,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(0,21,1))+
  labs(x='x',y='')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
       axis.line=element_line(colour='black'))


sd(temp_min$x)

Z2 <- temp_min_orvalho
fit <- fExtremes::gevFit(Z2, type = 'mle')
starts <- c(mu=fit@fit$par.ests[2],sigma=fit@fit$par.ests[3], xi=fit@fit$par.ests[1], delta=0)
test <- optim(par=starts,fn=likbgev,y=Z2,method = 'BFGS',hessian = T)

fit@fit$par.ests
fit@fit$par.ses
sqrt(diag(fit@fit$varcov))


test$par
test$hessian
sqrt(diag(solve(test$hessian)))


#### Gráfico Bilateral ###### 
colors <- c('BGEV'='black', 'GEV'='red')

H2 <- ggplot(temp_min,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(0,21,1))+
  geom_line(aes(x=x,y=dbgevd(x, mu=test$par[1],sigma=test$par[2],
                            xi = test$par[3], delta= test$par[4]), color='BGEV'),size=1)+
  ##PorLegenda
  geom_line(aes(x=x,y=dgev(x, loc=8.9407448, scale = 5.7027691, shape = -0.6542575), color='GEV'),size=1)+
  labs(x='min',y='', color='')+
  scale_color_manual(values=colors)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'),
        legend.position = 'top')


plot_grid(H1,H2)
ggsave('D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/painel_temperatura_orvalho.png',
       width=158,height=93,units='mm')
