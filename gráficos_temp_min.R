## Temperatura média ponto do orvalho tirando mínimo####
## vou trabalhar com ela nesse código e espero conseguir inclusive um bom ajuste do modelo ##
source("BGEV_functions.R")

library(cowplot)
library(tidyverse)
library(ggplot2)
library(evd)

### Primeiros analisamos 

temp_min_orvalho <- readRDS("dados_resumidos/temperatura_min_orvalho.rds")



temp_media <- readRDS("dados_resumidos/temp_media_diaria.rds")


mean(temp_media$media_temp)
sd(temp_media$media_temp)
median(temp_media$media_temp)
max(temp_media$media_temp)
min(temp_media$media_temp)


temp_min <- data.frame(x = temp_min_orvalho)  

Z2 <- temp_min_orvalho

## fit de GEV unimodal
fit <- fExtremes::gevFit(Z2, type = 'mle')

## usando como starts o fit da GEV unimodal
starts <- c(mu=fit@fit$par.ests[2],sigma=fit@fit$par.ests[3], xi=fit@fit$par.ests[1], delta=0)

## Fit IID BGEV 
IID <- optim(par=starts,fn=likbgev,y=Z2,method = 'BFGS',hessian = T)


fit@fit$par.ests[2]
fit@fit$par.ses
sqrt(diag(fit@fit$varcov))


IID$par
IID$hessian
sqrt(diag(solve(IID$hessian)))


#### Gráfico Bilateral ###### 


H1 <-  ggplot(temp_media,aes(x=media_temp))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(0,24,1))+
  labs(x='x',y='')+
  #labs(y='Densidade',x='Temp. Orvalho média diária')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))

colors <- c('BGEV'='black', 'GEV'='red')

H2 <- ggplot(temp_min,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(0,21,1))+
  geom_line(aes(x=x,y=dbgevd(x, mu=test$par[1],sigma=test$par[2],
                             xi = test$par[3], delta= test$par[4]), color='BGEV'),size=1)+
  ##PorLegenda
  geom_line(aes(x=x,y=dgev(x, loc=fit@fit$par.ests[2], scale = fit@fit$par.ests[3], shape = fit@fit$par.ests[1]), color='GEV'),size=1)+
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

H1
ggsave('imagens/correcoes_cira/hist_data_temp.png',width=158,height=93,units='mm')
H2
ggsave('imagens/correcoes_cira/resultado_temp.png',width=158,height=93,units='mm')
