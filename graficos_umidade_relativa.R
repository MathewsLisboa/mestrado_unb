### Esse código é pra analisarmos a umidade relativa, mínima com blocos mínimos 

temp <- readRDS(file='D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/umidade_diaria.rds')

umidade_min <- readRDS(file = 'D://Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/umidade_minima.rds')

max(temp$umidade)
min(temp$umidade)

library(tidyverse)


H1<- ggplot(temp,aes(x=umidade))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(20,92,5))+
  labs(y='',x='x')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))



umidade_min_df <- data.frame(x = umidade_min)  


ggplot(umidade_min_df,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(21,80,4))+
  labs(x='x',y='')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))



Z2 <- umidade_min

fit <- fExtremes::gevFit(Z2, type = 'mle')

starts <- c(mu=fit@fit$par.ests[2],sigma=fit@fit$par.ests[3], xi=fit@fit$par.ests[1], delta=0)
test <- optim(par=starts,fn=likbgev,y=Z2,method = 'BFGS',hessian = T)
x <- seq(min(Z2), max(Z2), 0.01)


fit@fit$par.ests
fit@fit$par.ses

test$par
sqrt(diag(solve(test$hessian)))

colors <- c('BGEV'='black', 'GEV'='red')
H2<- ggplot(umidade_min_df,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(21,75,3))+
  geom_line(aes(x=x,y=dbgevd(x, mu=test$par[1],sigma=test$par[2],
                            xi = test$par[3], delta= test$par[4]), color='BGEV'),size=1)+
  
  ##PorLegenda
  geom_line(aes(x=x,y=dgev(x, loc= fit@fit$par.ests[2], scale = fit@fit$par.ests[3], shape =fit@fit$par.ests[1]), color='GEV'),size=1)+
  
  labs(x='m',y='', color='')+
  scale_color_manual(values=colors)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'),
        legend.position = 'top')


library(cowplot)
plot_grid(H1,H2)
ggsave('D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/painel_umidade.png',
       width=158,height=93,units='mm')
