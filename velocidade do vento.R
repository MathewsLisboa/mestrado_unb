#### Velocidade do vento rajada máxima 
library(evd)

vento_diario <- readRDS(file = 'D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/vento_diario.rds')
vento_max <- readRDS(file='D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/vento_max.rds')

vento_diario$rajada %>% min()


H1<- ggplot(vento_diario,aes(x=rajada))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(3,24,1))+
  labs(x='x',y='')+
  labs(y='Densidade',x='Velocidade rajada de vento')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))


vento_max_df <- data.frame(x = vento_max)


ggplot(vento_max_df,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(8,23,1))+
  labs(x='x',y='')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))


Z2 <- vento_max

fit <- fExtremes::gevFit(Z2, type = 'mle')

bgev.support(mu=fit@fit$par.ests[2],sigma=21, xi=0, delta=3)
min(Z2);max(Z2)

starts <- c(mu=fit@fit$par.ests[2],sigma=fit@fit$par.ests[3], xi=fit@fit$par.ests[1], delta=0)
test <- optim(par=starts,fn=likbgev,y=Z2,method = 'BFGS',hessian = T)

x <- seq(min(Z2), max(Z2), 0.1)
test$par

test$hessian
colors <- c('BGEV'='black', 'GEV'='red')


H2 <- ggplot(vento_max_df,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(8,24,1))+
  geom_line(aes(x=x,y=dbgev(x, mu=test$par[1],sigma=test$par[2],
                            xi = test$par[3], delta=  test$par[4]), color='BGEV'),size=1)+
  
  ##PorLegenda
  geom_line(aes(x=x,y=dgev(x, loc= fit@fit$par.ests[2], scale = fit@fit$par.ests[3], shape =fit@fit$par.ests[1]),
                color='GEV'),size=1)+
  
  labs(x='Velocidade rajada de vento',y='Densidade', color='Legenda')+
  scale_color_manual(values=colors)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'),
        legend.position = 'top')

H2
plot_grid(H1,H2)
ggsave('D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/painel_rajada_vento.png',
       width=158,height=93,units='mm')




#### Tentando com vento por m/s blocos máximo 


Z2 <- MI

fit <- fExtremes::gevFit(Z2, type = 'mle')

bgev.support(mu=fit@fit$par.ests[2],sigma=fit@fit$par.ests[3], xi=fit@fit$par.ests[1], delta=1)
min(Z2);max(Z2)

starts <- c(mu=fit@fit$par.ests[2],sigma=fit@fit$par.ests[3], xi=fit@fit$par.ests[1], delta=1)
test <- optim(par=starts,fn=likbgev,y=Z2,method = 'BFGS',hessian = T)
x <- seq(min(Z2), max(Z2), 0.1)
test$par

sqrt(diag(solve(test$hessian))) 

2*pnorm(abs(0.0553/0.1394),lower.tail = F)

hist(MI, probability = T)
lines(x,dbgev(x,mu=test$par[1], sigma = test$par[2], xi= test$par[3], delta = test$par[4]),)
