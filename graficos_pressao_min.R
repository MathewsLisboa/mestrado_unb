### Pressão atmosférica min 

pressao <- readRDS(file = 'D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/pressao_diaria.rds')
pressao_min <- readRDS(file='D:/Users/Mathews/Documents/Git/mestrado_unb/dados_resumidos/pressao_min.rds')

max(pressao$pressao)
min(pressao$pressao)

H1 <- ggplot(pressao,aes(x=pressao))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(924,940,1))+
  labs(x='x',y='')+
  #labs(y='Densidade',x='Pressão atmosférica média diária')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))

pressao_min_df <- data.frame(x = pressao_min)

hist(Z2)
Z2 <- pressao_min

fit <- fExtremes::gevFit(Z2, type = 'mle')

starts <- c(mu=fit@fit$par.ests[2],sigma=fit@fit$par.ests[3], xi=fit@fit$par.ests[1], delta=0)
test <- optim(par=starts,fn=likbgev,y=Z2,method = 'BFGS',hessian = T)
x <- seq(min(Z2), max(Z2), 0.1)


test$par
test$hessian

colors <- c('BGEV'='black', 'GEV'='red')

H2 <- ggplot(pressao_min_df,aes(x=x))+
  geom_histogram(aes(y = ..density..),colour='white',fill='#696969',breaks=seq(925,933,1))+
  geom_line(aes(x=x,y=dbgevd(x, mu=test$par[1],sigma=test$par[2],
                            xi = test$par[3], delta=  test$par[4]), color='BGEV'),size=1)+
  
  ##PorLegenda
  geom_line(aes(x=x,y=dgev(x, loc= fit@fit$par.ests[2], scale = fit@fit$par.ests[3], shape =fit@fit$par.ests[1]),
            color='GEV'),size=1)+

  labs(x='m',y='', color='')+
  scale_color_manual(values=colors)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'),
        legend.position = 'top')

H2

## no fim ficaram sobrepostas pq era unimodal então gev e bgev são iguais ###
plot_grid(H1,H2)
ggsave('D:/Users/Mathews/Documents/Git/mestrado_unb/imagens/painel_pressao.png',
       width=158,height=93,units='mm')

