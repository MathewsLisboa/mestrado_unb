publica <- dados_inep %>% filter(TP_ESCOLA==2)


ggplot(dados_inep,aes(x=as.numeric(NU_NOTA_MT)))+
  geom_histogram( aes(y = ..density..),colour='white',fill='#A11D21',breaks=seq(0,900,25))+
  geom_density(lwd=1.2,
               colour= 'blue')+
  labs(x='Notas de Matemática Enem 2022',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imagens/hist_enem_nt_mt.png',width=158,height=93,units='mm')


ggplot(saeb,aes(x=as.numeric(PROFICIENCIA_LP_SAEB)))+
  geom_histogram( aes(y = ..density..),colour='white',fill='#A11D21',breaks=seq(500,900,25))+
  geom_density(lwd=1.2,
               colour= 'blue')+
  labs(x='Proficiência em Língua Portuguesa SAEB 2021',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imagens/hist_saeb_LP.png',width=158,height=93,units='mm')

ggplot(saeb,aes(x=as.numeric(PROFICIENCIA_MT_SAEB)))+
  geom_histogram( aes(y = ..density..),colour='white',fill='#A11D21',breaks=seq(600,900,10))+
  geom_density(lwd=1.2,
               colour= 'blue')+
  labs(x='Proficiência em Matemática SAEB 2021',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imagens/hist_saeb_MT.png',width=158,height=93,units='mm')



ggplot(CIC_unicos,aes(x=IRA))+
  geom_histogram( aes(y = ..density..),colour='white',fill='#A11D21',breaks=seq(0,5,0.50))+
  geom_density(lwd=1.2,
               colour= 'blue')+
  labs(x='IRA Alunos entre 2014-2019',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imagens/hist_CIC_IRA.png',width=158,height=93,units='mm')





