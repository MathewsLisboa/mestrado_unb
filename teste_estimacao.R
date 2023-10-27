library(tidyverse)

dados_inep <- read.csv2('C:\\Users\\Usuario\\Documents\\UNB_mestrado\\Orientação\\dados_inep\\DADOS\\MICRODADOS_ENEM_2022.csv')
enem_escola <- read.csv2('C:\\Users\\Usuario\\Documents\\UNB_mestrado\\Orientação\\microdados_enem_por_escola/DADOS/MICRODADOS_ENEM_ESCOLA.csv')
saeb <- read.csv2('C:\\Users\\Usuario\\Documents\\UNB_mestrado\\Orientação\\microdados_saeb_2021_ensino_fundamental_e_medio/DADOS/TS_ALUNO_2EF.csv')

nao_faltantes <- dados_inep %>% filter(TP_STATUS_REDACAO!='4')
escola_privada <- dados_inep %>% filter(TP_ESCOLA=='3')
escola_publica <- dados_inep %>% filter(TP_ESCOLA!='2')

hist(as.numeric(escola_publica$NU_NOTA_MT), col=rgb(1,0,0,0.5),breaks = 50)
hist(as.numeric(escola_privada$NU_NOTA_MT), col=rgb(0,0,1,0.5), breaks = 50, add = T)


dens <- density(as.numeric(saeb$PROFICIENCIA_MT_SAEB),na.rm = T)
plot(dens)

hist(as.numeric(enem_escola$NU_TAXA_APROVACAO))

hist(as.numeric(saeb$PROFICIENCIA_LP))


ggplot(surv_est_f,aes(x=ira))+
  geom_histogram( aes(y = ..density..),colour='white',fill='#A11D21',breaks=seq(0,5,0.5))+
  geom_density()+
  labs(x='IRA',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
#ggsave('imgs_banco_cheio/hist_taxa_reprovacao.png',width=158,height=93,units='mm')


## dados_inep$NU_NOTA_MT

ggplot(dados_inep,aes(x=as.numeric(NU_NOTA_MT)))+
  geom_histogram( aes(y = ..density..),colour='white',fill='#A11D21',breaks=seq(0,900,25))+
  geom_density(lwd=1.2,
               colour= 'blue')+
  labs(x='IRA',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))


## DADOS SAEB 

saeb$PROFICIENCIA_LP_SAEB

ggplot(saeb,aes(x=as.numeric(PROFICIENCIA_LP_SAEB)))+
  geom_histogram( aes(y = ..density..),colour='white',fill='#A11D21',breaks=seq(500,900,25))+
  geom_density(lwd=1.2,
               colour= 'blue')+
  labs(x='Proficiência em Língua Portuguesa SAEB',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))


nas <-  as.numeric(saeb$PROFICIENCIA_LP) %>% is.na()
Z <- as.numeric(surv_est_f$ira)
hist(Z)
length(Z)

mu      <- 0
sigma   <- 2
xi      <- 1
delta   <- 1

starts <- c(mu, sigma, xi, delta)



test <- optim(par= starts, fn = lik1, y=Z, method="BFGS")


n       <- 10^3
mu      <- 0
sigma   <- 10
xi      <- 1
delta   <- 5

starts <- c(mu, sigma, xi, delta)

suport(sigma, xi, delta, mu)
Z <- rdgevd(n, mu, sigma, xi, delta)
hist(Z)

which(Z<=suport(sigma, xi, delta, mu))

log(dbgevd(Z,mu, sigma, xi, delta))

sum(-log(dbgevd(Z,mu, sigma, xi, delta)))


lik1(theta = starts, y=Z)
lik_l_meu(theta = starts, y=Z)

