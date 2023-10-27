####### Investigação EXPLORATÓRIA das disciplinas ####### 
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)

`%notin%` <- Negate(`%in%`)

setwd('D:/Users/Mathews/Documents/Git/TCC_2')

#saveRDS(CIC_unicos,'banco_unico/CIC_unicos.rds')

CIC_unicos <- readRDS('banco_unico/CIC_unicos.rds')



#CIC_unicos$Status <- ifelse(test = CIC_unicos$Status==1, 'Falha',"Censura")

#saveRDS(CIC_unicos, 'banco_unico/CIC_unicos.rds')

########### Procurando por correlações entre variáveis ######### 

cor(CIC_unicos$total_trancamentos, CIC_unicos$taxa_creditos_nao_cumpridos, use = 'complete.obs')
# -0.207

#### Não dá pra usar reprovacoes e taxas de crédito junto

cor(CIC_unicos$total_reprovacoes, CIC_unicos$total_creditos_reprovado, use = 'complete.obs')

cor(as.numeric(CIC_unicos$IRA), CIC_unicos$taxa_creditos_nao_cumpridos, use ="na.or.complete" )

cor.test(CIC_unicos$IRA, CIC_unicos$taxa_creditos_nao_cumpridos)

ggplot(CIC_unicos,aes(x=IRA,y=taxa_creditos_nao_cumpridos))+geom_point(colour='#A11D21',
                                        size=3)+
  labs(x='IRA',y='Taxa de reprovação')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio/dispersao_taxa_IRA.png',width=158,height=93,units='mm')




## não dá mesmo, muito alta correlação

##### Não da pra usar IRA com nenhuma das taxas de reprovações 

cor(as.numeric(CIC_unicos$IRA), CIC_unicos$total_reprovacoes, use ="na.or.complete" )

cor(as.numeric(CIC_unicos$IRA), CIC_unicos$taxa_reprovacao_1_semestre, use ="na.or.complete" )

cor(as.numeric(CIC_unicos$IRA), CIC_unicos$taxa_reprovacao_3_semestres, use ="na.or.complete" )


## pior que mesmo não

###### Até dá pra usar aprovado com taxas de crédito, mas cor=-0.49

cor(CIC_unicos$total_creditos_aprovado, CIC_unicos$taxa_creditos_nao_cumpridos, use = 'complete.obs')

#### Dá pra usar créditos cursado

cor(CIC_unicos$total_creditos_cursado, CIC_unicos$taxa_creditos_nao_cumpridos, use = 'complete.obs')

#### número de chamadas parece de boa, mas acho que é pouco representativo

cor(as.numeric(CIC_unicos$chamada_ingressou_UnB), CIC_unicos$taxa_creditos_nao_cumpridos, use = 'complete.obs')


##### Tabela de formas de ingresso, precisa mudar para apenas 4 formas.
## FORMAS: 1-PAS, 2-ENEM,3-VEST ; 4 outros

table(CIC_unicos$forma_ingresso_unb)


#### Não dá pra usar Escola e Cotas JUNTO 
chisq.test(CIC_unicos$sistema_cotas, CIC_unicos$Escola)

table(CIC_unicos$sistema_cotas,CIC_unicos$Escola)

##### Fazer correlação entre variáveis explicativas e Status ######


######## STATUS ################

CIC_unicos %>%
  count(var1=Status) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x=var1, y=n))+geom_col(fill='#A11D21', width = 0.65, position='dodge')+
  labs(x='Status',y='Frequência')+
  geom_text(aes(label= scales::percent(pct)),
            position = position_dodge(width = .9),# move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio///barras_satus.png',width=158,height=93,units='mm')



########## Gênero #############

### Gênero e Statu, p.value = 0.119, >5% , parece não haver relação

chisq.test(CIC_unicos$genero, CIC_unicos$Status) ## nesse caso com o banco resudizdo deu que não é
table(CIC_unicos$genero, CIC_unicos$Status)

### Gráfico de barra de Gênero Univariado 

CIC_unicos %>%
  count(var1=genero) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x=var1, y=n))+geom_col(fill='#A11D21', width = 0.65, position='dodge')+
  labs(x='Sexo',y='Frequência')+
  geom_text(aes(label= scales::percent(pct)),
            position = position_dodge(width = .9),# move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio//barra_genero.png',width=158,height=93,units='mm')


### Gráfico Bivariado com Genero e Status 

CIC_unicos %>%
  count(var1=genero, var2=Status) %>% 
  group_by(var1) %>% summarise(pct=prop.table(n),n=n, Status=factor(var2)) %>%  
  ggplot(aes(x = var1, y = n, fill = Status, label = scales::percent(pct))) + 
  scale_fill_manual(name='Status',values=c('#A11D21',
                                           '#003366'), labels=c('Censura','Falha'))+
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),# move to center of bars
            vjust = -0.1,    # nudge above top of bar
            size = 3) + 
  labs(x='Sexo',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))+
  theme(legend.position='top')
ggsave('imgs_banco_cheio/barra_genero_vs_status.png',width=158,height=93,units='mm')


####### Estado Nascimento #########
#### estado_nascimento, acho que vou excluir essa variável, não parece ser de grande valor

table(CIC_unicos$estado_nascimento, CIC_unicos$Status)

########### Sistema de Cotas  e Escola ###########

#### Não tem relação entre cotas e Status
chisq.test(CIC_unicos$sistema_cotas, CIC_unicos$Status)


### barras sistema de cotas 

CIC_unicos %>%
  count(var1=sistema_cotas) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x=var1, y=n))+geom_col(fill='#A11D21', width = 0.65, position='dodge')+
  labs(x='Sistema de Cotas',y='Frequência')+
  geom_text(aes(label= scales::percent(pct)),
            position = position_dodge(width = .9),# move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio//barra_cotas.png',width=158,height=93,units='mm')

### barras de cotas vs status 

CIC_unicos %>%
  count(var1=sistema_cotas, var2=Status) %>% 
  group_by(var1) %>% summarise(pct=prop.table(n),n=n, Status=factor(var2)) %>%  
  ggplot(aes(x = var1, y = n, fill = Status, label = scales::percent(pct))) + 
  scale_fill_manual(name='Status',values=c('#A11D21',
                                           '#003366'), labels=c('Censura','Falha'))+
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),# move to center of bars
            vjust = -0.1,    # nudge above top of bar
            size = 3) + 
  labs(x='Sistema de Cotas',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))+
  theme(legend.position='top')
ggsave('imgs_banco_cheio/barra_cotas_vs_status.png',width=158,height=93,units='mm')



### Porém tem com escola, ou seja, Escola e Cotas tem alta correlação e Escola tem com Status
## Mas sistema_de_cotas não tem com status, logo usar Escola
### melhor usar escola que usar cotas

chisq.test(CIC_unicos$Escola, CIC_unicos$Status)


### barras escolas 
CIC_unicos$Escola %>% unique()
#CIC_unicos$Escola[CIC_unicos$Escola=='Publica'] <- 'Pública'

CIC_unicos %>%
  count(var1=Escola) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x=var1, y=n))+geom_col(fill='#A11D21', width = 0.65, position='dodge')+
  labs(x='Tipo de Escola',y='Frequência')+
  geom_text(aes(label= scales::percent(pct)),
            position = position_dodge(width = .9),# move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio/barra_escola.png',width=158,height=93,units='mm')

### barras de escolas vs status 


CIC_unicos %>%
  count(Escola, Status) %>% 
  group_by(Escola) %>% summarise(pct=prop.table(n),n=n, Status=factor(Status)) %>%  
  ggplot(aes(x = Escola, y = n, fill = Status, label = scales::percent(pct))) + 
  scale_fill_manual(name='Status',values=c('#A11D21',
                                           '#003366'), labels=c('Censura','Falha'))+
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),# move to center of bars
            vjust = -0.1,    # nudge above top of bar
            size = 3) + 
  labs(x='Tipo de Escola',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))+
  theme(legend.position='top')
ggsave('imgs_banco_cheio/barra_escola_vs_status.png',width=165,height=93,units='mm')



###### Avaliando Forma de Ingresso na UNB #######

chisq.test(CIC_unicos$forma_ingresso_unb, CIC_unicos$Status)


### barras sistema de Forma de ingresso unb 


CIC_unicos %>%
  count(var1=forma_ingresso_unb) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x=var1, y=n))+geom_col(fill='#A11D21', width = 0.65, position='dodge')+
  labs(x='Forma de Ingreso UNB',y='Frequência')+
  geom_text(aes(label= scales::percent(pct)),
            position = position_dodge(width = .9),# move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio/barra_forma_ingresso.png',width=158,height=93,units='mm')

### barras de formas de ingresso  vs status 

CIC_unicos %>%
  count(var1=forma_ingresso_unb, var2=Status) %>% 
  group_by(var1) %>% summarise(pct=prop.table(n),n=n, Status=factor(var2)) %>%  
  ggplot(aes(x = var1, y = n, fill = Status, label = scales::percent(pct))) + 
  scale_fill_manual(name='Status',values=c('#A11D21',
                                           '#003366'), labels=c('Censura','Falha'))+
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),# move to center of bars
            vjust = -0.1,    # nudge above top of bar
            size = 3) + 
  labs(x='Forma de Ingreso UNB',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))+
  theme(legend.position='top')
ggsave('imgs_banco_cheio/barra_formas_ingresso_vs_status.png',width=158,height=93,units='mm')


#### IRA ##########

ggplot(CIC_unicos,aes(x=IRA))+geom_histogram(colour='white',
                                             fill='#A11D21',breaks=c(seq(0,5,by=0.5)))+
  labs(x='Valores de IRA',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio/hist_IRA.png',width=158,height=93,units='mm')



Ira_menor_0 <- CIC_unicos %>% filter(IRA<=1)

#### Até exise a relação e a diferenciação, mas é menor
### que a taxa de reprovação no curso todo, e temos um problema.
### com o quanto a duas variáveis tem realção uma com a outra...

CIC_unicos %>% 
  ggplot(aes(x=as.factor(Status),y=IRA))+
  geom_boxplot(fill=c('#A11D21'),width=0.5)+
  #ggtitle('Cálculo 1')+
  stat_summary(fun='mean',geom='point',shape=23,size=3,
               fill='white')+
  labs(x='Status',y="IRA")+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line.y=element_line(colour='black'))
ggsave('imgs_banco_cheio/box_IRA.png',width=158,height=93,units='mm')



#### Taxa de Reprovação ##########

ggplot(CIC_unicos,aes(x=taxa_creditos_nao_cumpridos))+geom_histogram(colour='white',
                                             fill='#A11D21',breaks=seq(0,1,0.2))+
  labs(x='Taxa de Reprovação',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio/hist_taxa_reprovacao.png',width=158,height=93,units='mm')

#### Até exise a relação e a diferenciação, mas é menor
### que a taxa de reprovação no curso todo, e temos um problema.
### com o quanto a duas variáveis tem realção uma com a outra...

CIC_unicos %>% 
  ggplot(aes(x=as.factor(Status),y=taxa_creditos_nao_cumpridos))+
  geom_boxplot(fill=c('#A11D21'),width=0.5)+
  #ggtitle('Cálculo 1')+
  stat_summary(fun='mean',geom='point',shape=23,size=3,
               fill='white')+
  labs(x='Status',y="Taxa de Reprovação")+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line.y=element_line(colour='black'))
ggsave('imgs_banco_cheio/box_taxa_reprovacao.png',width=158,height=93,units='mm')


### Cursou verão ########

##### Verificando realação com Dummys de verão ou não 
## única variável que parece ter algo a ser dito é "Cursou Verão" ou não
table(CIC_unicos$entrou_curso_verao)

chisq.test(CIC_unicos$Status, CIC_unicos$cursou_verao)

chisq.test(CIC_unicos$Status, CIC_unicos$saiu_verao)


CIC_unicos %>%
  count(Var1=cursou_verao) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x=Var1, y=n))+geom_col(fill='#A11D21', width = 0.65, position='dodge')+
  labs(x='Cursou Verão',y='Frequência')+
  geom_text(aes(label= scales::percent(pct)),
            position = position_dodge(width = .9),# move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3)+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio//barra_cursou_verao.png',width=158,height=93,units='mm')

### barras de cursou_verao vs status 

CIC_unicos %>%
  count(Var1=cursou_verao, Status) %>% 
  group_by(Var1) %>% summarise(pct=prop.table(n),n=n, Status=factor(Status)) %>%  
  ggplot(aes(x = Var1, y = n, fill = Status, label = scales::percent(pct))) + 
  scale_fill_manual(name='Status',values=c('#A11D21',
                                           '#003366'), labels=c('Censura','Falha'))+
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),# move to center of bars
            vjust = -0.1,    # nudge above top of bar
            size = 3) + 
  labs(x='Cursou Verão',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))+
  theme(legend.position='top')
ggsave('imgs_banco_cheio/barra_cursou_verao_vs_status.png',width=165,height=93,units='mm')



##### Total de Trancamentos #####

## moderadamente relacionado com tempo

cor(CIC_unicos$total_trancamentos, CIC_unicos$tempo)

cor(CIC_unicos$total_trancamentos, CIC_unicos$Status)


### histograma sistema de total de trancamentos


ggplot(CIC_unicos,aes(x=total_trancamentos))+geom_histogram(colour='white',
                                                            fill='#A11D21',bins = 7)+
  labs(x='Número de Trancamentos',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio/hist_total_trancamentos.png',width=158,height=93,units='mm')

### boxplot de total de trancamentos vs status 

CIC_unicos %>% 
  ggplot(aes(x=as.factor(Status),y=total_trancamentos))+
  geom_boxplot(fill=c('#A11D21'),width=0.5)+
  #ggtitle('Cálculo 1')+
  stat_summary(fun='mean',geom='point',shape=23,size=3,
               fill='white')+
  labs(x='Status',y="Número de trancamentos")+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line.y=element_line(colour='black'))
ggsave('imgs_banco_cheio/box_trancamentos_status.png',width=158,height=93,units='mm')


##### Distância UNB ########

## parece não a ver correlação
cor(CIC_unicos$distancia_unb, CIC_unicos$tempo, use='complete.obs')

### histograma sistema de total de trancamentos

## variável de distância tem um problema grande, os alunos que moram fora de bsb
## e colocaram cep de outros estados

CIC_unicos %>% filter(estado_nascimento%in%c('DF','GO'), distancia_unb<100) %>% 
  ggplot(aes(x=distancia_unb))+geom_histogram(colour='white',
                                              fill='#A11D21',bins = 5)+
  labs(x='Número de Trancamentos',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))

### boxplot de total de trancamentos vs status 

CIC_unicos %>% filter(estado_nascimento%in%c('DF','GO'), distancia_unb<100) %>% 
  ggplot(aes(x=as.factor(Status),y=distancia_unb))+
  geom_boxplot(fill=c('#A11D21'),width=0.5)+
  #ggtitle('Cálculo 1')+
  stat_summary(fun='mean',geom='point',shape=23,size=3,
               fill='white')+
  labs(x='Status',y="Ditância da UNB (Km)")+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line.y=element_line(colour='black'))
ggsave('imgs_banco_cheio//box_distancia_unb.png',width=158,height=93,units='mm')


##### Distância ficou horrível de observar melhor tirar do banco 


### parece que distância não interfere tnato 



### Verificar IDADE ######

cor(CIC_unicos$idade_anos, CIC_unicos$tempo)
## parece nem ter correlação
cor(CIC_unicos$idade_anos, CIC_unicos$Status)

#### Modificar IDADE #####

CIC_unicos$idade_anos <- NA
CIC_unicos$data_saida_curso <- CIC_unicos$periodo_saida_curso_sv

CIC_unicos$data_saida_curso <- str_sub(CIC_unicos$data_saida_curso,end=-2)


for(i in 1:nrow(CIC_unicos)){
  if(str_sub(CIC_unicos$periodo_ingresso_curso_sv[i],5,5)==1){
    CIC_unicos$data_saida_curso[i] <-str_c(CIC_unicos$data_saida_curso[i],"1",'1',sep='-') 
  } else{
    CIC_unicos$data_saida_curso[i] <- str_c(CIC_unicos$data_saida_curso[i],'6','1',sep='-')
  }

}

CIC_unicos$data_saida_curso <- as.Date(CIC_unicos$data_saida_curso)


CIC_unicos$idade_anos <- difftime(CIC_unicos$data_saida_curso, CIC_unicos$nascimento,units ='days')/365.6

CIC_unicos$idade_anos <- round(CIC_unicos$idade_anos)


### histograma de idade

CIC_unicos %>% 
  ggplot(aes(x=idade_anos))+geom_histogram(colour='white',
                                           fill='#A11D21',bins = 7,
                                           breaks=c(20,25,30,35,40,45,50,55,60))+
  labs(x='Idade em Anos',y='Frequência')+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))
ggsave('imgs_banco_cheio/hist_idade.png',width=158,height=93,units='mm')

### boxplot de idade vs status 

CIC_unicos %>%  
  ggplot(aes(x=as.factor(Status),y=idade_anos))+
  geom_boxplot(fill=c('#A11D21'),width=0.5)+
  #ggtitle('Cálculo 1')+
  stat_summary(fun='mean',geom='point',shape=23,size=3,
               fill='white')+
  labs(x='Status',y="Idade em Anos")+
  theme_bw()+
  theme(axis.title.y=element_text(colour='black',size=12),
        axis.title.x=element_text(colour='black',size=12),
        axis.text=element_text(colour='black',size=9.5),
        panel.border=element_blank(),
        axis.line.y=element_line(colour='black'))
ggsave('imgs_banco_cheio/box_idade.png',width=158,height=93,units='mm')






### parece que quem comete evasão tem ca caixa levemente a cima do que 
### os que estão em censura


#### criação de banco para modelagem com variáveis promissoras ######


CIC_unicos_cheio <- CIC_unicos %>% select(Aluno,tempo,Status,genero,sistema_cotas,distancia_unb,idade_anos,
                                    Escola,IRA,forma_ingresso_unb,cursou_verao, total_trancamentos, 
                                    taxa_creditos_nao_cumpridos)


CIC_modelagem_cheio <- CIC_unicos %>% select(Aluno,tempo,Status_l,genero,sistema_cotas,distancia_unb,idade_anos,
                                             Escola,IRA,forma_ingresso_unb,cursou_verao, total_trancamentos, 
                                             taxa_creditos_nao_cumpridos)


saveRDS(CIC_modelagem_cheio, file='banco_unico/CIC_modelagem_cheio.rds')

