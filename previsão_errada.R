#### PREVISÃO ERRADA DO CENSO ####

getwd()
library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
library(ggthemes)
library(scales)
library(dplyr)
library(magrittr)

################################################################################
############### Evolução valor adicionado do PIB ###############################
################################################################################
### Pegando os dados de variação de volume no VA de PE
vol_pe<-read.csv2("vol_pe.csv", header=T)
vol_pest<-ts(vol_pe, start = 2003, end = 2020, frequency = 1)
plot(vol_pest, col="red")

### Criando a coluna de datas
Ano<-seq.Date(from = as_date("2003-01-01"),
              to = as_date("2020-01-01"),
              "year")
### Criando um data frame
vol_pedf<-data.frame(Ano, vol_pe)
colnames(vol_pedf)<- c("Anos", "Tx_evolução")

#### Gráfico com o tema economist
ggplot(vol_pedf, aes(x=Ano))+theme_economist()+
  geom_line(aes(y= Tx_evolução, colour="Tx_evolução"), linewidth =0.8)+
  theme(axis.text.x=element_text(angle=45, hjust=0.05), axis.text.y = element_text (angle=0, vjust = 0.05),
        plot.title = element_text(size=10, face='bold', hjust=0.5), plot.subtitle = element_text(size=10, face='bold', hjust=0.5),legend.position = "top")+
  labs(x='Anos', y='Crescimento (%)',
       title='Evolução no Valor Adicionado em PE',subtitle = '(2003-2020)',
       caption='IBGE, UE-PE', color = "") +
  scale_x_date(date_labels = "%Y", breaks = date_breaks("1 year")) ### Deu certo.

#### Gráfico taxa de evolução com o tema linedraw
ggplot(vol_pedf, aes(x=Ano, y=Tx_evolução))+theme_linedraw()+
  geom_line(col="darkgreen", linewidth =0.8)+
  theme(axis.text.x=element_text(angle=45, hjust=0.90), axis.text.y = element_text (angle=0, vjust = 0.05),
        plot.title = element_text(size=10, face='bold', hjust=0.50), plot.subtitle = element_text(size=10, face='bold', hjust=0.5),legend.position = "top")+
  labs(x='Anos', y='Crescimento (%)',
       title='Evolução no Valor Adicionado em PE',subtitle = '(2003-2020)',
       caption='IBGE, Contas Regionais - UE-PE', color = "") +
  scale_x_date(date_labels = "%Y", breaks = date_breaks("1 year")) ### Deu certo.


################################################################################
################ Desemprego trimestral em PE ##################################
################################################################################
### Pegando os dados de taxa de desocupação em PE
tx_des_pe<-read.csv2("tx_des_pe.csv", header=T)

### Criando a coluna de datas
trim<-seq.Date(from = as_date("2012-01-01"),
               to = as_date("2020-01-01"),
               "quarter")
trimestre = as.yearqtr(trim, format='%Y%q')
trimestre

### Criando um data frame
des_pedf <- data.frame(trimestre, tx_des_pe)
colnames(des_pedf) = c('trimestres', 'tx_des_PE')

### Gráfico desemprego com o tema linedraw
ggplot(des_pedf, aes(x=trimestres, y=tx_des_PE))+theme_linedraw()+
  geom_line(col="blue", size = .8)+
  scale_x_yearqtr(breaks = seq(from = min(des_pedf$trimestres),
                               to = max(des_pedf$trimestres),
                               by = .5),
                  format = "%Y/Tri%q")+
  theme(axis.text.x=element_text(angle=45, hjust=1), plot.title = element_text(size=10, 
                                                                               face='bold', hjust=0.50),
        plot.subtitle = element_text(size=10, face='bold', hjust=0.5))+
  labs(x='Ano/Trimestre', y='PIB (%)',
       title='Taxa de Desocupação em PE',subtitle = '2012-2020',
       caption='Fonte: IBGE-PNADc-UE/PE') ### Perfeito!

### Gráfico com o tema economist
ggplot(des_pedf, aes(x=trimestres, y=tx_des_PE))+theme_economist()+
  geom_line(col="blue", size = .8)+
  scale_x_yearqtr(breaks = seq(from = min(des_pedf$trimestres),
                               to = max(des_pedf$trimestres),
                               by = .5),
                  format = "%Y/Tri%q")+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        plot.title = element_text(size=10, face='bold'))+
  labs(x='Ano/Trimestre', y='PIB (%)',
       title='Taxa de Desocupação em PE',subtitle = '2012-2020',
       caption='Fonte: IBGE-PNADc-UE/PE') ### Perfeito!


################################################################################
################  Rendimento anual em PE      ##################################
################################################################################
### Pegando dados do rendimento médio mensal real em PE
rend_pe<-read.csv2("rend_pe.csv", header=T)

### Criando a coluna de datas
Anos<-seq.Date(from = as_date("2012-01-01"),
              to = as_date("2021-01-01"),
              "year")

### Criando um data frame
rend_pedf<-data.frame(Anos, rend_pe)
colnames(rend_pedf)<- c("Anos", "rendimento_PE")

#### Gráfico rendimento com o tema linedraw
ggplot(rend_pedf, aes(x=Anos, y=rendimento_PE))+theme_linedraw()+
  geom_line(col="darkorange", linewidth =0.8)+
  theme(axis.text.x=element_text(angle=45, hjust=0.90), axis.text.y = element_text (angle=0, vjust = 0.05),
        plot.title = element_text(size=10, face='bold', hjust=0.50), plot.subtitle = element_text(size=10, face='bold', hjust=0.5),legend.position = "top")+
  labs(x='Anos', y='Rendimento (R$)',
       title='Rendimento Médio Mensal Real em PE',subtitle = '(2012-2021)',
       caption='IBGE, PNADc - UE-PE', color = "") +
  scale_x_date(date_labels = "%Y", breaks = date_breaks("1 year")) ### Deu certo.


################################################################################
################ Taxa de natalidade em PE      ##################################
################################################################################
### Pegando dados de taxa de natalidade em PE
nat_pe<-read.csv2("natalidadePE.csv", header = T)

### Criando a coluna de datas
ANOS<- seq.Date(from = as_date("2011-01-01"),
                      to = as_date("2020-01-01"),
                      "year")

### Criando um data frame
nat_pedf<-data.frame(ANOS, nat_pe)
colnames(nat_pedf)<- c("Anos", "Tx_natalidade_PE")

#### Gráfico da taxa de natalidade com o tema linedraw
ggplot(nat_pedf, aes(x=Anos, y=Tx_natalidade_PE))+theme_linedraw()+
  geom_line(col="brown", linewidth =0.8)+
  theme(axis.text.x=element_text(angle=45, hjust=0.90), axis.text.y = element_text (angle=0, vjust = 0.05),
        plot.title = element_text(size=10, face='bold', hjust=0.50), plot.subtitle = element_text(size=10, face='bold', hjust=0.5),legend.position = "top")+
  labs(x='Anos', y='Taxa de natalidade',
       title='Taxa de Natalidade em PE',subtitle = '(2011-2020)',
       caption='SINASC- Ministério da Saúde - UE-PE', color = "") +
  scale_x_date(date_labels = "%Y", breaks = date_breaks("1 year")) ### Deu certo.
