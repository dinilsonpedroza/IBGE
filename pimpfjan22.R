################################################################################
################ PIM-PF - Janeiro de 2022 ######################################
################################################################################
getwd()
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
#### Pegando as séries (baixadas no pc em csv_BR)
pfba= read.csv2('pimpfba', header = T)
pfpe<-read.csv2('pimpfpe', header = T)
pfce<-read.csv2("pimpfce", header = T)
pfbr<-read.csv2("pimpfbr", header = T)

#### Vetor de datas
mes <- seq.Date(from = as_date("2019-01-01"),
                to = as_date("2022-10-01"),
                "month")
#### Criando um data frame
pfjan22<-data.frame(mes, pfbr, pfce, pfpe, pfba)

#### Gráficos

ggplot(pfjan22, aes(x=mes))+theme_economist()+
  geom_hline(yintercept=0, colour='red', linetype='dashed')+
  geom_line(aes(y= BR, colour="Brasil"), size =0.8)+
  geom_line(aes(y= PE, colour = "Pernambuco"), size = 0.8)+
  geom_line(aes(y= CE, colour = "Ceará"), size = 0.8)+
  geom_line(aes(y= BA, colour = "Bahia"), size = 0.8)+
  theme(axis.text.x=element_text(angle=45, hjust=0.05), axis.text.y = element_text (angle=0, vjust = 0.05),
        plot.title = element_text(size=10, face='bold', hjust=0.5), plot.subtitle = element_text(size=10, face='bold', hjust=0.5),legend.position = "top")+
  labs(x='Meses/Anos', y='PIM-PF(%)',
       title='PIM-PF - Variação mensal',subtitle = '(2019-2022)',
       caption='IBGE, UE-PE', color = "") +
  scale_x_date(date_labels = "%m/%y", breaks = date_breaks("3 month")) ### Deu certo.


