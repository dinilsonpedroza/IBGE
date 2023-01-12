################################################################################
################ PIM-PF - Janeiro de 2022 ######################################
################################################################################
getwd()
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)


#### Pegando as séries (baixadas no pc em csv_US)
pimpf<-read.csv("pf.csv", header = T) # Salvei o arquivo em CSV.US.


########### Para os arquivos separados (arquivos baixados em csv_BR)
#pfba= read.csv2('pimpfba', header = T)#
#pfpe<-read.csv2('pimpfpe', header = T)#
#pfce<-read.csv2("pimpfce", header = T)#
#pfbr<-read.csv2("pimpfbr", header = T)#
#### Vetor de datas
#mes <- seq.Date(from = as_date("2019-01-01"), to = as_date("2022-10-01"),"month")
#### Criando um data frame
# pfjan22<-data.frame(mes, pfbr, pfce, pfpe, pfba)

#### Vetor de datas
mes <- seq.Date(from = as_date("2019-01-01"),
                to = as_date("2022-10-01"),
                "month")
#### Criando um data frame
pfjan22<-data.frame(mes, pimpf)

#### Gráficos

ggplot(pfjan22, aes(x=mes))+theme_economist()+
  geom_hline(yintercept=0, colour='red', linetype='dashed')+
  geom_line(aes(y= BR, colour="Brasil"), linewidth =0.8)+
  geom_line(aes(y= NE, colour = "NE"), linewidth = 0.8)+
  geom_line(aes(y= PE, colour = "PE"), linewidth = 0.8)+
    theme(axis.text.x=element_text(angle=45, hjust=0.05), axis.text.y = element_text (angle=0, vjust = 0.05),
        plot.title = element_text(size=10, face='bold', hjust=0.5), plot.subtitle = element_text(size=10, face='bold', hjust=0.5),legend.position = "top")+
  labs(x='Meses/Anos', y='PIM-PF(%)',
       title='PIM-PF - Variação mensal',subtitle = '(2019-2022)',
       caption='IBGE, UE-PE', color = "") +
  scale_x_date(date_labels = "%m/%y", breaks = date_breaks("3 month")) ### Deu certo.

### Setores ####################################################################
################################################################################

pimpf2<-read.csv("pf2.csv", header = T) # Salvei o arquivo em CSV.US.

### Gráfico 1

ggplot(pimpf2, aes(y = var, x = Setor)) +
  geom_bar(stat = "identity", fill = "tomato")

### Gráfico 2
ggplot(pimpf2, aes(y = var, x = Setor, fill = Setor)) +
  geom_bar(stat = "identity")

### Gráfico 3
ggplot(pimpf2, aes(y = var, x = Setor, fill = Setor)) +
  geom_bar(stat = "identity")+
  xlab("Setor") + 
  ylab("Variação (%)")

### Gŕafico 4
ggplot(pimpf2, aes(y = var, x = Setor, fill = Setor)) +
  geom_bar(stat = "identity")+
  xlab("Setor") + 
  ylab("Variação (%)")+
  theme(axis.text.x=element_text(angle=45, hjust=1)) ### ficou bom.

### Gŕafico 5
ggplot(pimpf2, aes(y = var, x = Setor))+
  geom_bar(stat = "identity", fill = "tomato")+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x='Setores', y='Variação (%)',
       title='PIM-PF de PE - Variação mensal de alguns setores',subtitle = '(2019-2022)',
       caption='IBGE, UE-PE', color = "") ### ficou bom.

### Gŕafico 6
ggplot(pimpf2, aes(y = var, x = Setor))+
  geom_bar(stat = "identity", fill = "blue")+
  theme(axis.text.x=element_text(angle=45, hjust=1),
  plot.title = element_text(size=10, face='bold', hjust=0.5), 
plot.subtitle = element_text(size=10, face='bold', hjust=0.5))+
  labs(x='Setores', y='Variação (%)',
       title='PIM-PF de PE - Variação acum. no ano/ano anterior de alguns setores',subtitle = 'Novembro de 22',
       caption='IBGE, UE-PE', color = "") ### ficou bom.

### Gráfico 7
ggplot(pimpf2, aes(y = var, x = Setor))+ theme_excel_new()+
  geom_bar(stat = "identity", fill = "orange")+
  geom_text(aes(label=var), vjust=-0.7, color="black",size=4)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        plot.title = element_text(size=10, face='bold', hjust=0.5), 
        plot.subtitle = element_text(size=10, face='bold', hjust=0.5))+
  labs(x='Setores', y='Variação (%)',
       title='PIM-PF de PE - Variação acum. no ano/ano anterior de alguns setores',subtitle = 'Novembro de 22',
       caption='IBGE, UE-PE', color = "") ### ficou bom.

### UF's #######################################################################
################################################################################

pimpf3<-read.csv("pf3.csv", header = T) # Salvei o arquivo em CSV.US.

### Gráfico 1
ggplot(pimpf3, aes(y = var, x = UF))+
  geom_bar(stat = "identity", fill = "green")+ theme_excel_new()+
  geom_text(aes(label=var), vjust=-0.7, color="black",size=4)+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        plot.title = element_text(size=10, face='bold', hjust=0.5), 
        plot.subtitle = element_text(size=10, face='bold', hjust=0.5))+
  labs(x='UF', y='Variação (%)',
       title='PIM-PF de PE - Variação acum. no ano/ano anterior de alguns setores',subtitle = 'Novembro de 22',
       caption='IBGE, UE-PE', color = "") ### ficou bom.
