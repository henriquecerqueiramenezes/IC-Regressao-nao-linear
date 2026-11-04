#Iniciacao Cientifica (PIBIC VOLUNTARIA)
#MODELOS NAO LINEARES CRESCIMENTO DA PIMENTA BIQUINHO
#Orientando: Henrique Cerqueira
#Orientador: Rick Mangueira

#1.PREPARACAO DO AMBIENTE --------------------------------------------------

library(dplyr) #Para manipulacao de dados
library(readxl) #Para entrar com os dados do Excel
library(ggplot2) #Para criacao dos Graficos
library(ExpDes)

#2.IMPORTANDO OS DADOS ----------------------------------------------------

dad<-read_excel(file.choose())

## ---- DELINEAMENTO EM BLOCO NO ARRANJO FATORIAL (analise de variancia anova)
names(dad)
dad23 <- dad %>%
  filter(dias==23)
modelo23 <- aov(dad23$altura~dad23$bloco+dad23$`Tipo de agua`*dad23$lamina)
anova(modelo23)

dad30 <- dad %>%
  filter(dias==30)
dad30
modelo30 <- aov(dad30$altura~dad30$bloco+dad30$`Tipo de agua`*dad30$lamina)
anova(modelo30)

#pacote ExpDes
fat2.rbd(
  dad23$`Tipo de agua`,
  dad23$lamina,
  dad23$bloco,
  dad23$altura)

