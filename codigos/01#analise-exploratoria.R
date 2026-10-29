#Iniciacao Cientifica (PIBIC VOLUNTARIA)
#MODELOS NAO LINEARES CRESCIMENTO DA PIMENTA BIQUINHO
#Orientando: Henrique Cerqueira
#Orientador: Rick Mangueira

#1.PREPARACAO DO AMBIENTE --------------------------------------------------

library(dplyr) #Para manipulacao de dados
library(readxl) #Para entrar com os dados do Excel
library(ggplot2) #Para criacao dos Graficos

#2.IMPORTANDO OS DADOS ----------------------------------------------------

dad<-read_excel(file.choose())

#3. PRE-PROCESSAMENTO DOS DADOS ---------------------------------------------

# ---- Estrutura dos dados ----
head(dad) #Verificar os dados
dim(dad) #Dimensao dos dados
str(dad) #Estrutura dos dados
summary(dad) #Resumo geral dos dados

# ---- Qualidade dos dados ----
colSums(is.na(dad))# Quantidade de valores ausentes por variavel
sum(duplicated(dad))# Linhas duplicadas
sapply(dad, \(x) length(unique(x)))# Quantidade de valores distintos por variavel

# ---- Variaveis Numericas ----

#### Altura ####

summary(dad$altura)

mean(dad$altura, na.rm = TRUE)
median(dad$altura, na.rm = TRUE)
sd(dad$altura, na.rm = TRUE)
IQR(dad$altura, na.rm = TRUE)

#### Diametro ####

summary(dad$diametro)

mean(dad$diametro, na.rm = TRUE)
median(dad$diametro, na.rm = TRUE)
sd(dad$diametro, na.rm = TRUE)
IQR(dad$diametro, na.rm = TRUE)

# ---- Variaveis Categoricas ----

#### Tipo de Agua ####

table(dad$`Tipo de agua`)
prop.table(table(dad$`Tipo de agua`)) * 100

#### Lamina ####
table(dad$lamina)
prop.table(table(dad$lamina)) * 100

#### Planta ####

table(dad$planta)
prop.table(table(dad$planta)) * 100

#### Dias ####

table(dad$dias)
prop.table(table(dad$dias)) * 100

#4. ANALISE EXPLORATORIA DE DADOS --------------------------------------------

## ---- Visualizacao dos Dados ----

# Histograma Altura da Pimenta
ggplot(dad, aes(x = altura)) +
geom_histogram(bins = 20, color = "white") +
  labs(
    title = "Distribuicao da Altura da Pimenta",
    x = "Altura(cm)",
    y = "Frequencia"
  ) +
  theme_minimal()

#Grafico de Densidade Altura da Pimenta
ggplot(dad, aes(x= altura)) +
  geom_density() +
  labs(
    title = "Densidade da Altura da Pimenta",
    x = "Altura(cm)",
    y = "Frequência"
  ) +
  theme_minimal() # Nao é um grafico muito interessante para nossa analise

# Grafico de Dispersao da altura ao longo dos dias
ggplot(dad, aes(x = dias, y = altura)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Dispersao da Altura x Dias da Pimenta Biquinho",
    x = "Tempo(Dias)",
    y = "Altura(Cm)"
  ) +
  theme_minimal() 

#BoxPlot da altura ao longo dos dias
ggplot(dad, aes(x = dias, y = altura, group= dias)) +
  geom_boxplot(aes(fill = dias)) +
  theme_minimal()

#Grafico de Dispersao com Grupos
ggplot(dad, aes(x = altura, y = diametro, color = lamina, group=lamina)) +
  geom_point(alpha = 0.8) +
  labs(
    title = "Comprimento vs Largura da Pimenta (por Lamina)",
    x = "Pimenta.Altura",
    y = "Pimenta.Diametro"
  )



