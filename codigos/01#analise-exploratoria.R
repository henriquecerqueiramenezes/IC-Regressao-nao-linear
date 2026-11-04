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

mean(dad$altura, na.rm = TRUE) #Media
median(dad$altura, na.rm = TRUE) #Mediana
sd(dad$altura, na.rm = TRUE) #Desvio Padrao
IQR(dad$altura, na.rm = TRUE) #Intervalo Interquartil

#### Diametro ####

summary(dad$diametro)

mean(dad$diametro, na.rm = TRUE) #Media
median(dad$diametro, na.rm = TRUE) #Mediana
sd(dad$diametro, na.rm = TRUE) #Desvio Padrao
IQR(dad$diametro, na.rm = TRUE) #Intervalo Interquartil

# ---- Variaveis Categoricas ----

#### Tipo de Agua ####
#1-abastecimento
#2-residuaria
table(dad$`Tipo de agua`)
prop.table(table(dad$`Tipo de agua`)) * 100

#### Lamina ####
#1-20% da necessidade hidrica
#2-40% da necessidade hidrica
#3-60% da necessidade hidrica
#4-80% da necessidade hidrica
#5-100% da necessidade hidrica
table(dad$lamina)
prop.table(table(dad$lamina)) * 100

#### Planta ####

table(dad$planta)
prop.table(table(dad$planta)) * 100

#### Dias ####

table(dad$dias)
prop.table(table(dad$dias)) * 100

#---- Possibilidades ----
dad %>%
  count(`Tipo de agua`, lamina)

dad %>%
  filter(
    altura <= 0 |
      diametro <= 0
  )

#4. ANALISE EXPLORATORIA DE DADOS --------------------------------------------

## ---- Agrupamentos ----

#Estatistica Descritiva por Tempo
resumo <- dad %>%
  group_by(dias) %>%
  summarise(
    n = n(),
    media_altura = mean(altura, na.rm = TRUE),
    dp_altura = sd(altura, na.rm = TRUE),
    media_diametro = mean(diametro, na.rm = TRUE),
    dp_diametro = sd(diametro, na.rm = TRUE)
  )

resumo

#Estatisitca por Tratamento e Tempo
resumo_trat <- dad %>%
  group_by(`Tipo de agua`, lamina, dias) %>%
  summarise(
    n = sum(!is.na(altura)),
    media_altura = mean(altura, na.rm = TRUE),
    dp_altura = sd(altura, na.rm = TRUE),
    media_diametro = mean(diametro, na.rm = TRUE),
    dp_diametro = sd(diametro, na.rm = TRUE),
    .groups = "drop"
  )

resumo_trat

## ---- Visualizacao dos Dados ----

#---- Distribuição dos dado ----

# Histograma Altura da Pimenta
ggplot(dad, aes(x = altura)) +
geom_histogram(bins = 20, color = "white") +
  labs(
    title = "Distribuicao da Altura da Pimenta",
    x = "Altura(cm)",
    y = "Frequencia"
  ) +
  theme_minimal()

# Histograma Diametro da Pimenta
ggplot(dad, aes(x = diametro)) +
  geom_histogram(bins = 20, color = "white") +
  labs(
    title = "Distribuicao da Diametro da Pimenta",
    x = "Diametro(cm)",
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

#Grafico de Densidade Diametro da Pimenta
ggplot(dad, aes(x= diametro)) +
  geom_density() +
  labs(
    title = "Densidade do Diametro da Pimenta",
    x = "Diametro(cm)",
    y = "Frequência"
  ) +
  theme_minimal() # Nao é um grafico muito interessante para nossa analise

#---- Graficos crescimento temporal ----

# Grafico de Dispersao da altura ao longo dos dias
ggplot(dad, aes(x = dias, y = altura)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Dispersao da Altura x Dias da Pimenta Biquinho",
    x = "Tempo(Dias)",
    y = "Altura(Cm)"
  ) +
  theme_minimal() 

# Grafico de Dispersao do Diametro ao longo dos dias
ggplot(dad, aes(x = dias, y = diametro)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Dispersao do Diametro x Dias da Pimenta Biquinho",
    x = "Tempo(Dias)",
    y = "Diametro(Cm)"
  ) +
  theme_minimal() 

#BoxPlot da altura ao longo dos dias
ggplot(dad, aes(x = dias, y = altura, group= dias)) +
  geom_boxplot(aes(fill = dias)) +
  theme_minimal()

#BoxPlot da Diametro ao longo dos dias
ggplot(dad, aes(x = dias, y = diametro, group= dias)) +
  geom_boxplot(aes(fill = dias)) +
  theme_minimal()

#Grafico das medias da altura com barras de erro
ggplot(resumo_trat,
       aes(x = dias, y = media_altura,
           color = factor(lamina),
           group = lamina)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = media_altura - dp_altura,
      ymax = media_altura + dp_altura
    ),
    width = 2
  ) +
  labs(
    x = "Tempo(Dias)",
    y = "Altura media",
    color = "Lamina",
    title = "Crescimento medio da altura"
  ) +
  theme_minimal()


#Grafico das medias do diametro com barras de erro 
ggplot(resumo_trat,
       aes(x = dias, y = media_diametro,
           color = factor(lamina),
           group = lamina)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = media_diametro - dp_diametro,
      ymax = media_diametro + dp_diametro
    ),
    width = 2
  ) +
  labs(
    x = "Tempo(Dias)",
    y = "Diametro medio",
    color = "Lâmina",
    title = "Crescimento medio do diametro"
  ) +
  theme_minimal()


