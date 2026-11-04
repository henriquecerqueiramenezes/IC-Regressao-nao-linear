#Iniciacao Cientifica (PIBIC VOLUNTARIA)
#MODELOS NAO LINEARES CRESCIMENTO DA PIMENTA BIQUINHO
#Orientando: Henrique Cerqueira
#Orientador: Rick Mangueira

#1.PREPARAÇÃO DO AMBIENTE --------------------------------------------------

library(dplyr) #Para manipulação de dados
library(readxl) #Para entrar com os dados do Excel
library(ggplot2) #Para criação dos Gráficos

#2.IMPORTANDO OS DADOS ----------------------------------------------------

dad<-read_excel(file.choose())

#3. PRÉ-PROCESSAMENTO DOS DADOS ---------------------------------------------

# ---- Estrutura dos dados ----
head(dad) #Verificar os dados
dim(dad) #Dimensão dos dados
str(dad) #Estrutura dos dados
summary(dad) #Resumo geral dos dados

# ---- Qualidade dos dados ----
colSums(is.na(dad))# Quantidade de valores ausentes por variavel
sum(duplicated(dad))# Linhas duplicadas
sapply(dad, \(x) length(unique(x)))# Quantidade de valores distintos por variavel

# ---- Variáveis Numericas ----

#### Altura ####

summary(dad$altura)

mean(dad$altura, na.rm = TRUE) #Média
median(dad$altura, na.rm = TRUE) #Mediana
sd(dad$altura, na.rm = TRUE) #Desvio Padrão
IQR(dad$altura, na.rm = TRUE) #Intervalo Interquartil

#### Diametro ####

summary(dad$diametro)

mean(dad$diametro, na.rm = TRUE) #Média
median(dad$diametro, na.rm = TRUE) #Mediana
sd(dad$diametro, na.rm = TRUE) #Desvio Padrão
IQR(dad$diametro, na.rm = TRUE) #Intervalo Interquartil

# ---- Variáveis Categoricas ----

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

#4. ANÁLISE EXPLORATÓRIA DE DADOS --------------------------------------------

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

## ---- Visualização dos Dados ----

#---- Distribuição dos dado ----

# Histograma Altura da Pimenta
ggplot(dad, aes(x = altura)) +
geom_histogram(bins = 20, color = "white") +
  labs(
    title = "Distribuicao da Altura da Pimenta",
    x = "Altura(cm)",
    y = "Frequência"
  ) +
  theme_minimal()

# Histograma Diametro da Pimenta
ggplot(dad, aes(x = diametro)) +
  geom_histogram(bins = 20, color = "white") +
  labs(
    title = "Distribuição da Diametro da Pimenta",
    x = "Diametro(cm)",
    y = "Frequência"
  ) +
  theme_minimal()

#Gráfico de Densidade Altura da Pimenta
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
  theme_minimal() # Não é um gráfico muito interessante para nossa análise

#---- Graficos crescimento temporal ----

# Gráfico de Dispersão da altura ao longo dos dias
ggplot(dad, aes(x = dias, y = altura)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Dispersão da Altura x Dias da Pimenta Biquinho",
    x = "Tempo(Dias)",
    y = "Altura(cm)"
  ) +
  theme_minimal() 

# Gráfico de Dispersão do Diametro ao longo dos dias
ggplot(dad, aes(x = dias, y = diametro)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Dispersão do Diametro x Dias da Pimenta Biquinho",
    x = "Tempo(Dias)",
    y = "Diametro(cm)"
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

#Gráfico das médias da altura com barras de erro
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
    y = "Altura média",
    color = "Lâmina",
    title = "Crescimento médio da altura"
  ) +
  theme_minimal()


#Gráfico das médias do diametro com barras de erro 
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
    y = "Diametro médio",
    color = "Lâmina",
    title = "Crescimento médio do diametro"
  ) +
  theme_minimal()

# ---- Graficos de Comparação Entre tratamentos ----

#Gráfico de Crescimento Sigmoid Altura média por tipo de água e lâmina
ggplot(resumo_trat,
       aes(x = dias, y = media_altura,
           color = factor(lamina),
           group = lamina)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~`Tipo de agua`) +
  labs(
    x = "Dias após a semeadura",
    y = "Altura média",
    color = "Lâmina",
    title = "Altura média por tipo de água e lâmina"
  ) +
  theme_minimal()

#Gráfico de Crescimento Sigmoid Altura média por lâmina e tipo de água
ggplot(resumo_trat,
       aes(x = dias, y = media_altura,
           color = factor(`Tipo de agua`),
           group = `Tipo de agua`)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~lamina) +
  labs(
    x = "Dias após a semeadura",
    y = "Altura média",
    color = "Tipo de água",
    title = "Altura média por lâmina e tipo de água"
  ) +
  theme_minimal()
