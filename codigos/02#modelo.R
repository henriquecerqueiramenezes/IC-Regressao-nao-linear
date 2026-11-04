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

#3.PREPARAÇÃO DADOS -------------------------------

#Classificando cada variáveil
dad <- dad %>%
  mutate(
    bloco = factor(bloco),
    `Tipo de agua` = factor(`Tipo de agua`),#torna categórico
    lamina = factor(lamina),#torna categorico
    dias = as.numeric(dias)
  )

#Verificando Balancamento
with(dad23, table(bloco, `Tipo de agua`, lamina))

#4.ANOVA -------------------------------------------

#Testando uma ANOVA
dad23 <- dad %>%
  filter(dias == 23)

fat2.rbd(
  factor1 = dad23$`Tipo de agua`,
  factor2 = dad23$lamina,
  block = dad23$bloco,
  resp = dad23$altura,
  quali = c(TRUE, TRUE),
  mcomp = "tukey",
  fac.names = c("Tipo de agua", "Lamina"),
  sigT = 0.05,
  sigF = 0.05
)

# Função de ANOVA utilizando ExpDes
anova_expdes <- function(data, dia, resposta) {
  
  dados_dia <- data %>%
    filter(dias == dia) %>%
    drop_na(all_of(resposta))
  
  cat("\n====================================\n")
  cat("DIA:", dia, "\n")
  cat("RESPOSTA:", resposta, "\n")
  cat("====================================\n")
  
  fat2.rbd(
    factor1 = dados_dia$`Tipo de agua`,
    factor2 = dados_dia$lamina,
    block = dados_dia$bloco,
    resp = dados_dia[[resposta]],
    quali = c(TRUE, TRUE),
    mcomp = "tukey",
    fac.names = c("Tipo de agua", "Lamina"),
    sigT = 0.05,
    sigF = 0.05
  )
}


# Altura em todos os dias
for (d in sort(unique(dad$dias))) {
  anova_expdes(dad, d, "altura")
}

# Diâmetro em todos os dias
for (d in sort(unique(dad$dias))) {
  anova_expdes(dad, d, "diametro")
}

######################

anova_aov <- function(data, dia, resposta) {

  dados_dia <- data %>%
    filter(dias == dia) %>%
    drop_na(all_of(resposta))

  modelo <- aov(
    as.formula(
      paste0(
        resposta,
        " ~ bloco + `Tipo de agua` * lamina"
      )
    ),
    data = dados_dia
  )

  tabela <- broom::tidy(anova(modelo))

  tabela %>%
    mutate(
      dias = dia,
      resposta = resposta
    )
}

# Altura
tab_altura <- bind_rows(
  lapply(
    sort(unique(dad$dias)),
    function(d) anova_aov(dad, d, "altura")
  )
)

# Diâmetro
tab_diametro <- bind_rows(
  lapply(
    sort(unique(dad$dias)),
    function(d) anova_aov(dad, d, "diametro")
  )
)

# Visualizar
tab_altura
tab_diametro

# Exportar
write.csv2(tab_altura, "anova_altura.csv", row.names = FALSE)
write.csv2(tab_diametro, "anova_diametro.csv", row.names = FALSE)


# Altura em todos os dias
for (d in sort(unique(dad$dias))) {
  anova_expdes(dad, d, "altura")
}

# Diâmetro em todos os dias
for (d in sort(unique(dad$dias))) {
  anova_expdes(dad, d, "diametro")
}


## ---- DELINEAMENTO EM BLOCO NO ARRANJO FATORIAL (analise de variancia anova) RASCUNHO----
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

## ---- ANOVA Completo ----
dad23 <- dad %>%
  filter(dias==23)

modelo23 <- aov(
  altura ~ bloco + `Tipo de agua` * lamina,
  data = dad23
)

anova(modelo23)
