#Iniciacao Cientifica (PIBIC VOLUNTARIA)
#MODELOS NAO LINEARES CRESCIMENTO DA PIMENTA BIQUINHO
#Orientando: Henrique Cerqueira
#Orientador: Rick Mangueira

#1.PREPARACAO DO AMBIENTE --------------------------------------------------

library(dplyr) #Para manipulacao de dados
library(readxl) #Para entrar com os dados do Excel
library(ggplot2) #Para criacao dos Graficos
library(ExpDes) #Para ANOVA
library(broom) #Para organizar Tabela

#2.IMPORTANDO OS DADOS ----------------------------------------------------

dad<-read_excel(file.choose())

#3.PREPARAÇÃO DADOS -------------------------------

#Classificando cada variável
dad <- dad %>%
  mutate(
    bloco = factor(bloco),
    `Tipo de agua` = factor(`Tipo de agua`),#torna categórico
    lamina = factor(lamina),#torna categorico
    dias = as.numeric(dias)
  )

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

# 4.1 ANOVA UTILIZANDO EXPDES --------------------------------

anova_expdes <- function(data, dia, resposta) {

  dados_dia <- data %>%
    filter(dias == dia) %>%
    filter(!is.na(.data[[resposta]]))

  cat("\n====================================\n")
  cat("DIA:", dia, "\n")
  cat("RESPOSTA:", resposta, "\n")
  cat("====================================\n")

  if (nrow(dados_dia) == 0) {
    stop("Não existem observações para esse dia e variável.")
  }

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

# Dias selecionados para a ANOVA
dias_selecionados <- c(23, 51, 79, 107, 135, 163)

# ALTURA ANOVA DIAS SELECIONADOS ------------------------------------------
for (d in dias_selecionados) {
  anova_expdes(dad, d, "altura")
}

# DIAMETRO ANOVA DIAS SELECIONADOS ----------------------------------------

for (d in sort(unique(dad$dias))) {
  anova_expdes(dad, d, "diametro")
}

# 5. FUNÇÃO PARA EXTRAIR OS RESULTADOS DA ANOVA -------------

anova_aov <- function(data, dia, resposta) {
  
  dados_dia <- data %>%
    filter(dias == dia) %>%
    filter(!is.na(.data[[resposta]]))
  
  modelo <- aov(
    as.formula(
      paste0(
        "`", resposta, "` ~ bloco + `Tipo de agua` * lamina"
      )
    ),
    data = dados_dia
  )
  
  tabela <- broom::tidy(anova(modelo))
  
  tabela %>%
    filter(term != "Residuals") %>%
    mutate(
      dias = dia,
      resposta = resposta
    )
}

# RESULTADOS DA ALTURA --------------------------------------

tab_altura <- bind_rows(
  lapply(
    dias_selecionados,
    function(d) anova_aov(dad, d, "altura")
  )
)

# RESULTADOS DO DIAMETRO ------------------------------------
tab_diametro <- bind_rows(
  lapply(
    dias_selecionados,
    function(d) anova_aov(dad, d, "diametro")
  )
)

#Visualização
print(tab_altura, n= 25)
print(tab_diametro, n=25)
