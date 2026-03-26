library(tidyverse)
library(readxl)
setwd(here::here())

data <- read_excel("./data/BASE.xlsx", sheet = 1)

head(data)

data_limpo = data %>%
  filter(
    `Categorização Atributos` != -1,
    `Categorização Consequências` != -1,
    `Categorização Valores` != -1
  ) %>%
  rename(ID = Ordem)

head(data_limpo)

# Adiciona os identificadores entre parênteses
atr <- paste0(as.character(data_limpo$`Categorização Atributos`), " (A)")
cons <- paste0(as.character(data_limpo$`Categorização Consequências`), " (C)")
val <- paste0(as.character(data_limpo$`Categorização Valores`), " (V)")

# Garante a ordem estruturada: primeiro Atributos, depois Consequências e depois Valores
niveis <- c(sort(unique(atr)), sort(unique(cons)), sort(unique(val)))

A <- factor(atr, levels = niveis)
C <- factor(cons, levels = niveis)
V <- factor(val, levels = niveis)

# Cria as tabelas de contingência das ligações (Atributos -> Consequências e Consequências -> Valores)
matriz_ac <- table(A, C)
matriz_cv <- table(C, V)
matriz_direta <- matriz_ac + matriz_cv

matriz_indireta <- table(A, V)

# Cria a matriz combinada no formato "Direta | Indireta" preservando nomes das linhas e colunas
matriz_interacoes <- matrix(
  paste(matriz_direta, matriz_indireta, sep = " | "),
  nrow = nrow(matriz_direta),
  dimnames = dimnames(matriz_direta)
)

matriz_interacoes[matriz_interacoes == "0 | 0"] <- ""
matriz_interacoes[!upper.tri(matriz_interacoes, diag = TRUE)] <- NA

View(matriz_interacoes)
