library(tidyverse)
library(readxl)
setwd(here::here())
source("utils.R")

data <- read_excel("./data/BASE.xlsx", sheet = 1)

head(data)


# ============ LEITURA E PROCESSAMENTO
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


# ============ TABELA DE INTERAÇÕES

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

# View(matriz_interacoes)

# ============ MAPA HIERÁRQUICO DE VALORES
library(igraph)

# Criar um data frame de conexões (de -> para) com pesos padrão
links_ac <- as.data.frame(as.table(matriz_ac)) %>%
  rename(from = A, to = C, weight = Freq)
links_cv <- as.data.frame(as.table(matriz_cv)) %>%
  rename(from = C, to = V, weight = Freq)

# Unir as conexões e filtrar pelo ponto de corte (ex: conexões > 1)
todas_conexoes <- bind_rows(links_ac, links_cv) %>%
  filter(weight > 2) # Ajuste o cut-off conforme sua quantidade de dados

# GARANTIA DE LADDERING COMPLETO (A -> C -> V) ====================
# A função abaixo varre a base recursivamente até sobrar apenas cadeias 3-níveis intactas
todas_conexoes <- filtrar_laddering(todas_conexoes)

# ============ MAPA HIERÁRQUICO DE VALORES ==========================
# Plota a versão HTML do grafo, controlando níveis, distâncias e interatividade nativa
plotar_mapa_hierarquico(
  conexoes = todas_conexoes,
  levelSeparation = 150,
  nodeSpacing = 1500,
  multTamanhoMultiplicador = 5
)
