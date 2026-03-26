library(tidyverse)
library(readxl)
setwd(here::here())

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
  filter(weight > 2) # Ajuste o cut-off conforme sua amostra

# Criar o objeto de grafo
rede <- graph_from_data_frame(todas_conexoes, directed = TRUE)

# Definir cores baseadas nos sufixos que você criou
V(rede)$color <- ifelse(
  grepl("\\(A\\)", V(rede)$name),
  "lightblue",
  ifelse(grepl("\\(C\\)", V(rede)$name), "orange", "lightgreen")
)

# Ajustar o tamanho dos nós pelo grau de conexão
V(rede)$size <- degree(rede) * 2

# ============ GRAFO INTERATIVO E ESTILIZADO (visNetwork)
# Como você pediu um gráfico interativo com zoom, a melhor opção no R é o `visNetwork`.
# Descomente a linha abaixo para instalar caso ainda não tenha no seu computador:
# install.packages("visNetwork")
library(visNetwork)

# 1. ESTILO DOS NÓS (Pontos do Grafo) =================================
nos <- data.frame(
  id = V(rede)$name,
  label = V(rede)$name, # Define qual será o texto dentro da bolinha
  color = V(rede)$color, # Puxa as cores de atributos, conquências e valores setadas acima
  value = degree(rede), # O 'value' ajusta automaticamente o tamanho do nó segundo seu "peso"/ocorrências

  # Aqui forçamos exatamente as 3 camadas originais (como o sugerido pelo layout_with_sugiyama)
  level = ifelse(
    grepl("\\(A\\)", V(rede)$name),
    1,
    ifelse(grepl("\\(C\\)", V(rede)$name), 2, 3)
  )
)

# 2. ESTILO DAS ARESTAS (Linhas de conexão) ===========================
linhas <- data.frame(
  from = todas_conexoes$from,
  to = todas_conexoes$to,
  value = todas_conexoes$weight, # Argumento 'value' aumenta automaticamente a grossura da linha onde a relação é mais forte
  color = list(color = "#848484", highlight = "#d12a2a"), # Linha por padrão é cinza, mas fica vermelha focada no clique
  arrows = "to" # Desenha uma flecha identificando qual a direção da consequência (causa/efeito)
)

# 3. RENDERIZAÇÃO COM COMPORTAMENTOS (Zoom, drag, etc) ================
visNetwork(
  nos,
  linhas,
  main = "Mapa Hierárquico de Valores (Laddering)",
  width = "100%",
  height = "700px"
) %>%

  # visHierarchicalLayout: Garante que o desenho seja construído como hierarquia (escada)
  # direction = "UD" = "Up-Down" (De cima para baixo). O R calcula os blocos para ficarem empilhados bonitos.
  visHierarchicalLayout(direction = "UD", levelSeparation = 150) %>%

  # visOptions: Efeitos de highlight ao selecionar os itens
  # nodesIdSelection cria uma caixa de pesquisa/combobox nativa para você encontrar um termo rápido
  # highlightNearest esconde os caminhos que não têm relação com o que você clicou ou passou o mouse (hover)
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection = TRUE
  ) %>%

  # visInteraction: Funcionalidades HTML exclusivas do clique / rolagem do mouse
  # zoomView = TRUE já ativa que o scroll do mouse ou os botões aproximem ou distanciem a tela.
  visInteraction(
    navigationButtons = TRUE,
    dragNodes = TRUE,
    dragView = TRUE,
    zoomView = TRUE
  )
