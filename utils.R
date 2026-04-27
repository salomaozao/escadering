library(dplyr)
library(igraph)
library(visNetwork)

#' Filtra a base de conexões mantendo apenas cadeias completas de laddering (A -> C -> V)
#'
#' @param conexoes Data frame contendo as colunas 'from' e 'to'.
#' @return Data frame purificado (livre de nós orfãos).
filtrar_laddering <- function(conexoes) {
  # Proteção crírica: Elimina caminhos puramente estruturais criados pelo as.table().
  # Se uma linha com peso zero entrasse no loop, o R acharia que a "Consequência" possui uma rota válida para fora,
  # desenharia o A -> C, mas esconderia a perna C -> V na tela pq a linha teria '0 pixels' de grossura, criando nós pendurados na tela.
  if ("weight" %in% names(conexoes)) {
    conexoes <- conexoes %>% filter(weight > 0)
  }

  rodando_limpeza <- TRUE
  while (rodando_limpeza) {
    qnt_linhas <- nrow(conexoes)

    # Uma Consequência precisa ser destino de algo e origem de algo
    consequencias_recebidas <- unique(conexoes$to[grepl(
      "\\(C\\)",
      conexoes$to
    )])
    consequencias_enviadas <- unique(conexoes$from[grepl(
      "\\(C\\)",
      conexoes$from
    )])
    consequencias_inteiras <- intersect(
      consequencias_recebidas,
      consequencias_enviadas
    )

    # Mantemos apenas os pares conectados a consequências válidas
    conexoes <- conexoes %>%
      filter(
        (grepl("\\(A\\)", from) & to %in% consequencias_inteiras) |
          (from %in% consequencias_inteiras & grepl("\\(V\\)", to))
      )

    if (nrow(conexoes) == qnt_linhas) rodando_limpeza <- FALSE
  }
  return(conexoes)
}

#' Plota o Mapa Hierárquico Interativo esteticamente configurado em cima do pacote visNetwork
#'
#' @param conexoes Data frame com caminhos validados
#' @param levelSeparation Distância vertical no layout da árvore entre A e C, e entre C e V.
#' @param nodeSpacing Distância horizontal mínima entre os nós vizinhos na mesma camada.
#' @param multTamanhoMultiplicador Multiplicador do grau do nó para influenciar seu tamanho dinamicamente na tela.
plotar_mapa_hierarquico <- function(
  conexoes,
  levelSeparation = 150,
  nodeSpacing = 250,
  multTamanhoMultiplicador = 1
) {
  # Criar grafo a partir de data frame
  rede <- graph_from_data_frame(conexoes, directed = TRUE)

  # Identidade visual conforme a tag string (Será a cor da borda da bolinha)
  cor_da_borda <- ifelse(
    grepl("\\(A\\)", V(rede)$name),
    "#4ca1e3", # Azul
    ifelse(
      grepl("\\(C\\)", V(rede)$name),
      "#f39c12", # Laranja
      "#58d68d"
    ) # Verde
  )

  # ESTILO NÓS
  nos <- data.frame(
    id = V(rede)$name,
    label = V(rede)$name,
    shape = "box", # Coloca o nome dentro da bolinha (ellipse ajusta melhor o texto do que 'circle')
    borderWidth = 2, # Engrossa as linhas em volta do círculo
    color.background = "#FFFFFF", # O fundo agora é branco opaco pra literalmente esconder o pedaço da linha que entra no círculo!
    color.border = cor_da_borda,
    color.highlight.background = "white",
    color.highlight.border = "red", # Muda a borda ao dar zoom/foco
    font.color = "#111111", # Fonte graffit escuro legível
    # Em formas geométricas tipo circle, diminuímos significativamente a fonte base e a taxa
    # de taxa de crescimento para caber na tela como 1/3 do volume (bolhas mais finas e organizadas)
    # *0.3 pra diminuir o tamanho excessivo
    font.size = 12,
    level = ifelse(
      grepl("\\(A\\)", V(rede)$name),
      1,
      ifelse(grepl("\\(C\\)", V(rede)$name), 2, 3)
    )
  )

  # ESTILO ARESTAS
  linhas <- data.frame(
    from = conexoes$from,
    to = conexoes$to,
    value = conexoes$weight,
    title = paste("Força da conexão (Peso):", conexoes$weight) # Adicionando tooltip ao passar pelas arestas
  )

  # HTML/JS INTERATIVO
  visNetwork(
    nos,
    linhas,
    main = "Mapa Hierárquico de Valores (Laddering)",
    width = "100%",
    height = "700px"
  ) %>%
    visHierarchicalLayout(
      direction = "UD",
      levelSeparation = levelSeparation,
      nodeSpacing = nodeSpacing
    ) %>%
    visPhysics(enabled = FALSE) %>% # Desliga completamente qualquer simulação gravitacional/mola e "danças" da rede
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = TRUE
    ) %>%
    visNodes(
      shape = "box",
      margin = 12, # Adiciona uma excelente margem ao redor do texto, impedindo o "aperto" visual
      widthConstraint = list(maximum = 140) # TRUQUE DO WRAPPING: Impede que o quadrado cresça infinitamente na horizontal!
    ) %>%
    visEdges(
      smooth = list(
        type = "cubicBezier",
        forceDirection = "vertical",
        roundness = 0.5
      ), # Garante que as linhas sempre saem por baixo e entram por cima
      shadow = FALSE,
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.4)), # Reposicionando as setinhas (cabeça pequena)
      color = list(
        color = "rgba(165, 165, 165, 0.4)",
        highlight = "rgba(209, 42, 42, 1)",
        hover = "rgba(85, 85, 85, 0.9)"
      )
    )
}

#' Extrai métricas analíticas de rede de um grafo de laddering
#'
#' @param conexoes Data frame puro das arestas já filtradas (A->C->V)
#' @return Data frame tabular com os cálculos de centralidade extraídos de cada nó.
calcular_metricas_grafo <- function(conexoes) {
  # Opcional: ignoramos warnings de bibliotecas para algumas métricas não-direcionadas
  suppressWarnings({
    rede <- graph_from_data_frame(conexoes, directed = TRUE)

    # Adiciona colunas com strings amigáveis para o usuário na UI final
    metricas <- data.frame(
      `Nome do Elemento` = V(rede)$name,
      `Classificação` = ifelse(
        grepl("\\(A\\)", V(rede)$name),
        "Atributo",
        ifelse(grepl("\\(C\\)", V(rede)$name), "Consequência", "Valor")
      ),
      `Influências Recebidas (In-Degree)` = degree(rede, mode = "in"),
      `Influências Geradas (Out-Degree)` = degree(rede, mode = "out"),
      `Relevância Estratégica (Eigenvector)` = round(
        eigen_centrality(rede, directed = FALSE)$vector,
        3
      ),
      `Ponte Exclusiva (Betweenness)` = round(
        betweenness(rede, directed = FALSE),
        1
      ),
      check.names = FALSE
    )

    # Classifica da maior importância global para a menor pontuação
    metricas <- metricas[
      order(-metricas$`Relevância Estratégica (Eigenvector)`),
    ]
    rownames(metricas) <- NULL
  })

  return(metricas)
}
