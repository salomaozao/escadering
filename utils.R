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
  if("weight" %in% names(conexoes)) {
    conexoes <- conexoes %>% filter(weight > 0)
  }
  
  rodando_limpeza <- TRUE
  while (rodando_limpeza) {
    qnt_linhas <- nrow(conexoes)
    
    # Uma Consequência precisa ser destino de algo e origem de algo
    consequencias_recebidas <- unique(conexoes$to[grepl("\\(C\\)", conexoes$to)])
    consequencias_enviadas <- unique(conexoes$from[grepl("\\(C\\)", conexoes$from)])
    consequencias_inteiras <- intersect(consequencias_recebidas, consequencias_enviadas)
    
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
plotar_mapa_hierarquico <- function(conexoes, levelSeparation = 150, nodeSpacing = 150, multTamanhoMultiplicador = 1) {
  # Criar grafo a partir de data frame
  rede <- graph_from_data_frame(conexoes, directed = TRUE)
  
  # Identidade visual conforme a tag string
  V(rede)$color <- ifelse(
    grepl("\\(A\\)", V(rede)$name), "lightblue",
    ifelse(grepl("\\(C\\)", V(rede)$name), "orange", "lightgreen")
  )
  
  # ESTILO NÓS
  nos <- data.frame(
    id = V(rede)$name,
    label = V(rede)$name,
    color = V(rede)$color,
    value = degree(rede) * multTamanhoMultiplicador,
    level = ifelse(grepl("\\(A\\)", V(rede)$name), 1,
              ifelse(grepl("\\(C\\)", V(rede)$name), 2, 3))
  )
  
  # ESTILO ARESTAS
  linhas <- data.frame(
    from = conexoes$from,
    to = conexoes$to,
    value = conexoes$weight,
    color = list(color = "#848484", highlight = "#d12a2a"),
    arrows = "to"
  )
  
  # HTML/JS INTERATIVO
  visNetwork(nos, linhas, main = "Mapa Hierárquico de Valores (Laddering)", width = "100%", height = "700px") %>%
    visHierarchicalLayout(
      direction = "UD",
      levelSeparation = levelSeparation,
      nodeSpacing = nodeSpacing
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = TRUE
    ) %>%
    visInteraction(
      navigationButtons = TRUE,
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE
    )
}
