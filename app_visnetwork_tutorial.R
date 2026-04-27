library(shiny)
library(visNetwork)
library(dplyr)

# 1. Preparação dos Dados (3 camadas: Atributos -> Consequências -> Valores)
nodes <- data.frame(
  id = 1:9,
  label = c(paste("Atributo", 1:3), paste("Consequência", 1:3), paste("Valor", 1:3)),
  group = c(rep("A", 3), rep("C", 3), rep("V", 3)),
  title = paste("<p>Informação extra sobre o nó", 1:9, "</p>"), # Tooltip aceita HTML
  level = c(rep(1, 3), rep(2, 3), rep(3, 3)), # Fundamental para layout hierárquico
  value = c(20, 15, 25, 30, 20, 40, 50, 60, 45) # Tamanho do nó (depende do shape)
)

edges <- data.frame(
  from = c(1, 1, 2, 3, 4, 4, 5, 6),
  to =   c(4, 5, 5, 6, 7, 8, 8, 9),
  label = paste("Peso:", sample(1:5, 8, replace = TRUE)),
  width = sample(1:5, 8, replace = TRUE), # Espessura da linha
  dashes = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE) # Linhas tracejadas
)

# 2. Interface do Usuário (UI)
ui <- fluidPage(
  titlePanel("Tutorial Interativo: Explorando o visNetwork"),
  p("Este app demonstra as principais opções visuais para a construção de grafos com três camadas (A-C-V)."),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Layout e Física"),
      checkboxInput("hierarchical", "Layout Hierárquico (Esquerda -> Direita)", value = TRUE),
      checkboxInput("physics", "Física (Animação de repulsão/molas)", value = FALSE),
      
      hr(),
      h4("Configurações dos Nós"),
      selectInput("nodeShape", "Formato (Shape)", 
                  choices = c("dot", "ellipse", "circle", "box", "text", "diamond", "star", "triangle"), 
                  selected = "dot"),
      checkboxInput("shadowNodes", "Sombra nos Nós", value = TRUE),
      checkboxInput("scaleNodes", "Tamanho pelo 'value'", value = TRUE),
      
      hr(),
      h4("Configurações das Arestas"),
      selectInput("edgeSmooth", "Estilo de Curva", 
                  choices = c("dynamic", "continuous", "discrete", "diagonalCross", "straightCross", "horizontal", "vertical", "curvedCW", "curvedCCW", "cubicBezier", "Nenhuma"), 
                  selected = "cubicBezier"),
      selectInput("arrowPosition", "Posições das Setas", 
                  choices = c("to", "middle", "from", "to;from", "Sem seta"), 
                  selected = "to"),
      checkboxInput("shadowEdges", "Sombra nas Arestas", value = FALSE),
      
      hr(),
      h4("Interatividade"),
      checkboxInput("hover", "Tooltip no Hover (Precisa de título no nó)", value = TRUE),
      checkboxInput("highlightNearest", "Destacar Conectados", value = TRUE),
      sliderInput("highlightDegree", "Grau (profundidade) de destaque", min = 1, max = 3, value = 1, step = 1),
      checkboxInput("nodesIdSelection", "Menu de Seleção de Nós", value = TRUE)
    ),
    
    mainPanel(
      width = 8,
      visNetworkOutput("rede", height = "750px")
    )
  )
)

# 3. Servidor (Server)
server <- function(input, output) {
  
  output$rede <- renderVisNetwork({
    # Usa ou remove a variável value dependendo da seleção
    current_nodes <- nodes
    if (!input$scaleNodes) {
      current_nodes$value <- NULL
    }
    
    vn <- visNetwork(current_nodes, edges, width = "100%", height = "100%") %>%
      
      # Cores por grupo (A, C, V)
      visGroups(groupname = "A", color = list(background = "#A3C1DA", border = "#3A6FA2", highlight = "#F6C15B")) %>%
      visGroups(groupname = "C", color = list(background = "#C5DFB2", border = "#5B8B3D", highlight = "#F6C15B")) %>%
      visGroups(groupname = "V", color = list(background = "#F9B7A9", border = "#D24D3E", highlight = "#F6C15B")) %>%
      
      # Física
      visPhysics(
        enabled = input$physics,
        solver = "repulsion" # força de repulsão entre nós
      )
    
    # Hierárquico
    if (input$hierarchical) {
      vn <- vn %>% visHierarchicalLayout(
        direction = "LR", # Left to Right
        levelSeparation = 200,
        nodeSpacing = 100,
        sortMethod = "directed"
          )
    }
    
    # Opções Globais dos Nós
    vn <- vn %>% visNodes(
      shape = input$nodeShape,
      shadow = input$shadowNodes,
      font = list(size = 18, face = "Arial", color = "#333333"),
      borderWidth = 2,
      borderWidthSelected = 4
    )
    
    # Lógica de curvar linha
    smooth_config <- if (input$edgeSmooth == "Nenhuma") FALSE else list(enabled = TRUE, type = input$edgeSmooth, roundness = 0.5)
    
    # Lógica de setas
    arrows_config <- if (input$arrowPosition == "Sem seta") "" else input$arrowPosition
    
    # Opções Globais das Arestas
    vn <- vn %>% visEdges(
      smooth = smooth_config,
      shadow = input$shadowEdges,
      arrows = arrows_config,
      color = list(color = "#848484", highlight = "#FF0000", hover = "#848484"),
      font = list(size = 12, align = "top") # Mostra os labels nas arestas
    )
    
    # Interatividade (hover, highlight, selection)
    vn <- vn %>% visInteraction(
      hover = input$hover,
      hoverConnectedEdges = TRUE,
      selectConnectedEdges = TRUE,
      tooltipDelay = 200 # Atraso para mostrar tooltip
    ) %>%
      visOptions(
        highlightNearest = list(
          enabled = input$highlightNearest, 
          degree = input$highlightDegree, 
          hover = input$hover
        ),
        nodesIdSelection = input$nodesIdSelection
      )
    
    vn
  })
}

shinyApp(ui = ui, server = server)
