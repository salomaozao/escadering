# app.R
# Pressione o botão "Run App" lá em cima no canto direito do RStudio para iniciar
# Caso você nunca tenha usado o shiny, rode `install.packages(c("shiny", "bslib", "DT"))` no console.

library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(visNetwork)
library(DT)
source("utils.R")

# 1. ARQUITETURA DA TELA FRONTAL ===============================================
ui <- page_sidebar(
  title = "Painel de Laddering Insights",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"), # Layout responsivo elegante

  sidebar = sidebar(
    width = 330,
    title = "Configuração do Modelo",

    fileInput("file1", "1. Importar Tabela (Excel)", accept = c(".xlsx")),
    numericInput(
      "sheet",
      "Número da Aba Interna",
      value = 1,
      min = 1,
      step = 1
    ),

    hr(),
    h5("Limpeza & Recorte (Filtros)"),
    sliderInput(
      "cutoff",
      "Força Mínima (Frequência >)",
      min = 0,
      max = 20,
      value = 0,
      step = 1
    ),

    # A seção abaixo captura demografias ou labels automaticamente
    selectInput(
      "coluna_grupo",
      "Filtro Dinâmico (Demografia):",
      choices = c("Nenhum")
    ),
    uiOutput("valor_grupo_ui"),

    hr(),
    h5("Moldura e Afastamentos (Design)"),
    sliderInput(
      "levelSep",
      "Esticar Verticalmente (Camadas)",
      min = 50,
      max = 500,
      value = 150,
      step = 10
    ),
    sliderInput(
      "nodeSpace",
      "Espalhar Lateralmente (Largura)",
      min = 50,
      max = 1500,
      value = 150,
      step = 50
    ),
    sliderInput(
      "sizeMult",
      "Destaque Visual das Bolas",
      min = 0.1, # Passos decimais mínimos
      max = 2, # Limitando o teto
      value = 1, # O padrão é o original
      step = 0.1 # Pulos finos
    )
  ),

  # A área principal agrupa ferramentas em 'Aba' (Cards)
  navset_card_tab(
    full_screen = TRUE,

    nav_panel(
      "Visão Hierárquica (HTML Interativo)",
      visNetworkOutput("rede_laddering", height = "80vh")
    ),

    nav_panel(
      "Métricas Analíticas de Influência",
      br(),
      div(
        style = "padding: 15px; background-color: #f8f9fa; border-left: 5px solid #2c3e50; border-radius: 5px;",
        markdown(
          "
**Como interpretar as métricas estratégicas da sua cadeia de valor?**

- **Influências Recebidas (In-Degree):** Conta quantas setas *chegam* no elemento. Valores e Consequências essenciais terão números altos aqui por serem o destino da expectativa dos clientes de vários produtos.
- **Influências Geradas (Out-Degree):** Conta quantas setas *partem* deste elemento. Atributos gatilho (matérias-primas que disparam várias jornadas positivas) estarão no topo.
- **Relevância Estratégica (Eigenvector):** Mede qualidade do node, não apenas o volume puro. Uma consequência conectada a nós altamente formadores de valor terá um índice alto. Aponta o *verdadeiro core* da pesquisa.
- **Ponte Exclusiva (Betweenness):** É um índice de 'gargalo matemático'. Se esse número é elevado, caminhos vitais entre os Atributos e os Valores *obrigatoriamente* cruzam por este elemento. Cortá-lo no design do serviço significa quebrar a cadeia de valor à frente.
                "
        )
      ),
      br(),
      DTOutput("tabela_metricas")
    )
  )
)

# 2. MOTOR LÓGICO TRASEIRO =====================================================
server <- function(input, output, session) {
  # 2.1 Puxa a memória inicial (Tenta achar a BASE sua por padrão, ou usar a upada via botão)
  dados_brutos <- reactive({
    if (is.null(input$file1)) {
      if (file.exists("./data/BASE.xlsx")) {
        return(read_excel("./data/BASE.xlsx", sheet = input$sheet))
      }
      return(NULL)
    }
    read_excel(input$file1$datapath, sheet = input$sheet)
  })

  # 2.2 Popula o mecanismo do filtro demográfico dinâmico com colunas que vc possua (ex: ordem, região)
  observe({
    req(dados_brutos())
    descarta <- c(
      "Categorização Atributos",
      "Categorização Consequências",
      "Categorização Valores",
      "ID",
      "Ordem"
    )
    opcoes <- setdiff(names(dados_brutos()), descarta)
    updateSelectInput(session, "coluna_grupo", choices = c("Nenhum", opcoes))
  })

  output$valor_grupo_ui <- renderUI({
    req(input$coluna_grupo, input$coluna_grupo != "Nenhum", dados_brutos())
    valores <- unique(na.omit(dados_brutos()[[input$coluna_grupo]]))
    selectInput(
      "valor_grupo",
      "Analisar somente perfil:",
      choices = c("Todos", as.character(valores))
    )
  })

  # 2.3 Refaz a matemática todinha numa fração de segundo quando um Slider / Filtro muda de lugar
  dados_processados <- reactive({
    dados <- dados_brutos()
    req(dados)

    # Corte Direcionando para uma Demografia/Grupo Escolhido
    if (
      !is.null(input$coluna_grupo) &&
        input$coluna_grupo != "Nenhum" &&
        !is.null(input$valor_grupo)
    ) {
      if (input$valor_grupo != "Todos") {
        dados <- dados[dados[[input$coluna_grupo]] == input$valor_grupo, ]
      }
    }

    # Descartáveis Incompletos:
    dados <- dados %>%
      filter(
        `Categorização Atributos` != -1,
        `Categorização Consequências` != -1,
        `Categorização Valores` != -1,
        !is.na(`Categorização Atributos`),
        !is.na(`Categorização Consequências`),
        !is.na(`Categorização Valores`)
      )

    if (nrow(dados) == 0) {
      return(NULL)
    }

    # Fabrica Categoria A | C | V
    atr <- paste0(as.character(dados$`Categorização Atributos`), " (A)")
    cons <- paste0(as.character(dados$`Categorização Consequências`), " (C)")
    val <- paste0(as.character(dados$`Categorização Valores`), " (V)")

    niveis <- c(sort(unique(atr)), sort(unique(cons)), sort(unique(val)))
    A <- factor(atr, levels = niveis)
    C <- factor(cons, levels = niveis)
    V <- factor(val, levels = niveis)

    # Cruza Contingência
    links_ac <- as.data.frame(as.table(table(A, C))) %>%
      rename(from = A, to = C, weight = Freq)
    links_cv <- as.data.frame(as.table(table(C, V))) %>%
      rename(from = C, to = V, weight = Freq)

    # Unifica e aplica barra de Filter do Slider
    todas_conexoes <- bind_rows(links_ac, links_cv) %>%
      filter(weight > input$cutoff)

    # Encaminha à rotina robusta de limpeza purificadora dos Galhos Órfãos
    rede_plena <- filtrar_laddering(todas_conexoes)

    if (nrow(rede_plena) == 0) {
      return(NULL)
    }
    return(rede_plena)
  })

  # 2.4 Renderiza Mapa Interativo pela Função do 'utils'
  output$rede_laddering <- renderVisNetwork({
    final_data <- dados_processados()
    # Intercepta visualmente pra avisar se o filtro matou o grafo inteiro
    validate(need(
      !is.null(final_data),
      "Abaque... Nenhuma cadeia A -> C -> V conseguiu sobreviver ao rigor das barras arrastadas! Reduza os filtros."
    ))

    plotar_mapa_hierarquico(
      conexoes = final_data,
      levelSeparation = input$levelSep,
      nodeSpacing = input$nodeSpace,
      multTamanhoMultiplicador = input$sizeMult
    )
  })

  # 2.5 Tabula DataTable de Métricas
  output$tabela_metricas <- renderDT(
    {
      final_data <- dados_processados()
      req(!is.null(final_data))

      calcular_metricas_grafo(final_data)
    },
    options = list(pageLength = 20, dom = 'ftip', autoWidth = TRUE),
    selection = "single"
  )
}

shinyApp(ui, server)
