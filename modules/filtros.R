# ===================================================================================
# OBJETIVO PRINCIPAL: MÓDULO DE FILTROS
# ===================================================================================
#
# Este arquivo encapsula toda a funcionalidade relacionada aos filtros da aplicação.
# Ele contém duas partes principais:
#
# 1. `filtrosUI`: Uma função que cria os componentes visuais dos filtros (os menus
#    dropdown `selectInput` e o botão `actionButton`). Esta função é chamada
#    no `ui.R` para posicionar os filtros na página.
#
# 2. `filtrosServer`: Uma função que contém a lógica do servidor para os filtros.
#    - Implementa "filtros em cascata", onde as opções de um filtro são atualizadas
#      com base nas seleções dos outros.
#    - Implementa a lógica para o botão "Limpar Todos" e para os botões de reset
#      individuais de cada filtro.
#    - **Retorna** o dataframe filtrado como um objeto reativo.
#
# ===================================================================================

### --- UI do Módulo de Filtros (COM ZONA) --- ###
filtrosUI <- function(id, opcoes_distrito, opcoes_zona, opcoes_lote, opcoes_nome, opcoes_modalidade, opcoes_concedente) {
  ns <- NS(id)
  
  tagList(
    # Este div wrapper é necessário para que a lógica flexbox de alinhar ao final funcione
    div(style = "display: flex; flex-direction: column; height: 100%;", 
        
        # Conteúdo principal dos filtros
        div(
          h3("Selecione os filtros"),
          
          # --- Filtro de Zona com Reset ---
          div(class = "filtro-container",
              div(class = "filtro-header",
                  tags$label("Zona", `for` = ns("filtro_zona")),
                  actionButton(ns("reset_zona"), label = NULL, icon = icon("undo"), class = "btn-reset")
              ),
              selectInput(
                inputId = ns("filtro_zona"),
                label = NULL,
                choices = opcoes_zona,
                selected = "Todos"
              )
          ),
          
          # --- Filtro de Distrito com Reset ---
          div(class = "filtro-container",
              div(class = "filtro-header",
                  tags$label("Distritos", `for` = ns("filtro_distrito")),
                  actionButton(ns("reset_distrito"), label = NULL, icon = icon("undo"), class = "btn-reset")
              ),
              selectInput(
                inputId = ns("filtro_distrito"),
                label = NULL,
                choices = opcoes_distrito,
                selected = "Todos"
              )
          ),
          
          # --- Filtro de Lote com Reset ---
          div(class = "filtro-container",
              div(class = "filtro-header",
                  tags$label("Lote", `for` = ns("filtro_lote")),
                  actionButton(ns("reset_lote"), label = NULL, icon = icon("undo"), class = "btn-reset")
              ),
              selectInput(
                inputId = ns("filtro_lote"),
                label = NULL, 
                choices = opcoes_lote,
                selected = "Todos"
              )
          ),
          
          # --- Filtro de Parceria com Reset ---
          div(class = "filtro-container",
              div(class = "filtro-header",
                  tags$label("Parceria", `for` = ns("filtro_nome")),
                  actionButton(ns("reset_nome"), label = NULL, icon = icon("undo"), class = "btn-reset")
              ),
              selectInput(
                inputId = ns("filtro_nome"),
                label = NULL,
                choices = opcoes_nome,
                selected = "Todos"
              )
          ),
          
          # --- Filtro de Modalidade com Reset ---
          div(class = "filtro-container",
              div(class = "filtro-header",
                  tags$label("Modalidade", `for` = ns("filtro_modalidade")),
                  actionButton(ns("reset_modalidade"), label = NULL, icon = icon("undo"), class = "btn-reset")
              ),
              selectInput(
                inputId = ns("filtro_modalidade"),
                label = NULL,
                choices = opcoes_modalidade,
                selected = "Todos"
              )
          ),
          
          # --- Filtro de Poder Concedente com Reset ---
          div(class = "filtro-container",
              div(class = "filtro-header",
                  tags$label("Poder Concedente", `for` = ns("filtro_concedente")),
                  actionButton(ns("reset_concedente"), label = NULL, icon = icon("undo"), class = "btn-reset")
              ),
              selectInput(
                inputId = ns("filtro_concedente"),
                label = NULL,
                choices = opcoes_concedente,
                selected = "Todos"
              )
          ),
          
          # Botão principal de Limpar Filtros
          div(style = "margin-top: 20px;",
              actionButton(
                inputId = ns("reset_filtros"),
                label = "Limpar Todos os Filtros",
                icon = icon("trash-alt"),
                width = "100%"
              )
          )
        ),
        
        # Logos no final da sidebar
        div(class = "sidebar-logos",
            tags$img(src = "imgs/logo_pref.png", class = "sidebar-logo"),
            tags$img(src = "imgs/logo_spp.png", class = "sidebar-logo")
        )
    )
  )
}


### --- Server do Módulo de Filtros (COM ZONA) --- ###
filtrosServer <- function(id, dados_brutos) {
  moduleServer(id, function(input, output, session) {
    
    # --- LÓGICA DOS FILTROS EM CASCATA ---
    
    # OBSERVADOR PARA ATUALIZAR AS OPÇÕES DE ZONA
    observeEvent(c(input$filtro_distrito, input$filtro_modalidade, input$filtro_concedente, input$filtro_nome, input$filtro_lote), {
      dados_intermediarios <- dados_brutos
      if (input$filtro_distrito != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(NM_DIST == input$filtro_distrito) }
      if (input$filtro_modalidade != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_modali == input$filtro_modalidade) }
      if (input$filtro_concedente != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_conced == input$filtro_concedente) }
      if (input$filtro_nome != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_nome == input$filtro_nome) }
      if (input$filtro_lote != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(lote == input$filtro_lote) }
      novas_opcoes <- c("Todos", sort(unique(na.omit(dados_intermediarios$zona))))
      selecao_atual <- if (input$filtro_zona %in% novas_opcoes) input$filtro_zona else "Todos"
      updateSelectInput(session, "filtro_zona", choices = novas_opcoes, selected = selecao_atual)
    }, ignoreInit = TRUE)
    
    # OBSERVADOR PARA ATUALIZAR AS OPÇÕES DE DISTRITO
    observeEvent(c(input$filtro_zona, input$filtro_modalidade, input$filtro_concedente, input$filtro_nome, input$filtro_lote), {
      dados_intermediarios <- dados_brutos
      if (input$filtro_zona != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(zona == input$filtro_zona) }
      if (input$filtro_modalidade != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_modali == input$filtro_modalidade) }
      if (input$filtro_concedente != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_conced == input$filtro_concedente) }
      if (input$filtro_nome != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_nome == input$filtro_nome) }
      if (input$filtro_lote != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(lote == input$filtro_lote) }
      novas_opcoes <- c("Todos", sort(unique(na.omit(dados_intermediarios$NM_DIST))))
      selecao_atual <- if (input$filtro_distrito %in% novas_opcoes) input$filtro_distrito else "Todos"
      updateSelectInput(session, "filtro_distrito", choices = novas_opcoes, selected = selecao_atual)
    }, ignoreInit = TRUE)
    
    # OBSERVADOR PARA ATUALIZAR AS OPÇÕES DE MODALIDADE
    observeEvent(c(input$filtro_zona, input$filtro_distrito, input$filtro_concedente, input$filtro_nome, input$filtro_lote), {
      dados_intermediarios <- dados_brutos
      if (input$filtro_zona != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(zona == input$filtro_zona) }
      if (input$filtro_distrito != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(NM_DIST == input$filtro_distrito) }
      if (input$filtro_concedente != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_conced == input$filtro_concedente) }
      if (input$filtro_nome != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_nome == input$filtro_nome) }
      if (input$filtro_lote != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(lote == input$filtro_lote) }
      novas_opcoes <- c("Todos", sort(unique(dados_intermediarios$ppp_modali)))
      selecao_atual <- if (input$filtro_modalidade %in% novas_opcoes) input$filtro_modalidade else "Todos"
      updateSelectInput(session, "filtro_modalidade", choices = novas_opcoes, selected = selecao_atual)
    }, ignoreInit = TRUE)
    
    # OBSERVADOR PARA ATUALIZAR AS OPÇÕES DE PODER CONCEDENTE
    observeEvent(c(input$filtro_zona, input$filtro_distrito, input$filtro_modalidade, input$filtro_nome, input$filtro_lote), {
      dados_intermediarios <- dados_brutos
      if (input$filtro_zona != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(zona == input$filtro_zona) }
      if (input$filtro_distrito != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(NM_DIST == input$filtro_distrito) }
      if (input$filtro_modalidade != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_modali == input$filtro_modalidade) }
      if (input$filtro_nome != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_nome == input$filtro_nome) }
      if (input$filtro_lote != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(lote == input$filtro_lote) }
      novas_opcoes <- c("Todos", sort(unique(dados_intermediarios$ppp_conced)))
      selecao_atual <- if (input$filtro_concedente %in% novas_opcoes) input$filtro_concedente else "Todos"
      updateSelectInput(session, "filtro_concedente", choices = novas_opcoes, selected = selecao_atual)
    }, ignoreInit = TRUE)
    
    # OBSERVADOR PARA ATUALIZAR AS OPÇÕES DE NOME DA PARCERIA
    observeEvent(c(input$filtro_zona, input$filtro_distrito, input$filtro_modalidade, input$filtro_concedente, input$filtro_lote), {
      dados_intermediarios <- dados_brutos
      if (input$filtro_zona != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(zona == input$filtro_zona) }
      if (input$filtro_distrito != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(NM_DIST == input$filtro_distrito) }
      if (input$filtro_modalidade != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_modali == input$filtro_modalidade) }
      if (input$filtro_concedente != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_conced == input$filtro_concedente) }
      if (input$filtro_lote != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(lote == input$filtro_lote) }
      novas_opcoes <- c("Todos", sort(unique(dados_intermediarios$ppp_nome)))
      selecao_atual <- if (input$filtro_nome %in% novas_opcoes) input$filtro_nome else "Todos"
      updateSelectInput(session, "filtro_nome", choices = novas_opcoes, selected = selecao_atual)
    }, ignoreInit = TRUE)
    
    # OBSERVADOR PARA ATUALIZAR AS OPÇÕES DE LOTE
    observeEvent(c(input$filtro_zona, input$filtro_distrito, input$filtro_modalidade, input$filtro_concedente, input$filtro_nome), {
      dados_intermediarios <- dados_brutos
      if (input$filtro_zona != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(zona == input$filtro_zona) }
      if (input$filtro_distrito != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(NM_DIST == input$filtro_distrito) }
      if (input$filtro_modalidade != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_modali == input$filtro_modalidade) }
      if (input$filtro_concedente != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_conced == input$filtro_concedente) }
      if (input$filtro_nome != "Todos") { dados_intermediarios <- dados_intermediarios %>% filter(ppp_nome == input$filtro_nome) }
      novas_opcoes <- c("Todos", sort(unique(na.omit(dados_intermediarios$lote))))
      selecao_atual <- if (input$filtro_lote %in% novas_opcoes) input$filtro_lote else "Todos"
      updateSelectInput(session, "filtro_lote", choices = novas_opcoes, selected = selecao_atual)
    }, ignoreInit = TRUE)
    
    # --- LÓGICA PARA OS BOTÕES DE RESET ---
    
    # Lógica para o botão de reset GERAL
    observeEvent(input$reset_filtros, {
      updateSelectInput(session, "filtro_zona",       selected = "Todos")
      updateSelectInput(session, "filtro_distrito",   selected = "Todos")
      updateSelectInput(session, "filtro_lote",       selected = "Todos")
      updateSelectInput(session, "filtro_modalidade", selected = "Todos")
      updateSelectInput(session, "filtro_concedente", selected = "Todos")
      updateSelectInput(session, "filtro_nome",       selected = "Todos")
    })
    
    # Lógica para os botões de reset INDIVIDUAIS
    observeEvent(input$reset_zona, { updateSelectInput(session, "filtro_zona", selected = "Todos") })
    observeEvent(input$reset_distrito, { updateSelectInput(session, "filtro_distrito", selected = "Todos") })
    observeEvent(input$reset_lote, { updateSelectInput(session, "filtro_lote", selected = "Todos") })
    observeEvent(input$reset_nome, { updateSelectInput(session, "filtro_nome", selected = "Todos") })
    observeEvent(input$reset_modalidade, { updateSelectInput(session, "filtro_modalidade", selected = "Todos") })
    observeEvent(input$reset_concedente, { updateSelectInput(session, "filtro_concedente", selected = "Todos") })
    
    # --- DATAFRAME REATIVO FINAL ---
    dados_filtrados_reativos <- reactive({
      dados_filtrados <- dados_brutos
      if (input$filtro_zona != "Todos") {
        dados_filtrados <- dados_filtrados |> filter(zona == input$filtro_zona)
      }
      if (input$filtro_distrito != "Todos") {
        dados_filtrados <- dados_filtrados |> filter(NM_DIST == input$filtro_distrito)
      }
      if (input$filtro_lote != "Todos") {
        dados_filtrados <- dados_filtrados |> filter(lote == input$filtro_lote)
      }
      if (input$filtro_modalidade != "Todos") {
        dados_filtrados <- dados_filtrados |> filter(ppp_modali == input$filtro_modalidade)
      }
      if (input$filtro_concedente != "Todos") {
        dados_filtrados <- dados_filtrados |> filter(ppp_conced == input$filtro_concedente)
      }
      if (input$filtro_nome != "Todos") {
        dados_filtrados <- dados_filtrados |> filter(ppp_nome == input$filtro_nome)
      }
      req(nrow(dados_filtrados) > 0)
      
      return(dados_filtrados)
    })
    
    return(dados_filtrados_reativos)
  })
}