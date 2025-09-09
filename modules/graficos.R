# ===================================================================================
# OBJETIVO PRINCIPAL: MÓDULO DE GRÁFICOS E KPIs
# ===================================================================================
#
# Este arquivo encapsula a lógica e a UI para a seção de indicadores
# (Key Performance Indicators) e os gráficos da aplicação.
#
# 1. graficosUI: Define os contêineres onde os KPIs e os gráficos (plotlyOutput)
#    serão renderizados. A estrutura (kpi-row, chart-container) é controlada
#    pelo CSS definido no ui.R.
#
# 2. graficosServer: Contém a lógica reativa para:
#    - Calcular e renderizar os KPIs (Maior Área, Distrito com Mais Projetos, etc.).
#    - Gerar os três gráficos de barras interativos (Modalidade, Poder Concedente,
#      Distrito) usando ggplot2 e plotly.
#    - Recebe o dataframe `dados_filtrados` do servidor principal e reage a
#      qualquer mudança nele, atualizando todos os outputs.
#
# ===================================================================================
# --- CARREGAMENTO DE BIBLIOTECAS ---
library(shiny)
library(dplyr)
library(sf)
library(ggplot2)
library(plotly)
library(stringr)

### --- UI do Módulo de Gráficos (COM GRÁFICO DE ZONA) --- ###
graficosUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "kpi-row",
        uiOutput(ns("kpi_area")),
        uiOutput(ns("kpi_distrito")),
        uiOutput(ns("kpi_concedente"))
    ),
    
    # Gráficos agora dentro de seus containers
    div(class = "chart-container", 
        plotlyOutput(ns("plot_modalidade"), height = "100%")
    ),
    div(class = "chart-container",
        plotlyOutput(ns("plot_concedente"), height = "100%")
    ),
    # O último não precisa de margem abaixo, então removemos a margem via estilo inline
    div(class = "chart-container", style = "margin-bottom: 0;",
        plotlyOutput(ns("plot_zona"), height = "100%")
    )
  )
}

### --- Server do Módulo de Gráficos (COM GRÁFICO DE ZONA) --- ###
graficosServer <- function(id, dados_filtrados) {
  moduleServer(id, function(input, output, session) {
    
    # --- KPIs ---
    output$kpi_area <- renderUI({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      maior_area <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(ppp_area)) |> arrange(desc(ppp_area)) |> slice(1)
      valor <- if (nrow(maior_area) == 0) "N/A" else maior_area$ppp_nome
      div(class = "kpi-box", h5("Maior Área de Concessão"), p(valor))
    })
    
    output$kpi_distrito <- renderUI({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      contagem_distritos <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(NM_DIST)) |> count(NM_DIST, name = "n", sort = TRUE)
      req(nrow(contagem_distritos) > 0)
      max_valor <- contagem_distritos$n[1]
      top_distritos <- contagem_distritos |> filter(n == max_valor)
      valor <- paste(top_distritos$NM_DIST, collapse = ", ")
      titulo <- if(nrow(top_distritos) > 1) "Distritos com Mais Projetos" else "Distrito com Mais Projetos"
      div(class = "kpi-box", h5(titulo), p(valor))
    })
    
    output$kpi_concedente <- renderUI({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      contagem_concedente <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(ppp_conced)) |> count(ppp_conced, name = "n", sort = TRUE)
      req(nrow(contagem_concedente) > 0)
      max_valor <- contagem_concedente$n[1]
      top_concedentes <- contagem_concedente |> filter(n == max_valor)
      valor <- paste(top_concedentes$ppp_conced, collapse = ", ")
      titulo <- if(nrow(top_concedentes) > 1) "Principais Poderes Concedentes" else "Poder Concedente Principal"
      div(class = "kpi-box", h5(titulo), p(valor))
    })
    
    # --- GRÁFICOS ---
    
    output$plot_modalidade <- renderPlotly({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      dados_grafico <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> count(ppp_modali, name = "Quantidade") |> mutate(ppp_modali_wrap = str_wrap(ppp_modali, width = 20))
      
      p <- ggplot(dados_grafico, aes(x = reorder(ppp_modali_wrap, Quantidade), y = Quantidade, text = paste0("Modalidade: ", ppp_modali, "\nQuantidade: ", Quantidade))) +
        geom_col(fill = "#023047", show.legend = FALSE) + coord_flip() + labs(x = NULL, y = NULL) + theme_minimal() + 
        theme(
          # <<< ALTERAÇÃO DE FONTE >>>
          text = element_text(size = 12), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_line(color = "#E0E0E0", linetype = "dotted")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          # <<< ALTERAÇÃO DE FONTE >>>
          title = list(text = "<b>Projetos por Modalidade</b>", y = 0.95, x = 0.5, xanchor = 'center', yanchor = 'top', font = list(size = 16)),
          margin = list(l = 10, r = 10, b = 20, t = 40),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    output$plot_concedente <- renderPlotly({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      dados_grafico <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(ppp_conced)) |> count(ppp_conced, name = "Quantidade") |> top_n(10, Quantidade) |> mutate(ppp_conced_display = str_replace(ppp_conced, "^Secretaria Municipal", "Sec Mun"), ppp_conced_display = str_wrap(ppp_conced_display, width = 20))
      
      p <- ggplot(dados_grafico, aes(x = reorder(ppp_conced_display, Quantidade), y = Quantidade, text = paste0("Concedente: ", ppp_conced, "\nQuantidade: ", Quantidade))) +
        geom_col(fill = "#023047", show.legend = FALSE, width = 0.6) + coord_flip() + labs(x = NULL, y = NULL) + theme_minimal() +
        theme(
          # <<< ALTERAÇÃO DE FONTE >>>
          text = element_text(size = 12), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_line(color = "#E0E0E0", linetype = "dotted")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          # <<< ALTERAÇÃO DE FONTE >>>
          title = list(text = "<b>Top 10 Poderes Concedentes</b>", y = 0.95, x = 0.5, xanchor = 'center', yanchor = 'top', font = list(size = 16)),
          margin = list(l = 10, r = 10, b = 20, t = 40),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
    output$plot_zona <- renderPlotly({
      dados <- dados_filtrados(); req(nrow(dados) > 0)
      dados_grafico <- dados |> st_drop_geometry() |> distinct(ppp_nome, .keep_all = TRUE) |> filter(!is.na(zona)) |> count(zona, name = "Quantidade")
      
      p <- ggplot(dados_grafico, aes(x = reorder(zona, Quantidade), y = Quantidade, text = paste0("Zona: ", zona, "\nQuantidade: ", Quantidade))) +
        geom_col(fill = "#023047", show.legend = FALSE) + coord_flip() + labs(x = NULL, y = NULL) + theme_minimal() +
        theme(
          # <<< ALTERAÇÃO DE FONTE >>>
          text = element_text(size = 12), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_line(color = "#E0E0E0", linetype = "dotted")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          # <<< ALTERAÇÃO DE FONTE >>>
          title = list(text = "<b>Projetos por Zona</b>", y = 0.95, x = 0.5, xanchor = 'center', yanchor = 'top', font = list(size = 16)),
          margin = list(l = 10, r = 10, b = 20, t = 40),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
    
  })
}