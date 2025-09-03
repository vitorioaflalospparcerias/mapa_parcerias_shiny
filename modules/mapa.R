# ===================================================================================
# OBJETIVO PRINCIPAL: MÓDULO DO MAPA
# ===================================================================================
#
# Este módulo é responsável por toda a interatividade do mapa Leaflet.
#
# 1. mapaUI: Simplesmente cria o contêiner `leafletOutput` que ocupará
#    100% do espaço da coluna central.
#
# 2. mapaServer: Contém a lógica principal do mapa.
#    - `renderLeaflet`: Renderiza o mapa base uma única vez, com os polígonos
#      de todos os distritos.
#    - `observe`: Observa o dataframe `projetos_reativos`. Sempre que os filtros
#      mudam, este observador é acionado, limpando os marcadores antigos
#      e adicionando os novos marcadores (pontos dos projetos) usando a função
#      `leafletProxy` para alta performance.
#    - `observeEvent` (para projeto_selecionado): Controla a lógica de zoom
#      quando uma parceria específica é selecionada no filtro. Usa `flyTo` para
#      uma animação suave até o ponto do projeto.
#    - `observeEvent` (para distrito_selecionado): Controla a lógica de zoom
#      quando um distrito é selecionado. Usa `flyToBounds` para ajustar a visão
#      aos limites do polígono do distrito. A lógica dá prioridade ao zoom
#      do projeto.
#
# ===================================================================================


# --- CARREGAMENTO DE BIBLIOTECAS ---
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(htmltools)

# --- UI DO MÓDULO ---
mapaUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("mapa_distritos"), height = "100%", width = "100%")
  )
}

# --- SERVER DO MÓDULO ---
mapaServer <- function(id, projetos_reativos, distritos_estaticos, distrito_selecionado, projeto_selecionado) {
  moduleServer(id, function(input, output, session) {
    
    # RENDERIZA O MAPA BASE
    output$mapa_distritos <- renderLeaflet({
      leaflet() |>
        addTiles() |>
        addPolygons(
          data = distritos_estaticos,
          fillColor = "lightblue",
          color = "black",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.5,
          label = ~NM_DIST,
          layerId = ~NM_DIST
        )
    })
    
    # OBSERVADOR PARA ATUALIZAR OS PROJETOS
    observe({
      dados_para_plotar <- projetos_reativos()
      icon_pin <- awesomeIcons(
        icon = 'info-sign',
        library = 'glyphicon',
        markerColor = 'red',
        iconColor = '#FFFFFF'
      )
      
      leafletProxy("mapa_distritos", data = dados_para_plotar) |>
        clearGroup("projetos") |>
        addAwesomeMarkers(
          data = dados_para_plotar,
          icon = icon_pin,
          label = ~lapply(label_html, HTML),
          labelOptions = labelOptions(direction = "auto", textsize = "13px"),
          group = "projetos"
        )
    })
    
    # OBSERVADOR PARA ZOOM NA PARCERIA (CORRIGIDO)
    observeEvent(projeto_selecionado(), {
      nome_projeto <- projeto_selecionado()
      mapa_proxy <- leafletProxy("mapa_distritos")
      
      if (nome_projeto != "Todos") {
        projeto_zoom <- projetos_reativos() %>%
          filter(ppp_nome == nome_projeto)
        
        if (nrow(projeto_zoom) > 0) {
          coords <- sf::st_coordinates(projeto_zoom)
          mapa_proxy %>%
            flyTo(lng = coords[1], lat = coords[2], zoom = 17)
        }
      } else {
        # Se o filtro de parceria foi limpo, verifica se deve fazer zoom-out
        if (distrito_selecionado() == "Todos") {
          bbox <- st_bbox(distritos_estaticos) %>% as.vector()
          mapa_proxy %>%
            flyToBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4])
        }
      }
    })
    
    # OBSERVADOR PARA O ZOOM NO DISTRITO
    observeEvent(distrito_selecionado(), {
      req(projeto_selecionado() == "Todos")
      
      nome_distrito <- distrito_selecionado()
      mapa_proxy <- leafletProxy("mapa_distritos")
      
      if (nome_distrito == "Todos") {
        bbox <- st_bbox(distritos_estaticos) %>% as.vector()
        mapa_proxy %>%
          flyToBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4])
      } else {
        distrito_zoom <- distritos_estaticos %>%
          filter(NM_DIST == nome_distrito)
        
        if (nrow(distrito_zoom) > 0) {
          bbox <- st_bbox(distrito_zoom) %>% as.vector()
          mapa_proxy %>%
            flyToBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4])
        }
      }
    })
  })
}