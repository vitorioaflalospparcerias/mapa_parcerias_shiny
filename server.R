# ===================================================================================
# OBJETIVO PRINCIPAL: LÓGICA DO SERVIDOR (BACK-END)
# ===================================================================================
#
# Este arquivo é o "cérebro" da aplicação. Ele contém toda a lógica reativa que
# responde às interações do usuário. Suas responsabilidades são:
# 1. Carregar as bibliotecas e os módulos necessários.
# 2. Carregar os dados processados (`.rds`) que serão manipulados.
# 3. Definir a função `server`, que orquestra a comunicação entre os módulos.
#
# FLUXO DE DADOS REATIVO:
# - `filtrosServer` retorna um dataframe REATIVO (`projetos_filtrados`).
#
# - `mapaServer` e `graficosServer` recebem o `projetos_filtrados` como entrada.
#
# - Lógicas adicionais capturam seleções específicas dos filtros (distrito e parceria)
#   e as passam para o módulo do mapa para acionar funcionalidades de zoom.
#
# ===================================================================================

# --- CARREGAMENTO DE BIBLIOTECAS E MÓDULOS ---
library(shiny)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(plotly)

# Carrega a lógica do servidor de cada módulo.
source("modules/mapa.R")
source("modules/filtros.R")
source("modules/graficos.R")

# --- CARREGAMENTO DOS DADOS PRINCIPAIS ---
# Carrega os dataframes que foram preparados pelo script `análises.R`.
projetos <- readRDS("DADOS/projetos.rds")
distritos <- readRDS("DADOS/distritos_processados.rds")

# --- DEFINIÇÃO DA LÓGICA DO SERVIDOR ---
server <- function(input, output, session) {
  
  # --- 1. MÓDULO DE FILTROS ---
  # Chama o server do módulo de filtros.
  # A função retorna um dataframe REATIVO, que é armazenado em `projetos_filtrados`.
  projetos_filtrados <- filtrosServer(id = "filtros_app", dados_brutos = projetos)
  
  # --- 2. MÓDULO DO MAPA ---
  # Cria expressões reativas para obter valores específicos dos filtros.
  # `req()` garante que o código só continue se o input não for nulo.
  
  # Captura o distrito selecionado (reativo) para a lógica de zoom.
  distrito_selecionado_reativo <- reactive({
    req(input[["filtros_app-filtro_distrito"]])
    return(input[["filtros_app-filtro_distrito"]])
  })
  
  # Captura a parceria selecionada (reativo) para a lógica de zoom.
  projeto_selecionado_reativo <- reactive({
    req(input[["filtros_app-filtro_nome"]])
    return(input[["filtros_app-filtro_nome"]])
  })
  
  # Chama o server do módulo do mapa, passando os dados necessários.
  mapaServer(
    id = "mapa",
    # Passa o dataframe REATIVO. O mapa irá observar mudanças neste objeto.
    projetos_reativos = projetos_filtrados,
    # Passa os dados estáticos dos polígonos dos distritos.
    distritos_estaticos = distritos,
    # Passa o distrito selecionado (reativo) para a lógica de zoom.
    distrito_selecionado = distrito_selecionado_reativo,
    # Passa a parceria selecionada (reativo) para a lógica de zoom.
    projeto_selecionado = projeto_selecionado_reativo
  )
  
  # --- 3. MÓDULO DE GRÁFICOS ---
  # Chama o server do módulo de gráficos.
  # Passa o mesmo dataframe REATIVO. Os gráficos e KPIs também reagirão.
  graficosServer(id = "graficos_main", dados_filtrados = projetos_filtrados)
  
}