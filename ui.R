# ===================================================================================
# OBJETIVO PRINCIPAL: DEFINIÇÃO DA INTERFACE DO USUÁRIO (FRONT-END)
# ===================================================================================
#
# Este arquivo constrói toda a estrutura visual e o layout da aplicação Shiny.
# Ele é o "esqueleto" da página que o usuário vê no navegador.
#
# RESPONSABILIDADES:
# 1. Carregar as bibliotecas R necessárias para construir a UI.
# 2. Carregar os arquivos de UI dos módulos (`filtrosUI`, `mapaUI`, etc.).
# 3. Ler os dados pré-processados (`.rds`) para popular as opções iniciais dos filtros.
# 4. Definir a estrutura principal da página usando `fluidPage`.
# 5. Injetar um bloco de código CSS customizado para controlar a aparência, cores,
#    fontes e o layout preciso de cada componente.
# 6. Montar o layout final, posicionando o título, a barra de filtros, o mapa e a
#    área de gráficos e KPIs em suas respectivas colunas.
#
# ===================================================================================


# --- 1. CARREGAMENTO DE BIBLIOTECAS E MÓDULOS ---

# Carrega a biblioteca principal do Shiny para as funções de UI.
library(shiny)
# Carrega a biblioteca dplyr, embora seja mais usada no server, pode ser útil para alguma manipulação rápida.
library(dplyr)
# Carrega a biblioteca tidyr para manipulação de dados.
library(tidyr)
# Carrega a biblioteca plotly para renderizar os gráficos interativos.
library(plotly)

# `source` lê e executa o código de outros arquivos R.
# Aqui, estamos carregando as funções que definem as UIs de cada módulo.
source("modules/mapa.R")      # Carrega a função mapaUI()
source("modules/filtros.R")   # Carrega a função filtrosUI()
source("modules/graficos.R") # Carrega a função graficosUI()


# --- 2. CARREGAMENTO DE DADOS PARA OPÇÕES DE FILTRO ---

# `readRDS` lê um objeto R salvo em um arquivo. É muito mais rápido que ler um .csv ou .xlsx.
# Carrega o dataframe de projetos, que contém as informações de cada parceria.
projetos <- readRDS("DADOS/projetos.rds")
# Carrega os dados espaciais (polígonos) de todos os distritos.
distritos <- readRDS("DADOS/distritos_processados.rds")


# --- 3. GERAÇÃO DAS OPÇÕES INICIAIS PARA OS FILTROS ---

# Cria os vetores de texto que serão usados para preencher as caixas de seleção (selectInput).
# A opção "Todos" é adicionada no início de cada vetor para permitir a visualização sem filtros.
opcoes_modalidade <- c("Todos", unique(projetos$ppp_modali))
opcoes_concedente <- c("Todos", unique(projetos$ppp_conced))
opcoes_nome <- c("Todos", sort(unique(projetos$ppp_nome)))

# ADICIONADO PARA O FILTRO DE LOTE
# Usamos na.omit para remover projetos sem lote e sort para ordenar
lotes_com_projetos <- sort(unique(na.omit(projetos$lote)))
opcoes_lote <- c("Todos", lotes_com_projetos)

# Para os distritos, um tratamento especial:
# 1. `na.omit` remove qualquer projeto que não tenha um distrito associado.
# 2. `unique` pega apenas os nomes de distritos únicos.
# 3. `sort` ordena os nomes em ordem alfabética.
distritos_com_projetos <- sort(unique(na.omit(projetos$NM_DIST)))
opcao_distritos <- c("Todos", distritos_com_projetos)


# --- 4. DEFINIÇÃO DA UI PRINCIPAL ---

# `fluidPage` cria uma página web fluida, que se adapta ao tamanho da tela do navegador.
ui <- fluidPage(
  # `fluid = TRUE` faz com que o layout ocupe 100% da largura da tela.
  fluid = TRUE,
  
  # `tags$head` permite adicionar elementos ao <head> do HTML, como CSS e JavaScript.
  tags$head(
    # `tags$style(HTML(...))` é a forma de injetar um bloco de CSS diretamente na página.
    tags$style(HTML("
      /* ====================================================== */
      /* ESTILOS GERAIS E ESTRUTURA                             */
      /* ====================================================== */
      
      /* Define a altura total e remove margens/preenchimentos padrão do navegador. */
      html, body {height: 100%; margin: 0; padding: 0; overflow: hidden; background-color: #f5f5ff;}
      
      /* Define o container principal da página para usar Flexbox, organizando os elementos em coluna. */
      .container-fluid {height: 100%; display: flex; flex-direction: column; padding: 0;}
      
      /* O wrapper das 3 colunas principais. flex-grow faz ele ocupar todo o espaço vertical disponível. */
      .main-content-wrapper {flex-grow: 1; display: flex; height: 0; padding: 10px; gap: 10px;}

      /* Estilo genérico para as colunas principais (filtros, mapa, gráficos) para garantir altura total. */
      .main-content-wrapper > [class*='col-sm-'] { height: 100%; padding: 0; box-sizing: border-box; }
      
      
      /* ====================================================== */
      /* PAINEL DE FILTROS (ESQUERDA)                           */
      /* ====================================================== */
      .sidebar-panel { 
        height: 100%; 
        box-sizing: border-box; 
        background-color: #FFF5E1; /* Fundo amarelo claro */
        border: 1px solid #FADCB3;   /* Borda sutil */
        border-radius: 8px;        /* Cantos arredondados */
        padding: 20px; 
        overflow-y: auto;          /* Adiciona barra de rolagem se o conteúdo for maior que a tela */
        display: flex;             /* Adicionado para alinhar os logos no final */
        flex-direction: column;    /* Adicionado para alinhar os logos no final */
      }
      .sidebar-panel h3 {color: #D35400; font-size: 18px; font-weight: 700; margin-top: 0;}

      
      /* ====================================================== */
      /* PAINEL DO MAPA (CENTRO)                                */
      /* ====================================================== */
      .map-panel { height: 100%; border-radius: 8px; overflow: hidden; }
      .map-panel .leaflet { height: 100% !important; } /* !important força o mapa a ocupar 100% da altura do painel. */
      
      
      /* ====================================================== */
      /* PAINEL DE CONTEÚDO (DIREITA)                           */
      /* ====================================================== */
      .content-panel { 
        height: 100%; 
        background-color: #f8f9fa; /* Fundo cinza claro */
        border-radius: 8px; 
        padding: 10px; 
        overflow-y: auto; 
        box-sizing: border-box; 
        display: flex; 
        flex-direction: column; 
        gap: 0; /* Remove o gap para controlar o espaçamento com margin nos filhos */
      }
      
      /* --- ESTILOS DOS KPIs --- */
      .kpi-row { display: flex; gap: 10px; margin-bottom: 10px; }
      .kpi-row > div { flex: 1; }
      .kpi-box {
          background-color: #FFF5E1; /* Mesma cor da sidebar */
          border: 1px solid #FADCB3; 
          padding: 10px;
          border-radius: 5px; 
          text-align: center; 
          height: auto;
      }
      .kpi-box h5 {
          font-size: 13px; 
          color: #023047; /* Mesma cor das barras do gráfico */
          margin-top: 0; 
          margin-bottom: 5px; 
          text-transform: uppercase; 
          font-weight: bold;
      }
      .kpi-box p {
          font-size: 15px; 
          font-weight: 700; 
          color: #D35400; /* Laranja escuro para o valor principal */
          margin-bottom: 0;
      }
      
      /* --- ESTILOS DOS GRÁFICOS --- */
      .plotly.html-widget { height: 100% !important; min-height: 280px !important; }

      .chart-container {
        background-color: #FFF5E1; /* Mesma cor da sidebar */
        border: 1px solid #FADCB3; 
        border-radius: 8px;
        padding: 10px;
        margin-bottom: 10px; /* Espaço entre os gráficos */
        height: 320px; 
        display: flex;
        flex-direction: column;
      }

      /* --- ESTILOS PARA FILTROS INDIVIDUAIS COM RESET --- */
      .filtro-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px; }
      .filtro-header label { margin-bottom: 0; }
      .btn-reset { background: none; border: none; padding: 0 5px; color: #888; font-size: 12px; cursor: pointer; }
      .btn-reset:hover { color: #333; }
      .filtro-container { margin-bottom: 15px; }

      /* --- ESTILOS PARA O TÍTULO CUSTOMIZADO --- */
      .custom-title-panel {
        padding: 10px 20px;
        border-bottom: 1px solid #ddd;
        background-color: white;
        flex-shrink: 0; /* Impede que o header encolha */
      }
      .title-wrapper {
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;
      }
      .title-left, .title-right {
        flex: 1; /* Ocupa espaço igual nas laterais */
        display: flex;
        align-items: center;
      }
      .title-right { justify-content: flex-end; }
      .title-center { flex: 2; text-align: center; }
      .title-center h2 { margin: 0; font-size: 24px; }
      .title-left img { margin-right: 15px; }

      /* ====================================================== */
      /* NOVO ESTILO: Logos no final da Sidebar                 */
      /* ====================================================== */
      .sidebar-logos {
        display: flex;
        justify-content: space-around;
        align-items: center;
        width: 100%;
        margin-top: auto; /* Empurra os logos para o final */
        padding-top: 15px;
        border-top: 1px solid #FADCB3;
        box-sizing: border-box;
      }
      .sidebar-logo {
        max-width: 48%;
        height: auto;
        display: block;
      }
    "))
  ),
  
  # --- 5. CABEÇALHO CUSTOMIZADO ---
  div(class = "custom-title-panel",
      div(class = "title-wrapper",
          div(class = "title-left",
              tags$img(src = "imagens/logo_pref.png", height = "50px"),
              tags$img(src = "imagens/logo_spp.png", height = "50px")
          ),
          div(class = "title-center",
              tags$h1(tags$strong("Mapa de Parcerias"))
          ),
          div(class = "title-right")
      )
  ),
  
  # --- 6. LAYOUT PRINCIPAL DAS COLUNAS ---
  div(class = "main-content-wrapper",
      # Coluna 1: Barra Lateral (Filtros)
      div(class = "col-sm-2",
          div(class = "sidebar-panel",
              filtrosUI(
                id = "filtros_app",
                opcoes_distrito = opcao_distritos,
                opcoes_lote = opcoes_lote,
                opcoes_nome = opcoes_nome,
                opcoes_modalidade = opcoes_modalidade,
                opcoes_concedente = opcoes_concedente
              )
          )
      ),
      # Coluna 2: Mapa
      div(class = "col-sm-4",
          div(class = "map-panel",
              mapaUI("mapa")
          )
      ),
      # Coluna 3: Gráficos e KPIs
      div(class = "col-sm-6",
          div(class = "content-panel",
              graficosUI("graficos_main")
          )
      )
  )
)