# ====================================================================================
# OBJETIVO PRINCIPAL: PONTO DE ENTRADA DA APLICAÇÃO SHINY
# ====================================================================================
#
# Este é o arquivo principal que inicia a aplicação Shiny. Sua responsabilidade é:
# 1. Carregar a biblioteca `shiny`.
# 2. Carregar o código da Interface do Usuário (UI) a partir do arquivo `ui.R`.
# 3. Carregar o código da lógica do Servidor (Server) a partir do arquivo `server.R`.
# 4. Chamar a função `shinyApp` para combinar a UI e o Server e executar a aplicação.
#
# Ele funciona como o "maestro" que junta as duas partes essenciais da aplicação.
#
# ====================================================================================
#shiny::addResourcePath(prefix = "imagens", directoryPath = file.path(getwd(), "www", "imgs"))

# Carrega a biblioteca principal do Shiny.
library(shiny)

# Carrega o código que define a interface do usuário (aparência do app).
# O objeto `ui` será criado a partir deste arquivo.
source("ui.R")

# Carrega o código que define a lógica do servidor (como o app reage e processa dados).
# O objeto `server` será criado a partir deste arquivo.
source("server.R")

# Executa a aplicação Shiny combinando a UI e o Server.
shinyApp(ui = ui, server = server)