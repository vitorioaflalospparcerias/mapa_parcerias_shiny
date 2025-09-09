# --- Script para Publicar o Aplicativo ---

# 1. Carregar a biblioteca necessária
library(rsconnect)
rsconnect::setAccountInfo(name='saopaulo-parcerias', token='56EE1707084C7F0979772937C8F07318', secret='BM316BYtkhoNLClscYbassBoJAbUFLICD1LaPX4E')

# 3. Listar TODOS os arquivos e pastas que o aplicativo precisa
#    Esta é a parte mais importante!
app_files <- c(
  "app.R",
  "ui.R",
  "server.R",
  "modules/filtros.R",
  "modules/graficos.R",
  "modules/mapa.R",
  "DADOS/projetos.rds",              # Essencial para os dados
  "DADOS/distritos_processados.rds", # Essencial para os dados
  "www/imgs/logo_pref.png",          # ESSENCIAL para as imagens
  "www/imgs/logo_spp.png"            # ESSENCIAL para as imagens
)

# 4. Publicar o aplicativo, usando a nossa lista de arquivos
rsconnect::deployApp(
  appDir = getwd(),                 # Diretorio do app (o atual)
  appFiles = app_files,             # Força a inclusão dos nossos arquivos!
  appName = "parcerias-sp", # Escolha um nome único para seu app na URL
  account = "saopaulo-parcerias" # Seu nome de usuário do shinyapps.io
)

print("Publicação enviada!")