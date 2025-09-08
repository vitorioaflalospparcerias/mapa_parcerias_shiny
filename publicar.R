# --- Script para Publicar o Aplicativo ---

# 1. Carregar a biblioteca necessária
library(rsconnect)
rsconnect::setAccountInfo(name='saopaulo-parcerias', token='F489E4B2DE045534558CB22808FD3099', secret='61XRWBp+bLltugJF9W213zO5On8hZG9SGiyM7NsR')

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
  appName = "mapa-de-parcerias-sp", # Escolha um nome único para seu app na URL
  account = "saopaulo-parcerias" # Seu nome de usuário do shinyapps.io
)

print("Publicação enviada!")