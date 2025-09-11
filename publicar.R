# --- Script para Publicar o Aplicativo ---

# 1. Carregar a biblioteca necessária
library(rsconnect)
rsconnect::setAccountInfo(name='sp-parcerias', token='C070CC0EFF007E8998C67B6E17CB4DAF', secret='l2LQQh/Lx97Jze9WJQbHRusZi5nReqT1BglRB6TC')

# 3. Listar TODOS os arquivos e pastas que o aplicativo precisa
#    Esta é a parte mais importante!
# 2. Publicar o aplicativo (MÉTODO AUTOMÁTICO)
rsconnect::deployApp(
  appDir = getwd(),
  appName = "sp-mapa-parcerias",
  account = "sp-parcerias",
  forceUpdate = TRUE # Adicionar este argumento é uma boa prática para forçar a atualização
)

print("Publicação enviada!")
