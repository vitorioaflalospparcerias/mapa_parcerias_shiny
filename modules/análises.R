# ===================================================================================
# OBJETIVO PRINCIPAL: SCRIPT DE PRÉ-PROCESSAMENTO DE DADOS
# ===================================================================================
#
# Este script NÃO faz parte da aplicação Shiny em si, mas é um passo crucial de
# preparação. Sua responsabilidade é carregar os dados brutos de diferentes fontes
# (shapefiles de distritos, shapefile de concessões do GeoSampa, e uma planilha
# Excel com dados mapeados manualmente), realizar a limpeza, transformação, unificação
# e, finalmente, salvar os dados processados em arquivos .rds.
#
# Os arquivos .rds (`projetos.rds` e `distritos_processados.rds`) são otimizados
# para uma leitura rápida, tornando o carregamento inicial do aplicativo Shiny
# muito mais eficiente.
#
# FLUXO DE EXECUÇÃO:
# 1. Carrega as bibliotecas necessárias para manipulação de dados e geoespacial.
# 2. Carrega e processa o shapefile dos distritos de São Paulo.
# 3. Carrega e processa o shapefile de concessões do GeoSampa.
# 4. Carrega e processa a planilha de concessões mapeadas manualmente.
# 5. TRANSFORMA TODOS OS POLÍGONOS EM PONTOS (CENTROIDES).
# 6. Realiza a junção espacial (spatial join) para associar cada projeto a um distrito.
# 7. Executa uma limpeza final nos dados (padronização de texto, tratamento de NAs).
# 8. Cria uma coluna 'label_html' para os pop-ups do mapa.
# 9. Salva os dataframes finais (`projetos` e `distritos`) em formato .rds.
#
# ===================================================================================

# --- CARREGAMENTO DE BIBLIOTECAS ---
library(shiny)
library(leaflet)
library(sf)      # Para manipulação de dados espaciais (shapefiles)
library(tidyverse) # Coleção de pacotes para manipulação de dados (dplyr, ggplot2, etc.)
library(dplyr)
library(stringr) # Para manipulação de strings
library(readxl)  # Para ler arquivos Excel (.xlsx)
library(htmltools)

# --- ETAPA 1: PROCESSAMENTO DA CAMADA DE DISTRITOS DE SÃO PAULO ---
# Carrega o arquivo shapefile que contém os limites de todos os distritos do estado.
distritos <- st_read("DADOS/distritos_sao_paulo/SP_distritos_CD2022.shp", quiet = TRUE)
# Filtra para manter apenas os distritos pertencentes ao município de "São Paulo".
distritos <- distritos %>%
  filter(NM_MUN == "São Paulo")
# Transforma o sistema de coordenadas (CRS) para WGS 84 (lat/long), padrão usado pelo Leaflet.
distritos <- st_transform(distritos, crs = 4326)


# --- ETAPA 2: PROCESSAMENTO DA CAMADA DE CONCESSÕES DO GEOSAMPA ---
# Carrega o shapefile com os polígonos das parcerias e concessões.
concessoes_geosampa <- st_read("DADOS/parcerias_poligono/SIRGAS_SHP_concessaoparceria.shp", quiet = TRUE)

# Define o CRS original do arquivo (SIRGAS 2000 / UTM zone 23S) e depois o converte para WGS 84.
st_crs(concessoes_geosampa) <- 31983
concessoes_geosampa <- st_transform(concessoes_geosampa, crs = 4326)

# Realiza a limpeza e correção de erros de digitação nos nomes dos projetos e dos concedentes.
concessoes_geosampa <- concessoes_geosampa %>%
  mutate(ppp_nome = str_replace_all(ppp_nome, c(
    "Cemitério da Consolção" = "Cemitério da Consolação",
    "Cemitério Vila Nova Cachoerinha" = "Cemitério Vila Nova Cachoeirinha",
    "Terminal Grajau" = "Terminal Grajaú",
    "Terminal Cachoerinha" = "Terminal Cachoeirinha",
    "Parque Jacintho Albero" = "Parque Jacintho Alberto",
    "Terminal Paralheiros" = "Terminal Parelheiros"
  ))) %>%
  mutate(ppp_conced = str_replace_all(ppp_conced, c(
    "Secetaria Municipal de Educação" = "Secretaria Municipal de Educação"
  ))) %>%
  # Seleciona apenas as colunas relevantes para a análise.
  select(ppp_nome, geometry, ppp_modali, ppp_conced, ppp_area)


# --- ETAPA 3: PROCESSAMENTO DA CAMADA DE CONCESSÕES MAPEADAS MANUALMENTE ---
# Carrega a planilha Excel que contém projetos representados por pontos (latitude/longitude).
concessoes_lat_long <- read_excel("DADOS/parcerias_lat_long.xlsx")

# Converte a coluna de geometria (que está em formato de texto "POINT(lon, lat)") para colunas numéricas de latitude e longitude.
concessoes_lat_long <- concessoes_lat_long %>%
  mutate(
    geometry_clean = str_replace_all(geometry, " ", ""),
    geometry_clean = str_remove_all(geometry_clean, "POINT\\(|\\)"),
    latitude = str_extract(geometry_clean, "^-?\\d+\\.\\d+"),
    longitude = str_extract(geometry_clean, "(?<=,)-?\\d+\\.\\d+")
  ) %>%
  mutate(
    lat_num = as.numeric(latitude),
    lon_num = as.numeric(longitude)
  )

# Converte o dataframe para um objeto espacial do tipo 'sf', usando as coordenadas extraídas.
concessoes_lat_long <- st_as_sf(
  concessoes_lat_long,
  coords = c("lon_num", "lat_num"),
  crs = 4326, # Define o CRS como WGS 84
  remove = FALSE
)

# Lógica para evitar duplicatas: Pega apenas os projetos da planilha que NÃO existem no GeoSampa.
valores_lat_long <- unique(concessoes_lat_long$ppp)
valores_geosampa <- unique(concessoes_geosampa$ppp_nome)
somente_lat_long <- setdiff(valores_lat_long, valores_geosampa) # Encontra os nomes que estão apenas na planilha

# Filtra a camada de pontos para manter apenas os projetos únicos e remove o projeto "Estacionamento Rotativo".
concessoes_lat_long <- concessoes_lat_long %>%
  filter(ppp %in% somente_lat_long,
         ppp != "Estacionamento Rotativo") %>%
  # Seleciona e renomeia as colunas para que fiquem compatíveis com o dataframe do GeoSampa.
  select(ppp, geometry, ppp_modali, ppp_conced, ppp_area) %>%
  rename(ppp_nome = ppp)


# --- ETAPA 4: CONSOLIDAÇÃO DOS DADOS ---
# Une os dois dataframes (polígonos do GeoSampa e pontos da planilha) em um único dataframe.
projetos <- rbind(concessoes_geosampa, concessoes_lat_long)

# Remove duplicatas baseadas no nome do projeto, mantendo a primeira ocorrência.
projetos <- projetos %>%
  distinct(ppp_nome, .keep_all = TRUE)


# --- ETAPA 5: TRANSFORMAÇÃO DE POLÍGONOS EM PONTOS (CENTROIDES) ---
# Identifica quais geometrias são polígonos
indices_poligonos <- which(st_geometry_type(projetos) %in% c("POLYGON", "MULTIPOLYGON"))
# Para essas geometrias, calcula o centroide e substitui a geometria original
if (length(indices_poligonos) > 0) {
  st_geometry(projetos[indices_poligonos, ]) <- st_centroid(st_geometry(projetos[indices_poligonos, ]))
}


# --- ETAPA 6: JUNÇÃO ESPACIAL E LIMPEZA FINAL ---
# Realiza uma junção espacial: para cada projeto (agora todos pontos), descobre em qual polígono de distrito ele está localizado.
projetos <- st_join(projetos, distritos %>% select(NM_DIST))

# Limpeza final dos dados textuais.
projetos <- projetos %>%
  mutate(
    # Padroniza a capitalização e remove espaços em branco extras.
    ppp_conced = str_to_title(trimws(ppp_conced)),
    # Substitui valores NA (ausentes) na modalidade por "Não Especificado".
    ppp_modali = replace_na(ppp_modali, "Não Especificado"),
    # Padroniza a capitalização dos nomes dos distritos.
    NM_DIST = str_to_title(trimws(NM_DIST))
  )

# Cria a coluna 'label_html' que será usada para exibir informações nos pop-ups do mapa.
# `coalesce` é usado para substituir NAs por um texto padrão, evitando erros.
projetos <- projetos |>
  mutate(
    label_html = sprintf(
      "<strong>PPP:</strong> %s<br><strong>Modalidade:</strong> %s<br><strong>Poder Concedente:</strong> %s<br><strong>Área da Concessão (m²):</strong> %s",
      coalesce(ppp_nome, "Não disponível"),
      coalesce(ppp_modali, "Não disponível"),
      coalesce(ppp_conced, "Não disponível"),
      coalesce(as.character(ppp_area), "Não disponível")
    )
  )

# Organiza e seleciona as colunas finais para o dataframe de projetos.
ordem_colunas <- c("ppp_nome", "geometry", "ppp_modali", "ppp_conced", "ppp_area", "NM_DIST", "label_html")
colunas_existentes <- intersect(ordem_colunas, names(projetos))
projetos <- projetos[, colunas_existentes]

# Padroniza também o nome dos distritos no dataframe de distritos para garantir consistência.
distritos <- distritos %>%
  mutate(NM_DIST = str_to_title(trimws(NM_DIST)))

# Filtro de Lote

equipamentos_lotes <- c(
  "Baixo do Viaduto Antártica" = "Baixo Viaduto",
  "Baixo do Viaduto Lapa" = "Baixo Viaduto",
  "Baixo do Viaduto Pompéia" = "Baixo Viaduto",
  "Biblioteca Mário de Andrade" = "Polos Gastronômicos",
  "Campo de Marte" = "Equipamento único",
  "Cemitério da Consolação" = "Cemitérios - Bloco 1",
  "Cemitério da Freguesia do Ó" = "Cemitérios - Bloco 4",
  "Cemitério da Lapa" = "Cemitérios - Bloco 3",
  "Cemitério da Penha" = "Cemitérios - Bloco 4",
  "Cemitério da Quarta Parada" = "Cemitérios - Bloco 1",
  "Cemitério da Saudade" = "Cemitérios - Bloco 3",
  "Cemitério da Vila Mariana" = "Cemitérios - Bloco 1",
  "Cemitério de Itaquera" = "Cemitérios - Bloco 4",
  "Cemitério de Parelheiros" = "Cemitérios - Bloco 3",
  "Cemitério de Santana" = "Cemitérios - Bloco 1",
  "Cemitério de Santo Amaro" = "Cemitérios - Bloco 2",
  "Cemitério de Tremembé" = "Cemitérios - Bloco 1",
  "Cemitério do Araçá" = "Cemitérios - Bloco 2",
  "Cemitério do Campo Grande" = "Cemitérios - Bloco 3",
  "Cemitério do Lageado" = "Cemitérios - Bloco 3",
  "Cemitério Dom Bosco" = "Cemitérios - Bloco 2",
  "Cemitério São Luis" = "Cemitérios - Bloco 4",
  "Cemitério São Paulo" = "Cemitérios - Bloco 2",
  "Cemitério São Pedro" = "Cemitérios - Bloco 4",
  "Cemitério Vila Formosa I" = "Cemitérios - Bloco 1",
  "Cemitério Vila Formosa II" = "Cemitérios - Bloco 1",
  "Cemitério Vila Nova Cachoeirinha" = "Cemitérios - Bloco 2",
  "Centro Cultural da Penha" = "Polos Gastronômicos",
  "Centro Cultural Tendal da Lapa" = "Polos Gastronômicos",
  "Centro Cultural Vila Itororó" = "Polos Gastronômicos",
  "Centro TEA" = "Equipamento único",
  "CEU Artur Alvim" = "CEUs - IB",
  "CEU Carrão" = "CEUs - IB",
  "CEU Cidade Ademar" = "1º Lote de CEUs",
  "CEU Cidade Líder" = "1º Lote de CEUs",
  "CEU Cidade Tiradentes" = "CEUs - IB",
  "CEU Ermelino Matarazzo" = "1º Lote de CEUs",
  "CEU Freguesia do Ó" = "CEUs - IB",
  "CEU Grajaú" = "1º Lote de CEUs",
  "CEU Imperador" = "1º Lote de CEUs",
  "CEU José Bonifácio" = "CEUs - IB",
  "CEU Parque do Carmo" = "CEUs - IB",
  "CEU Parque Novo Mundo" = "CEUs - IB",
  "CEU Pinheirinho" = "CEUs - IB",
  "CEU São Miguel" = "CEUs - IB",
  "CEU Taipas" = "CEUs - IB",
  "CEU Tremembé" = "CEUs - IB",
  "CEU Vila Alpina" = "CEUs - IB",
  "Complexo do Anhembi" = "Equipamento único",
  "Crematório da Vila Alpina" = "Cemitérios - Bloco 4",
  "Estádio do Pacaembú" = "Equipamento único",
  "Mercado de Santo Amaro" = "Mercados",
  "Mercado Kinjo Yamato" = "Mercados",
  "Mercado Paulistano" = "Mercados",
  "Parada 14 Bis" = "Equipamento único",
  "Parque Brigadeiro Faria Lima" = "1º Lote de Parques",
  "Parque do Ibirapuera" = "1º Lote de Parques",
  "Parque do Lageado" = "1º Lote de Parques",
  "Parque Eucaliptos" = "1º Lote de Parques",
  "Parque Jacintho Alberto" = "1º Lote de Parques",
  "Parque Jardim da Felicidade" = "1º Lote de Parques",
  "Parque Mário Covas" = "3º Lote de Parques",
  "Parque Trianon" = "3º Lote de Parques",
  "Praça Alexandre de Gusmão" = "3º Lote de Parques",
  "Terminal Água Espraiada" = "Terminal - Bloco Sul",
  "Terminal Amaral Gurgel" = "Terminal - Bloco Noroeste",
  "Terminal Antônio Estevão de Carvalho" = "Terminal - Bloco Leste",
  "Terminal Aricanduva" = "Terminal - Bloco Leste",
  "Terminal Bandeira" = "Terminal - Bloco Sul",
  "Terminal Cachoeirinha" = "Terminal - Bloco Noroeste",
  "Terminal Campo Limpo" = "Terminal - Bloco Noroeste",
  "Terminal Capelinha" = "Terminal - Bloco Sul",
  "Terminal Casa Verde" = "Terminal - Bloco Noroeste",
  "Terminal Cidade Tiradentes" = "Terminal - Bloco Leste",
  "Terminal Grajaú" = "Terminal - Bloco Sul",
  "Terminal Guarapiranga" = "Terminal - Bloco Sul",
  "Terminal Hidroviário Cantinho do Céu" = "Terminais Hidroviários (Lote Sul)",
  "Terminal Hidroviário Mar Paulista" = "Terminais Hidroviários (Lote Sul)",
  "Terminal Itaquera II" = "Terminal - Bloco Leste",
  "Terminal Jardim Ângela" = "Terminal - Bloco Sul",
  "Terminal Jardim Britânia" = "Terminal - Bloco Noroeste",
  "Terminal João Dias" = "Terminal - Bloco Sul",
  "Terminal Lapa" = "Terminal - Bloco Noroeste",
  "Terminal Mercado" = "Terminal - Bloco Leste",
  "Terminal Parada Clínicas" = "Equipamento único",
  "Terminal Parada Eldorado" = "Equipamento único",
  "Terminal Parelheiros" = "Terminal - Bloco Sul",
  "Terminal Parque Dom Pedro II" = "Terminal - Bloco Leste",
  "Terminal Pinheiros" = "Terminal - Bloco Noroeste",
  "Terminal Pirituba" = "Terminal - Bloco Noroeste",
  "Terminal Princesa isabel" = "Terminal - Bloco Noroeste",
  "Terminal Sacomã" = "Terminal - Bloco Leste",
  "Terminal Santo Amaro" = "Terminal - Bloco Sul",
  "Terminal São Miguel" = "Terminal - Bloco Leste",
  "Terminal Sapopemba" = "Terminal - Bloco Leste",
  "Terminal Varginha" = "Terminal - Bloco Sul",
  "Terminal Vila Carrão" = "Terminal - Bloco Leste",
  "Terminal Vila Prudente" = "Terminal - Bloco Leste",
  "Vale do Anhangabaú" = "Equipamento único",
  "Terminal Penha" = "Terminal - Bloco Leste"
)

projetos <- projetos %>%
  mutate(lote = equipamentos_lotes[ppp_nome])

# Para acessar um valor específico (exemplo)
# print(equipamentos_lotes["Cemitério da Consolação"])
# --- ETAPA 7: SALVAR OS OBJETOS PROCESSADOS ---
# Salva os dataframes limpos e prontos para uso em formato .rds.
saveRDS(distritos, file = "DADOS/distritos_processados.rds")
saveRDS(projetos, file = "DADOS/projetos.rds")

# Mensagem de confirmação no console para indicar que o script foi executado com sucesso.
print("Script de análise executado e arquivos .rds (com geometrias de PONTOS) foram salvos com sucesso!")