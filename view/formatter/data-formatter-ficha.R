library(here)
library(tidyverse)
library(rcongresso)
source(here::here('R/camara-lib.R'))
source(here::here('R/congresso-lib.R'))
source(here::here('R/analyzer.R'))
source(here::here('R/fetcher.R'))

extract_informations <- function(bill_id_camara, bill_id_senado, url) {
  nome_ementa_camara <- get_ementas_in_camara(bill_id_camara)
  nome_ementa_senado <- get_nome_ementa_Senado(bill_id_senado)

  tramitacao_camara <- read_csv(
    here::here(paste0('data/camara/', 'tramitacao-camara-', bill_id_camara, '.csv')))
  tramitacao_senado <- read_csv(
    here::here(paste0('data/senado/', bill_id_senado, '-fases-tramitacao-senado.csv')))
  despacho_camara <- last_n_despacho_in_camara(tramitacao_camara)
  despacho_senado <- tail_descricao_despacho_Senado(tramitacao_senado)

  last_events_senado <- extract_n_last_eventos_Senado(tramitacao_senado, 3)
  last_events_camara <- extract_last_n_events_in_camara(tramitacao_camara, 3)

  nome <- paste0(
    nome_ementa_senado$sigla_subtipo_materia, nome_ementa_senado$numero_materia,
    ' / ', nome_ementa_camara$siglaTipo, nome_ementa_camara$numero
  )

  casa <- if_else(despacho_camara$data_hora > despacho_senado$data_tramitacao, 'Câmara', 'Senado')
  if(casa == 'Câmara') {
    eventos <- as.list(last_events_camara$evento)
    despacho <- despacho_camara$descricao_tramitacao
    relator <- extract_last_relator_in_camara(tramitacao_camara)
  } else {
    eventos <- as.list(last_events_senado$evento)
    despacho <- despacho_senado$situacao_descricao_situacao
    relator <- fetch_last_relatoria(id)$nome_parlamentar
  }

  proposicoes_df <-
    frame_data(~ nome, ~ casa_atual, ~ ementa, ~ despacho_atual, ~ ultimos_eventos, ~ ultimo_relator,
               nome, casa, nome_ementa_camara$ementa, despacho, eventos, relator)
  proposicoes_df$nome <-paste0('[', proposicoes_df$nome, '](', url, ')')

  proposicoes_df
}

gera_tabela_proposicoes_congresso <- function(dataframe) {
  dataframe %>%
    rowwise() %>%
    do(extract_informations(.$id_camara, .$id_senado, .$url)) %>%
    rbind(tibble::as.tibble(),.) %>%
    mutate(ultimos_eventos = toString(ultimos_eventos))
}

gera_tabela_proposicoes_uma_casa <- function(dataframe) {
  dataframe %>%
    rowwise() %>%
    do(extract_informations_from_single_house(.$id, .$casa, .$url)) %>%
    tibble::as.tibble()
}

extract_informations_from_single_house <- function(id, casa, url=NULL) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    prop_camara <- readr::read_csv(here::here(paste0('data/camara/proposicao-camara-', id, '.csv')))
    tram_camara <- readr::read_csv(here::here(paste0('data/camara/tramitacao-camara-', id, '.csv')))
    nome_camara <- prop_camara %>% dplyr::select(ementa, sigla_tipo, numero) %>% tail(1)
    ano <- tram_camara$ano
    page_url <- prop_camara$page_url
    regime <- prop_camara$regime_tramitacao
    apreciacao <- prop_camara$forma_apreciacao
    nome <- paste0(nome_camara$siglaTipo, nome_camara$numero, "/", ano)
    despacho_camara <- last_n_despacho_in_camara(tram_camara)
    autor <- extract_autor_in_camara(id) %>% tail(1)
    ementa <- nome_camara$ementa
    relator <- extract_last_relator_in_camara(tram_camara)
    despacho <- despacho_camara$descricao_tramitacao
    nome_autor <- autor$autor.nome
    casa_origem <- autor$casa_origem
    data_apresentacao <- format(as.Date(fetch_proposicao(id, 'camara')$dataApresentacao), '%d/%m/%Y')
    eventos <- as.list(extract_last_n_events_in_camara(tram_camara, 3)$evento)
  } else if (casa == 'senado') {
    prop_senado <- readr::read_csv(here::here(paste0('data/senado/', id, '-proposicao-senado.csv')))
    tram_senado <- readr::read_csv(here::here(paste0('data/senado/', id, '-fases-tramitacao-senado.csv')))
    ano <- prop_senado$ano_materia
    apreciacao <- extract_apreciacao_Senado(id)
    regime <- extract_regime_Senado(tram_senado)
    despacho_senado <- tail_descricao_despacho_Senado(tram_senado)
    nome_senado <- prop_senado %>% select(ementa_materia, sigla_subtipo_materia, numero_materia) %>% unique
    page_url <- prop_senado$page_url
    nome <- paste0(nome_senado$sigla_subtipo_materia, nome_senado$numero_materia, '/', ano)
    casa_origem <- prop_senado$nome_casa_origem
    nome_autor <- prop_senado$nome_autor
    partido_autor <- prop_senado$sigla_partido_parlamentar
    uf_autor <- prop_senado$uf_parlamentar
    nome_autor <- ifelse(is.null(partido_autor) & is.null(uf_autor), nome_autor, paste0(nome_autor, ' ', partido_autor, '/', uf_autor))
    despacho <- despacho_senado$texto_tramitacao
    relatoria <- fetch_last_relatoria(id) %>% tail(1)
    ementa <- if_else(is.null(prop_senado$explicacao_ementa_materia), prop_senado$ementa_materia, prop_senado$explicacao_ementa_materia)
    relator <- extract_ultimo_relator(id)
    data_apresentacao <- format(as.Date(prop_senado$data_apresentacao), '%d/%m/%Y') %>% tail(1)
    eventos <-  as.list(extract_n_last_eventos_Senado(tram_senado, 3)$evento)
  }
  proposicoes_df <-
    frame_data(~ nome, ~autor, ~ casa_origem, ~ data_apresentacao, ~ ementa, ~ status_atual, ~ ultimo_relator, ~ ultimos_eventos, ~ regime, ~ apreciacao, ~ page_url,
               nome, nome_autor, casa_origem, data_apresentacao, ementa, despacho, relator, eventos, regime, apreciacao, page_url)

  proposicoes_df$nome <-paste0("[", proposicoes_df$nome, "](", url, ")")

  proposicoes_df
}

extract_ultimo_relator <- function(id){
  data <- fetch_current_relatoria(id)
  if(ncol(data)){
    paste0(data$identificacao_parlamentar_nome_parlamentar, ' - ',
           ifelse(
             identical(data$identificacao_parlamentar_sigla_partido_parlamentar[[1]], character(0)) |
               identical(data$identificacao_parlamentar_uf_parlamentar[[1]], character(0)),
             '',
             paste0(data$identificacao_parlamentar_sigla_partido_parlamentar, '/', data$identificacao_parlamentar_uf_parlamentar)
           )
    )
  } else {
    'Não encontrado'
  }
}

gera_tabela_apensadas_senado <- function(bill_id_senado) {
  url_senado <- "https://www25.senado.leg.br/web/atividade/materias/-/materia/"

  senado <-
    fetch_proposicao(bill_id_senado, 'senado')

  #se não tiver proposição
  if (!("" %in% senado$proposicoes_apensadas)) {
    senado <-
      senado %>%
      mutate(proposicoes_apensadas = strsplit(.$proposicoes_apensadas, " ")) %>%
      unnest() %>%
      select(apensadas = proposicoes_apensadas)

    senado  %>%
      rowwise() %>%
      mutate(casa = "Senado",
             apensadas = paste0("[", paste0(get_nome_ementa_Senado(apensadas)$sigla_subtipo_materia, get_nome_ementa_Senado(apensadas)$numero_materia), "](", paste0(url_senado, apensadas), ")")) %>%
      select(apensadas, casa)
  }else {
    NA
  }
 }

extract_informations_all_houses <- function(senado_id, camara_id) {
  df_camara <- extract_informations_from_single_house(camara_id, 'camara')
  df_senado <- extract_informations_from_single_house(senado_id, 'senado')

  data <- frame_data()

  if (tolower(df_camara$casa_origem) == 'senado federal') {
    nome <- df_senado$nome
    data_apresentacao <- df_senado$data_apresentacao
    ementa <- df_senado$ementa
    autor <- df_senado$autor
    casa_origem <- df_senado$casa_origem
    status_atual <- df_camara$status_atual
    ultimo_relator <- df_camara$ultimo_relator
    casa_atual <- 'Câmara dos Deputados'

  } else {
    nome <- df_camara$nome
    data_apresentacao <- df_camara$data_apresentacao
    ementa <- df_camara$ementa
    autor <- df_camara$autor
    casa_origem <- df_camara$casa_origem
    status_atual <- df_senado$status_atual
    ultimo_relator <- df_senado$ultimo_relator
    casa_atual <- 'Senado Federal'
  }

  proposicoes_df <-
    frame_data(~ nome, ~autor, ~ casa_origem, ~ data_apresentacao, ~ ementa, ~ status_atual, ~ ultimo_relator, ~casa_atual,
               nome, autor, casa_origem, data_apresentacao, ementa, status_atual, ultimo_relator, casa_atual)
  proposicoes_df
}

gera_tabela_requerimentos <- function(bill_id, house) {
  requerimentos <- data.frame()
  if (house == 'camara') {
    requerimentos <- fetch_related_requerimentos(bill_id) %>% select(dataApresentacao,descricaoTipo,ementa,deferimento,statusProposicao.despacho)
  } else if (house == 'senado') {
    requerimentos <- as.array(strsplit(fetch_proposicao(bill_id, 'senado')$proposicoes_relacionadas, " ")[[1]]) %>% fetch_deferimento()
  }
  requerimentos
}

gera_tabela_apensadas_camara <- function(bill_id_camara) {
  url_camara <- "http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao="

  apensadas <-
    fetch_apensadas(bill_id_camara)

  if (nrow(apensadas) != 0) {
    apensadas %>%
      mutate(casa = "Câmara",
             apensadas = paste0("[", paste0(get_ementas_in_camara(apensadas)$siglaTipo, get_ementas_in_camara(apensadas)$numero), "](", paste0(url_camara, apensadas), ")"))
  } else {
    apensadas %>%
      mutate(casa = "Câmara")
  }
}
