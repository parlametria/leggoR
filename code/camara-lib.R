source(here::here("code/congresso-lib.R"))

get_nome_ementa_Camara <- function(bill_id) {
  rcongresso::fetch_proposicao(bill_id) %>% dplyr::select(ementa, siglaTipo, numero)
}

tail_descricao_despacho_Camara <- function(df, qtd=1) {
  df %>% 
    dplyr::arrange(data_hora) %>% 
    tail(qtd) %>% 
    dplyr::select(data_hora, descricao_tramitacao, despacho)
}

extract_relator_Camara <- function(df) {
  df %>% 
    dplyr::mutate(relator = 
             case_when(stringr::str_detect(tolower(despacho), '^designad. relat.r') ~ 
                       stringr::str_extract(despacho, regex('dep.+', ignore_case=TRUE))))
}

extract_last_relator_Camara <- function(df) {
  relatores <- extract_relator_Camara(df)
  relator <- 
    relatores %>%
    dplyr::filter(!is.na(relator)) %>%
    dplyr::arrange(desc(data_hora)) %>%
    dplyr::select(relator) 
  
  relator$relator[1]
}

extract_phases_Camara <- function(dataframe, phase_one, phase_two, phase_three, phase_four, phase_five) {
  dataframe %<>%
    dplyr::mutate(fase = dplyr::case_when(detect_phase(id_tipo_tramitacao, phase_one) ~ 'iniciativa',
                            detect_phase(id_tipo_tramitacao, phase_two) ~ 'relatoria',
                            detect_phase(id_tipo_tramitacao, phase_three) ~ 'discussao_deliberacao',
                            detect_phase(id_tipo_tramitacao, phase_four) ~ 'virada_de_casa',
                            detect_phase(id_tipo_tramitacao, phase_five) ~ 'final',
                            detect_phase(id_situacao, 937) ~ 'final'))
}

extract_n_last_events_Camara <- function(df, num) {
  df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::arrange(data_hora) %>%
    tail(n = num) %>%
    dplyr::select(data_hora, evento)
}

rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

extract_events_Camara <- function(tramitacao_df, events_df) {
  tramitacao_df %<>%
    dplyr::mutate(despacho_lower = tolower(despacho)) %>%
    fuzzyjoin::regex_left_join(importants_events, by = c(despacho_lower = "regex")) %>%
    dplyr::select(-c(despacho_lower, regex))
  tramitacao_df %<>%
    dplyr::mutate(evento = dplyr::case_when(id_tipo_tramitacao == special_commission ~ 'criacao_comissao_temporaria', 
                              TRUE ~ evento))
}

refact_date <- function(df) {
  dplyr::mutate(df, data_hora = lubridate::ymd_hm(data_hora))
}

# sort the 'tramitacao' dataframe by date
sort_by_date <- function(df) {
  dplyr::arrange(df, data_hora, sequencia)
}

extract_autor_Camara <- function(prop_id) {
  camara_exp <- 'câmara dos deputados'
  senado_exp <- 'senado federal'
  
  url_base_autores <- 'https://dadosabertos.camara.leg.br/api/v2/proposicoes/'
  url <- paste0(url_base_autores, prop_id, '/autores')
  json_voting <- jsonlite::fromJSON(url, flatten = T)
  
  autores <- json_voting %>% 
    magrittr::extract2("dados") %>%
    dplyr::rename(autor.uri = uri,
           autor.nome = nome,
           autor.tipo = tipo,
           autor.cod_tipo = codTipo) %>% 
    dplyrmutate(casa_origem = dplyr::case_when(
      stringr::str_detect(tolower(autor.nome), camara_exp) | autor.tipo == 'Deputado' ~ 'Câmara dos Deputados',
      stringr::str_detect(tolower(autor.nome), senado_exp) | autor.tipo == 'Senador' ~ 'Senado Federal'))
  
  autores
}

# Retorna a lista de IDs das proposições apensadas
fetch_apensadas <- function(prop_id) {
  api_v1_proposicao = 'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp='
  xml2::read_xml(paste0(api_v1_proposicao, prop_id)) %>% 
    xml2::xml_find_all('//apensadas/proposicao/codProposicao/text()') %>%
    tibble::as.tibble
}

fetch_proposicao_com_apensamentos <- function(prop_id) {
  prop <- rcongresso::fetch_proposicao(prop_id)
  prop$proposicoes_apensadas <- fetch_apensadas(prop_id) %>% paste(collapse = ' ')
  prop
}

fetch_requerimentos_relacionados <- function(id, mark_deferimento=T) {
  regexes <- 
    frame_data(~ deferimento, ~ regex,
             "indeferido", '^Indefiro',
             "deferido", '^(Defiro)|(Aprovado)')

  relacionadas <- 
    rcongresso::fetch_relacionadas(id)$uri %>% 
    strsplit('/') %>% 
    vapply(last, '') %>% 
    unique %>% 
    rcongresso::fetch_proposicao()
  
  requerimentos <- 
    relacionadas %>% 
    dplyr::filter(stringr::str_detect(.$siglaTipo, '^REQ'))
  
  if(!mark_deferimento) return(requerimentos)
  
  tramitacoes <- 
    requerimentos$id %>% 
    rcongresso::fetch_tramitacao()
  
  relacionadas <-
    tramitacoes %>% 
    # mark tramitacoes rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by=c(despacho="regex")) %>% 
    dplyr::group_by(id_prop) %>% 
    # fill down marks
    tidyr::fill(deferimento) %>%
    # get last mark on each tramitacao
    dplyr::do(tail(., n=1)) %>%
    dplyr::ungroup %>% 
    dplyr::select(id_prop, deferimento) %>% 
    # and mark proposicoes based on last tramitacao mark
    dplyr::left_join(relacionadas, by=c('id_prop' = 'id'))
}
