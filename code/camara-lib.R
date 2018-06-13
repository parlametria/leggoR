library(here)
source(here("code/congresso-lib.R"))

#' @title Cria coluna com as fases da tramitação na Câmara
#' @description Cria uma nova coluna com as fases na Câmara.
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "fase" adicionada.
#' @examples
#' tramitacao %>% extract_phases_Camara()
#' @export
extract_phases_Camara <- function(dataframe, phase_one, phase_two, phase_three, phase_four, phase_five) {
  require(magrittr)
  
  dataframe %<>%
    mutate(fase = case_when(detect_phase(id_tipo_tramitacao, phase_one) ~ 'iniciativa',
                            detect_phase(id_tipo_tramitacao, phase_two) ~ 'relatoria',
                            detect_phase(id_tipo_tramitacao, phase_three) ~ 'discussao_deliberacao',
                            detect_phase(id_tipo_tramitacao, phase_four) ~ 'virada_de_casa',
                            detect_phase(id_tipo_tramitacao, phase_five) ~ 'final',
                            detect_phase(id_situacao, 937) ~ 'final'))
}

#' @title Extrai os eventos importantes que aconteceram na Câmara
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram na Câmara
#' @param tramitacao_df Dataframe da tramitação na Cãmara
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
#' @examples
#' df %>% extract_events_Camara(importants_events)
#' @export
extract_events_Camara <- function(tramitacao_df, events_df) {
  require(magrittr)
  require(tidyverse)
  
  tramitacao_df %<>%
    mutate(despacho_lower = tolower(despacho)) %>%
    regex_left_join(importants_events, by = c(despacho_lower = "regex")) %>%
    select(-c(despacho_lower, regex))
  tramitacao_df %<>%
    mutate(evento = case_when(id_tipo_tramitacao == special_commission ~ 'criacao_comissao_temporaria', 
                              TRUE ~ evento))
}

#' @title Cria coluna com os relatores na tramitação na Câmara
#' @description Cria uma nova coluna com os relatores na Câmara. O relator é adicionado à coluna no 
#' envento pontual em que ele é designado
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "relator" adicionada.
#' @examples
#' tramitacao %>% extract_relator_Camara()
#' @export
extract_relator_Camara <- function(df) {
  require(dplyr)
  require(stringr)
  
  df %>% 
    mutate(relator = 
             case_when(str_detect(tolower(despacho), '^designad. relat.r') ~ str_extract(despacho, regex('dep.+', ignore_case=TRUE))))
}

#' @title Altera as datas da tramitação para formato mais fácil de tratar 
#' @description Formata cada data da coluna para o formato POSIXct
#' @param df Dataframe da tramitação na Cãmara
#' @return Dataframe com a coluna de datas refatorada para um formato tratável.
#' @examples
#' df %>% refact_date()
#' @export
refact_date <- function(df) {
  require(lubridate)
  
  mutate(df, data_hora = ymd_hm(data_hora))
}

#' @title Renomeia as colunas do dataframe
#' @description Renomeia as colunas do dataframe usando o padrão de letras minúsculas e underscore
#' @param df Dataframe 
#' @return Dataframe com as colunas renomeadas.
#' @examples
#' df %>% rename_df_columns()
#' @export
rename_df_columns <- function(df) {
  require(magrittr)
  
  names(df) %<>% to_underscore
  df
}

#' @title Ordena o dataframe de acordo com a data
#' @description Ordena o dataframe de acordo com a data
#' @param df Dataframe contendo coluna de datas
#' @return Dataframe ordenado pela data
#' @examples
#' df %>% sort_by_date()
#' @export
sort_by_date <- function(df) {
  require(tidyverse)
  
  arrange(df, data_hora, sequencia)
}

#' @title Recupera o autor de uma proposição na Câmara
#' @description Retorna um dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor
#' @param prop_id ID da proposição
#' @return Dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor.
#' @examples
#' extract_autor_Camara(2121442)
#' @export
extract_autor_Camara <- function(prop_id) {
  require(magrittr)
  require(jsonlite)
  require(stringr)
  
  camara_exp <- 'câmara dos deputados'
  senado_exp <- 'senado federal'
  
  url_base_autores <- 'https://dadosabertos.camara.leg.br/api/v2/proposicoes/'
  url <- paste0(url_base_autores, prop_id, '/autores')
  json_voting <- fromJSON(url, flatten = T)
  
  autores <- json_voting %>% 
    extract2("dados") %>%
    rename(autor.uri = uri,
           autor.nome = nome,
           autor.tipo = tipo,
           autor.cod_tipo = codTipo) %>% 
    mutate(casa_origem = case_when(
      str_detect(tolower(autor.nome), camara_exp) | autor.tipo == 'Deputado' ~ 'Câmara dos Deputados',
      str_detect(tolower(autor.nome), senado_exp) | autor.tipo == 'Senador' ~ 'Senado Federal'))
  
  autores
}

#' @title Recupera as proposições apensadas
#' @description Retorna os IDs das proposições apensadas a uma determinada proposição
#' @param prop_id ID da proposição
#' @return Ventor contendo os IDs das proposições apensadas
#' @examples
#' fetch_apensadas(2121442)
#' @export
fetch_apensadas <- function(prop_id) {
  require(xml2)
  api_v1_proposicao = 'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp='
  read_xml(paste0(api_v1_proposicao, prop_id)) %>% 
    xml_find_all('//apensadas/proposicao/codProposicao/text()')
}

#' @title Recupera o número, o tipo e ementa de uma proposição na Câmara
#' @description Retorna um dataframe contendo o número, o tipo e a ementa de uma proposição na Câmara através do ID da proposição
#' @param bill_id ID da proposição
#' @return Dataframe com o número, o tipo e a ementa da proposição na Câmara.
#' @examples
#' get_nome_ementa_Camara(2121442)
#' @export
get_nome_ementa_Camara <- function(bill_id) {
  require(dplyr)
  require(rcongresso)
  
  fetch_proposicao(bill_id) %>% select(ementa, siglaTipo, numero)
}

#' @title Recupera os n últimos despachos na Câmara
#' @description Retorna um dataframe das últimas n tramitações na Câmara contendo a hora, a descrição e o despacho 
#' @param df Dataframe da tramitação na Câmara
#' @param qtd  (opcional) Quantidade de eventos a serem recuperados
#' @return Dataframe com as última n tramitações da Câmara.
#' @examples
#' tramitacao %>% tail_descricao_despacho_Camara()
#' tramitacao %>% tail_descricao_despacho_Camara(4)
#' @export
tail_descricao_despacho_Camara <- function(df, qtd=1) {
  require(dplyr)
  
  df %>% 
    arrange(data_hora) %>% 
    tail(qtd) %>% 
    select(data_hora, descricao_tramitacao, despacho)
}

#' @title Busca os últimos n eventos da tramitação na Câmara
#' @description Recupera os útimos n eventos da tramitação na Cãmara, caso nenhuma quantidade seja informada, assume-se que é 1
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe dos últimos n eventos na Câmara contendo hora e evento.
#' @examples
#' tramitacao %>% extract_n_last_events_Camara()
#' tramitacao %>% extract_n_last_events_Camara(3)
#' @export
extract_n_last_events_Camara <- function(df, num=1) {
  require(tidyverse)
  
  df %>%
    arrange(data_hora) %>%
    tail(n = num) %>%
    select(data_hora, evento)
}

fetch_proposicao_com_apensamentos <- function(prop_id) {
  prop <- rcongresso::fetch_proposicao(prop_id)
  prop$proposicoes_apensadas <- fetch_apensadas(prop_id) %>% paste(collapse = ' ')
  prop
}

fetch_requerimentos_relacionados <- function(id, mark_deferimento=T) {
  require(dplyr)
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
    filter(stringr::str_detect(.$siglaTipo, '^REQ'))
  
  if(!mark_deferimento) return(requerimentos)
  
  tramitacoes <- 
    requerimentos$id %>% 
    rcongresso::fetch_tramitacao()
  
  relacionadas <-
    tramitacoes %>% 
    # mark tramitacoes rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by=c(despacho="regex")) %>% 
    group_by(id_prop) %>% 
    # fill down marks
    tidyr::fill(deferimento) %>%
    # get last mark on each tramitacao
    do(tail(., n=1)) %>%
    ungroup %>% 
    select(id_prop, deferimento) %>% 
    # and mark proposicoes based on last tramitacao mark
    left_join(relacionadas, by=c('id_prop' = 'id'))
}

