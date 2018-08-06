source(here::here("R/congresso-lib.R"))
source(here::here("R/utils.R"))
camara_codes <-
  jsonlite::fromJSON(here::here("R/config/environment_camara.json"))

#' @title Recupera o número, o tipo e ementa de uma proposição na Câmara
#' @description Retorna um dataframe contendo o número, o tipo e a ementa de uma proposição na Câmara através do ID da proposição
#' @param prop_id ID da proposição
#' @return Dataframe com o número, o tipo e a ementa da proposição na Câmara.
#' @examples
#' get_ementas_in_camara(2121442)
#' @export
get_ementas_in_camara <- function(prop_id) {
  rcongresso::fetch_proposicao(prop_id) %>% dplyr::select(ementa, siglaTipo, numero)
}

#' @title Recupera os n últimos despachos na Câmara
#' @description Retorna um dataframe das últimas n tramitações na Câmara contendo a hora, a descrição e o despacho
#' @param df Dataframe da tramitação na Câmara
#' @param qtd  (opcional) Quantidade de eventos a serem recuperados. (Default: qtd = 1)
#' @return Dataframe com as última n tramitações da Câmara.
#' @examples
#' tramitacao %>% last_n_despacho_in_camara()
#' tramitacao %>% last_n_despacho_in_camara(4)
#' @export
last_n_despacho_in_camara <- function(df, qtd = 1) {
  df %>%
    dplyr::arrange(data_hora) %>%
    tail(qtd) %>%
    dplyr::select(data_hora, descricao_tramitacao, despacho)
}

#' @title Retorna o dataframe das comissões na Câmara
#' @description Retorna o dataframe das comissões na Câmara, contendo as colunas: siglas_comissoes_antigas, siglas_comissoes, comissoes_temporarias, comissoes_permanentes
#' @return Dataframe das comissões na Câmara
#' @examples
#' get_comissoes_camara()
#' @export
get_comissoes_camara <- function() {
  comissoes <- camara_codes$comissoes
  dplyr::tibble(
    siglas_comissoes_antigas = list(comissoes$siglas_comissoes_antigas),
    siglas_comissoes = list(comissoes$siglas_comissoes),
    comissoes_temporarias = list(comissoes$comissoes_temporarias),
    comissoes_permanentes = list(comissoes$comissoes_permanentes)
  )
}


get_regex_comissoes_camara <- function() {
  get_comissoes_camara() %>%
    unlist() %>%
    paste(collapse = '|') %>%
    regex(ignore_case = TRUE)
}

#' @title Recupera as comissões pelas quais a proposição irá passar
#' @description Retorna um dataframe das comissões pelas quais a proposição irá passar, contendo a hora, o id da proposição e
#' as próximas comissões
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com as próximas comissões que a proposição irá passar.
#' @examples
#' tramitacao %>% get_comissoes_in_camara(df)
#' @export
get_comissoes_in_camara <- function(df) {
  reg <-
    unlist(get_comissoes_camara()) %>%
    paste(collapse = '|') %>%
    stringr::regex(ignore_case = TRUE)
  
  fix_names <- function(name) {
    if (!str_detect(name, 'Comissão') & !grepl("^[[:upper:]]+$", name))
      paste("Comissão de", name)
    else
      name
  }
  
  detect <- function(str, regex) {
    stringr::str_detect(str, stringr::regex(regex, ignore_case = TRUE))
  }
  
  df %>%
    dplyr::mutate(comissoes = dplyr::case_when((
      detect(descricao_tramitacao, 'distribuição') &
        (
          detect(descricao_tramitacao, 'cria..o de comiss.o tempor.ria') |
            detect(despacho, 'especial')
        )
    ) ~ 'Comissão Especial',
    (
      detect(descricao_tramitacao, 'distribuição') &
        (
          detect(despacho, 'às* comiss..s*|despacho à') |
            detect(despacho, 'novo despacho')
        )
    ) ~ despacho)) %>%
    dplyr::filter(!is.na(comissoes)) %>%
    dplyr::mutate(proximas_comissoes = stringr::str_extract_all(comissoes, reg) %>% as.list()) %>%
    dplyr::select(data_hora, id_prop, proximas_comissoes) %>%
    dplyr::mutate(proximas_comissoes = map(proximas_comissoes, fix_names)) %>%
    dplyr::mutate(proximas_comissoes = unique(proximas_comissoes, incomparables = FALSE))
}



#' @title Altera as datas da tramitação para formato mais fácil de tratar
#' @description Formata cada data da coluna para o formato POSIXct
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna de datas refatorada para um formato tratável.
#' @examples
#' df %>% refact_date()
#' @export
refact_date <- function(df) {
  dplyr::mutate(df, data_hora = lubridate::ymd_hm(data_hora))
}

#' @title Ordena o dataframe de acordo com a data
#' @description Ordena o dataframe de acordo com a data
#' @param df Dataframe contendo coluna de datas
#' @return Dataframe ordenado pela data
#' @examples
#' df %>% sort_by_date()
#' @export
sort_by_date <- function(df) {
  dplyr::arrange(df, data_hora, sequencia)
}

fetch_proposicao_with_apensamentos <- function(prop_id) {
  rcongresso::fetch_proposicao(prop_id) %>%
    dplyr::mutate(proposicoes_apensadas = paste(fetch_apensadas(prop_id), collapse =
                                                  ' '))
}

#' @title Baixa dados de requerimentos relacionados
#' @description Retorna um dataframe contendo dados sobre os requerimentos relacionados a uma proposição
#' @param prop_id ID de uma proposição
#' @return Dataframe
#' @examples
#' fetch_releated_requerimentos(2056568)
#' @export
fetch_related_requerimentos <- function(id, mark_deferimento = T) {
  regexes <-
    tibble::frame_data(
      ~ deferimento,
      ~ regex,
      'indeferido',
      '^Indefiro',
      'deferido',
      '^(Defiro)|(Aprovado)'
    )
  
  related <-
    rcongresso::fetch_relacionadas(id)$uri %>%
    strsplit('/') %>%
    vapply(last, '') %>%
    unique %>%
    rcongresso::fetch_proposicao()
  
  requerimentos <-
    related %>%
    dplyr::filter(stringr::str_detect(.$siglaTipo, '^REQ'))
  
  if (!mark_deferimento)
    return(requerimentos)
  
  tramitacoes <-
    requerimentos$id %>%
    rcongresso::fetch_tramitacao()
  
  related <-
    tramitacoes %>%
    # mark tramitacoes rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by = c(despacho = 'regex')) %>%
    dplyr::group_by(id_prop) %>%
    # fill down marks
    tidyr::fill(deferimento) %>%
    # get last mark on each tramitacao
    dplyr::do(tail(., n = 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_prop, deferimento) %>%
    # and mark proposicoes based on last tramitacao mark
    dplyr::left_join(related, by = c('id_prop' = 'id'))
}

#' @title Recupera os últimos eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' get_latest_events(2121442)
#' @export
get_latest_events <- function(prop_id) {
  fetch_events(prop_id) %>%
    dplyr::filter(timestamp <= lubridate::now())
}

#' @title Recupera os próximos eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' get_next_events(2121442)
#' @export
get_next_events <- function(prop_id) {
  fetch_events(prop_id) %>%
    dplyr::filter(timestamp > lubridate::now())
}



#' @title Recupera a proposição com as colunas renomeadas
#' @description Recupera o Dataframe contendo os detalhes da proposição com as colunas renomeadas
#' @param id ID da proposição
#' @return Dataframe da proposição
#' @examples
#'  fetch_proposicao_camara(345311)
#' @export
#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao_camara(id) %>%
    rename_df_columns
  
  df[,!sapply(df, is.list)]
}
