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
#' @export
last_n_despacho_in_camara <- function(df, qtd = 1) {
  df %>%
    dplyr::arrange(data_hora) %>%
    tail(qtd) %>%
    dplyr::select(data_hora, texto_tramitacao)
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

#' @title Retorna o regex das comissões na Câmara
#' @description Retorna o regex das comissões na Câmara
#' @return Regex das comissões na Câmara
#' @examples
#' get_regex_comissoes_camara()
get_regex_comissoes_camara <- function() {
  get_comissoes_camara() %>%
    unlist() %>%
    paste(collapse = '|') %>%
    stringr::regex(ignore_case = TRUE)
}

#' @title Recupera as comissões pelas quais a proposição irá passar
#' @description Retorna um dataframe das comissões pelas quais a proposição irá passar, contendo a hora, o id da proposição e
#' as próximas comissões
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com as próximas comissões que a proposição irá passar.
#' @export
get_comissoes_in_camara <- function(df) {
  reg <-
    unlist(get_comissoes_camara()) %>%
    paste(collapse = '|') %>%
    stringr::regex(ignore_case = TRUE)

  fix_names <- function(name) {
    ifelse(!stringr::str_detect(name, 'Comissão') & !grepl("^[[:upper:]]+$", name), paste("Comissão de", name), name)
  }

  detect <- function(str, regex) {
    stringr::str_detect(str, stringr::regex(regex, ignore_case = TRUE))
  }

  df %>%
    dplyr::mutate(
      comissoes = dplyr::case_when(
        (detect(texto_tramitacao, 'cria..o de comiss.o tempor.ria') |
         detect(texto_tramitacao, 'à comiss.o .*especial')) ~
        'Comissão Especial',
        ((detect(texto_tramitacao, '^às* comiss..s*') |
         detect(texto_tramitacao, '^despacho à'))
         #|detect(texto_tramitacao, 'novo despacho')
         ) ~
        texto_tramitacao)
    ) %>%
    dplyr::filter(!is.na(comissoes)) %>%
    dplyr::mutate(proximas_comissoes = stringr::str_extract_all(comissoes, reg) %>% as.list()) %>%
    dplyr::select(data_hora, prop_id, proximas_comissoes) %>%
    dplyr::mutate(proximas_comissoes = purrr::map(proximas_comissoes, fix_names)) %>%
    unique()
}

#' @title Altera as datas da tramitação para formato mais fácil de tratar
#' @description Formata cada data da coluna para o formato POSIXct
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna de datas refatorada para um formato tratável.
#' @export
refact_date <- function(df) {
  dplyr::mutate(df, data_hora = lubridate::ymd_hms(data_hora))
}

#' @title Ordena o dataframe de acordo com a data
#' @description Ordena o dataframe de acordo com a data
#' @param df Dataframe contendo coluna de datas
#' @return Dataframe ordenado pela data
#' @export
sort_by_date <- function(df) {
  dplyr::arrange(df, data_hora, sequencia)
}

#' @title Retorna proposição com apensamentos
#' @description Retorna o dataframe de proposição da Câmara adicionando a coluna de apensadas
#' @param prop_id Id da proposição
#' @return Dataframe da proposição + a coluna proposicoes_apensadas
#' @examples
#' fetch_proposicao_with_apensamentos(2121442)
#' @export
fetch_proposicao_with_apensamentos <- function(prop_id) {
  rcongresso::fetch_proposicao_camara(prop_id) %>%
    dplyr::mutate(proposicoes_apensadas = paste(rcongresso::fetch_apensadas_camara(prop_id), collapse = ' '))
}

#' @title Recupera a proposição com as colunas renomeadas
#' @description Recupera o Dataframe contendo os detalhes da proposição com as colunas renomeadas
#' @param id ID da proposição
#' @return Dataframe da proposição
#' @examples
#' fetch_proposicao_renamed(345311)
#' @export
#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao_camara(id) %>%
    rename_df_columns

  df[,!sapply(df, is.list)]
}
