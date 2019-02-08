source(here::here("R/utils.R"))

#' @title Busca a movimentação da proposição
#' @description Retorna dataframe com os dados da movimentação da proposição, incluindo tramitação, prazos, despachos e situação
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @param normalized whether or not the output dataframe should be normalized (have the same format and column names for every house)
#' @return Dataframe com as informações sobre a movimentação de uma proposição no Senado
#' @examples
#' fetch_tramitacao_senado(91341)
fetch_tramitacao_senado <- function(proposicao_id, normalized=FALSE) {
  url <-
    paste0(senado_env$endpoints_api$url_base,
           "movimentacoes/",
           proposicao_id)
  
  json_tramitacao <- fetch_json_try(url)
  
  tramitacao_data <-
    json_tramitacao %>%
    magrittr::extract2("MovimentacaoMateria") %>%
    magrittr::extract2("Materia")
  tramitacao_ids <-
    tramitacao_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as.tibble()
  tramitacao_actual_situation <-
    tramitacao_data %>%
    magrittr::extract2("SituacaoAtual") %>%
    magrittr::extract2("Autuacoes") %>%
    magrittr::extract2("Autuacao") %>%
    magrittr::extract2("Situacao") %>%
    tibble::as.tibble()
  proposicao_tramitacoes_df <-
    tramitacao_data %>%
    magrittr::extract2("Tramitacoes") %>%
    magrittr::extract2("Tramitacao") %>%
    tibble::as.tibble() %>%
    tibble::add_column(!!!tramitacao_ids)
  
  proposicao_tramitacoes_df <-
    proposicao_tramitacoes_df[, !sapply(proposicao_tramitacoes_df, is.list)]
  
  proposicao_tramitacoes_df <-
    rename_tramitacao_df(proposicao_tramitacoes_df) %>%
    dplyr::rename(data_hora = data_tramitacao, sequencia = numero_ordem_tramitacao)
  
  if (normalized) {
    proposicao_tramitacoes_df <- proposicao_tramitacoes_df %>%
      dplyr::mutate(data_hora = lubridate::ymd_hm(paste(data_hora, "00:00")),
                    prop_id = as.integer(codigo_materia),
                    sequencia = as.integer(sequencia),
                    id_situacao = as.integer(situacao_codigo_situacao),
                    casa = "senado") %>%
      dplyr::select(prop_id,
                    casa,
                    data_hora,
                    sequencia,
                    texto_tramitacao,
                    sigla_local = origem_tramitacao_local_sigla_local,
                    id_situacao,
                    descricao_situacao = situacao_descricao_situacao)
  }
  
  proposicao_tramitacoes_df
}

#' @title Renomeia as colunas do dataframe de movimentação no Senado
#' @description Renomeia as colunas do dataframe de movimentação no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe da votação no Senado
#' @return Dataframe com as colunas renomeadas
#' @export
rename_tramitacao_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace(
      "identificacao_tramitacao_|
      identificacao_tramitacao_origem_tramitacao_local_|
      identificacao_tramitacao_destino_tramitacao_local_|
      identificacao_tramitacao_situacao_",
      ""
    )
  
  names(df) <- new_names
  
  df
}

#' @title Baixa os dados da tramitação de um Projeto de Lei
#' @description Retorna dataframe com os dados da tramitação de uma proposição no Congresso
#' @param id ID de uma proposição na sua respectiva casa
#' @param casa Casa onde a proposição está tramitando
#' @param normalized whether or not the output dataframe should be normalized (have the same format and column names for every house)
#' @return Dataframe com os dados da tramitação de uma proposição no Congresso
#' @examples
#' fetch_tramitacao(91341,'senado')
#' @export
fetch_tramitacao <- function(id, casa, normalized=FALSE) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    fetch_tramitacao_camara(id, normalized)
  } else if (casa == 'senado') {
    fetch_tramitacao_senado(id, normalized)
  } else {
    print('Parâmetro "casa" não identificado.')
  }
}

#' @title Baixa os dados da tramitação de vários Projetos de Lei
#' @description Retorna dataframe com os dados da tramitação de proposições no Congresso
#' @param id ID de uma proposição na sua respectiva casa
#' @param casa Casa onde a proposição está tramitando
#' @return Dataframe com os dados da tramitação de proposições no Congresso
#' @examples
#' all_pls <- readr::read_csv('data/tabela_geral_ids_casa.csv')
#' fetch_tramitacoes(all_pls)
#' @export
fetch_tramitacoes <- function(pls_ids) {
  purrr::map2_df(pls_ids$id, pls_ids$casa, ~ fetch_tramitacao(.x, .y, TRUE))
}

#' @title Baixa os dados da tramitação da Câmara
#' @description Retorna dataframe com os dados da tramitação de uma proposição da Camara
#' @param bill_id ID de uma proposição na Camara
#' @param normalized Parametro para normalizar os dados
#' @return Dataframe com os dados da tramitação de uma proposição da Camara
#' @examples
#' fetch_tramitacao_camara(2121442, TRUE)
fetch_tramitacao_camara <- function(bill_id, normalized=FALSE) {
  tram_camara <- rcongresso::fetch_tramitacao(bill_id) %>%
    rename_df_columns
  
  if (normalized) {
    tram_camara <- tram_camara %>%
      dplyr::mutate(data_hora = lubridate::ymd_hm(stringr::str_replace(data_hora,'T',' ')),
                    casa = 'camara',
                    id_situacao = as.integer(cod_tipo_tramitacao)) %>%
      dplyr::select(prop_id = id_prop,
                    casa,
                    data_hora,
                    sequencia,
                    texto_tramitacao = despacho,
                    sigla_local = sigla_orgao,
                    id_situacao,
                    descricao_situacao)
  }
  
  tram_camara
}