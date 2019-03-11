#' @title Busca a movimentação da proposição
#' @description Retorna dataframe com os dados da movimentação da proposição, incluindo tramitação, prazos, despachos e situação
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param prop_id ID de uma proposição do Senado
#' @return Dataframe normalizado com as informações sobre a movimentação de uma proposição no Senado
#' @examples
#' fetch_tramitacao_senado(91341)
fetch_tramitacao_senado <- function(prop_id) {
    textos <- 
      extract_links_proposicao_senado(prop_id) %>%
      dplyr::select(data_hora = data,
                    link_inteiro_teor) %>%
      dplyr::mutate(data_hora = as.POSIXct(data_hora, tz = "UTC"))
  
    tramitacao <- rcongresso::fetch_tramitacao_senado(prop_id) %>%
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
                    descricao_situacao = situacao_descricao_situacao) %>%
      dplyr::left_join(textos, by = "data_hora")
    
}

#' @title Baixa os dados da tramitação de um Projeto de Lei
#' @description Retorna dataframe com os dados da tramitação de uma proposição no Congresso
#' @param id ID de uma proposição na sua respectiva casa
#' @param casa Casa onde a proposição está tramitando
#' @return Dataframe normalizado com os dados da tramitação de uma proposição no Congresso
#' @examples
#' fetch_tramitacao(91341, "senado")
#' @export
fetch_tramitacao <- function(id, casa) {
  casa <- tolower(casa)
  if (casa == "camara") {
    fetch_tramitacao_camara(id)
  } else if (casa == "senado") {
    fetch_tramitacao_senado(id)
  } else {
    print("Parâmetro 'casa' não identificado.")
  }
}

#' @title Baixa os dados da tramitação da Câmara
#' @description Retorna dataframe com os dados da tramitação de uma proposição da Camara
#' @param bill_id ID de uma proposição na Camara
#' @return Dataframe com os dados da tramitação de uma proposição da Camara
#' @examples
#' fetch_tramitacao_camara(2121442)
fetch_tramitacao_camara <- function(bill_id) {
  rcongresso::fetch_tramitacao_camara(bill_id) %>%
    dplyr::mutate(data_hora = lubridate::ymd_hm(stringr::str_replace(data_hora,"T"," ")),
                  casa = "camara",
                  id_situacao = as.integer(cod_tipo_tramitacao)) %>%
    dplyr::select(prop_id = id_prop,
                  casa,
                  data_hora,
                  sequencia,
                  texto_tramitacao = despacho,
                  sigla_local = sigla_orgao,
                  id_situacao,
                  descricao_situacao,
                  link_inteiro_teor = url)
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
    purrr::map2_df(pls_ids$id, pls_ids$casa, ~ fetch_tramitacao(.x, .y))
}
