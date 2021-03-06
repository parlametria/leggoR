congresso_env <- jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))
congresso_constants <- congresso_env$constants

#' @title Busca a movimentação dos requerimentos de uma proposição
#' @description Retorna dataframe com os dados da movimentação de requerimentos de uma proposição, incluindo tramitação, despachos e situação
#' @param prop_id ID de uma proposição
#' @param casa Casa onde a proposição está tramitando
#' @return Dataframe normalizado com as informações sobre a movimentação de requerimentos de uma proposição no Congresso
#' @export
#' @examples
#' fetch_eventos_reqs_prop(46249, 'camara')
fetch_eventos_reqs_prop <- function(prop_id, casa) {
  casa <- tolower(casa)
  eventos_reqs <- tibble::tibble()
  if (casa == congresso_constants$camara_label) {
    eventos_reqs <- fetch_eventos_reqs_prop_camara(prop_id)
  } else if (casa == congresso_constants$senado_label) {
    eventos_reqs <- fetch_eventos_reqs_prop_senado(prop_id)
  } else {
    print("Parâmetro 'casa' não identificado.")
  }
  return(eventos_reqs)
}

#' @title Busca a movimentação dos requerimentos de uma proposição na Câmara
#' @description Retorna dataframe com os dados da movimentação de requerimentos de uma proposição, incluindo tramitação, despachos e situação
#' @param prop_id ID de uma proposição na Câmara
#' @return Dataframe normalizado com as informações sobre a movimentação de requerimentos de uma proposição na Câmara
#' @examples
#' fetch_eventos_reqs_prop_camara(46249)
fetch_eventos_reqs_prop_camara <- function(prop_id) {
  reqs <- tryCatch(
    rcongresso::fetch_related_requerimentos_camara(prop_id = prop_id),
    error = function(error_message) {
      warning(paste("Erro na recuperação dos requerimentos do PL com id - ",prop_id, error_message))
      return(tibble::tibble())
    }
  )

  if(nrow(reqs) == 0) {
    return(tibble::tibble())
  }

  reqs_tipos <- reqs %>%
    dplyr::select(id_req, tipo_documento = descricaoTipo)

  eventos_reqs <- purrr::map_df(reqs$id_req, ~ rcongresso::fetch_events_requerimento_camara(.x))

  if(nrow(eventos_reqs) != 0) {
    eventos_reqs <-
      eventos_reqs %>%
      dplyr::left_join(reqs_tipos, by="id_req") %>%
      dplyr::select(-cod_situacao, -descricao_tramitacao, -regime, -uri_orgao, -id_req) %>%
      dplyr::rename(texto_tramitacao = despacho,
                    sigla_local = sigla_orgao,
                    id_situacao = cod_tipo_tramitacao,
                    link_inteiro_teor = url) %>%
      dplyr::mutate(data_hora = lubridate::ymd_hm(stringr::str_replace(data_hora,'T','')),
                    local = sigla_local,
                    prop_id = prop_id,
                    casa = congresso_constants$camara_label,
                    id_situacao = as.integer(id_situacao),
                    sequencia = as.integer(sequencia),
                    tipo_documento = dplyr::if_else(tipo_documento == 'Requerimento', 'Requerimento (tipo não especificado)', tipo_documento))
  }

  return(eventos_reqs)
}

#' @title Busca a movimentação dos requerimentos de uma proposição na Câmara
#' @description Retorna dataframe com os dados da movimentação de requerimentos de uma proposição, incluindo tramitação, despachos e situação
#' @param prop_id ID de uma proposição no Senado
#' @return Dataframe normalizado com as informações sobre a movimentação de requerimentos de uma proposição no Senado
#' @examples
#' fetch_eventos_reqs_prop_senado(91341)
fetch_eventos_reqs_prop_senado <- function(prop_id) {
  print("fetch_eventos_reqs_prop_senado not yet implemented.")
  return(tibble::tibble())
  #reqs <- rcongresso::fetch_related_requerimentos_camara(prop_id = prop_id)
  #eventos_reqs <- purrr::map_df(reqs$id_req, ~rcongresso::fetch_events_requerimento_camara(.x)) %>%
  #  dplyr::select(-cod_situacao, -descricao_tramitacao, -regime, -uri_orgao, -id_req) %>%
  #  dplyr::rename(texto_tramitacao = despacho,
  #                sigla_local = sigla_orgao,
  #                id_situacao = cod_tipo_tramitacao,
  #                link_inteiro_teor = url) %>%
  #  dplyr::mutate(data_hora = lubridate::ymd_hm(stringr::str_replace(data_hora,'T','')),
  #                local = sigla_local,
  #                prop_id = prop_id,
  #                casa = congresso_constants$senado_label,
  #                id_situacao = as.integer(id_situacao))
  #
  #return(eventos_reqs)
}
