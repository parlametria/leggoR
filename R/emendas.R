source(here::here("R/utils.R"))

#' @title Retorna as emendas de uma proposição no Congresso
#' @description Retorna dataframe com os dados das emendas de uma proposição no Congresso.
#' @param bill_id ID de uma proposição do Congresso
#' @return Dataframe com as informações sobre as emendas de uma proposição no Congresso.
#' @examples
#' fetch_emendas(91341,'senado')
#' @export
fetch_emendas <- function(id, casa) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    emendas <- fetch_emendas_camara(id)
  } else if (casa == 'senado') {
    emendas <- rcongresso::fetch_emendas_senado(id)
  } else {
    print('Parâmetro "casa" não identificado.')
    return()
  }

  emendas  <-
    emendas %>%
    dplyr::mutate(prop_id = id, codigo_emenda = as.integer(codigo_emenda)) %>%
    dplyr::select(
      prop_id, codigo_emenda, data_apresentacao, numero, local, autor, casa, tipo_documento, inteiro_teor)
}

#' @title Retorna as emendas de uma proposição na Camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
#' @param id ID de uma proposição da Camara
#' @param sigla Sigla da proposição
#' @param numero Numero da proposição
#' @param ano Ano da proposição
#' @return Dataframe com as informações sobre as emendas de uma proposição na Camara
#' @examples
#' fetch_emendas_camara(408406)
fetch_emendas_camara <- function(id=NA, sigla="", numero="", ano="") {
  if(is.na(id)) {
    url <-
      paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal?tipo=', sigla, '&numero=', numero, '&ano=', ano)
  }else {
    prop <- fetch_proposicao(id, 'camara')
    url <-
      paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal?tipo=', prop$tipo_materia, '&numero=', prop$numero, '&ano=', prop$ano)
  }

  eventos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()

  df <-
    eventos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    magrittr::extract2('Emendas') %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame()

  if(nrow(df) == 0) {
    return(tibble::frame_data( ~ codigo_emenda, ~ data_apresentacao, ~ numero, ~ local, ~ autor, ~ casa, ~ tipo_documento, ~ inteiro_teor))
  }

  new_names <- c("cod_proposicao", "descricao")
  names(df) <- new_names

  emendas <- purrr::map_df(df$cod_proposicao, fetch_emendas_camara_auxiliar)
  normalizes_names <- c("codigo_emenda", "data_apresentacao", "numero", "local", "autor", "casa", "tipo_documento", "inteiro_teor")
  names(emendas) <- normalizes_names

  emendas %>%
    dplyr::mutate(data_apresentacao = as.character(as.Date(data_apresentacao)))
}

#' @title Função auxiliar para o fetch_emendas_camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
fetch_emendas_camara_auxiliar <- function(id) {
  fetch_proposicao(id, "camara", normalized = T, emendas = T) %>%
    dplyr::select(c(prop_id, data_apresentacao, numero, status_proposicao_sigla_orgao, autor_nome, casa, tipo_materia, ementa))
}
