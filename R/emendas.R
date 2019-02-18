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
  return(emendas)
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
fetch_emendas_camara <- function(id=NULL, sigla=NULL, numero=NULL, ano=NULL) {
  if(!is.null(id)) {
    prop <- fetch_proposicao(id, 'camara')
    sigla <- prop$tipo_materia
    numero <- prop$numero
    ano <- prop$ano
  }

  rcongresso::fetch_emendas_camara(sigla=sigla, numero=numero, ano=ano)
}
