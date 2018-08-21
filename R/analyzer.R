source(here::here("R/senado_analyzer.R"))
source(here::here("R/camara_analyzer.R"))

#' @title Processa dados de um proposição do congresso.
#' @description Recebido um id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/<camara/Senado>.
#' @param id Identificador da proposição que pode ser recuperado no site da câmara/senado.
#' @param casa Casa onde o PL está tramitando ('camara'/'senado').
#' @importFrom magrittr '%>%'
#' @examples
#' process_proposicao(257161,'camara')
#' @export
process_proposicao <- function(id, casa) {
  if ("CAMARA" == toupper(casa)) {
    agoradigital:::process_proposicao_camara(id)
  } else if ("SENADO" == toupper(casa)) {
    senado_env <-
      jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
    senado_constants <- senado_env$constants
    agoradigital:::process_proposicao_senado(id)
  }
}

#' @title Processa dados de um proposição do congresso.
#' @description Recebido um dataframe a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/<camara/Senado>.
#' @param tramitacao_df Dataframe com tramitação da proposição
#' @param casa Casa onde o PL está tramitando ('camara'/'senado').
#' @importFrom magrittr '%>%'
#' @export
process_proposicao <- function(proposicao_df, tramitacao_df, casa) {
  if ("CAMARA" == toupper(casa)) {
    process_proposicao_camara(proposicao_df = proposicao_df, tramitacao_df=tramitacao_df)
  } else if ("SENADO" == toupper(casa)) {
    process_proposicao_senado(proposicao_df = proposicao_df, tramitacao_df=tramitacao_df)
  }
}
