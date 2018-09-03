source(here::here("R/senado_analyzer.R"))
source(here::here("R/camara_analyzer.R"))

#' @title Processa dados de um proposição do congresso.
#' @description Recebido um dataframe a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/<camara/Senado>.
#' @param tramitacao_df Dataframe com tramitação da proposição
#' @param casa Casa onde o PL está tramitando ('camara'/'senado').
#' @importFrom magrittr '%>%'
#' @export
process_proposicao <- function(proposicao_df, tramitacao_df, casa, out_folderpath=NULL) {
  proc_tram_data <- NULL
  prop_id <- NULL
  if ("CAMARA" == toupper(casa)) {
    proc_tram_data <- process_proposicao_camara_df(proposicao_df = proposicao_df, tramitacao_df=tramitacao_df)
    prop_id <- proc_tram_data[1,"id_prop"]
  } else if ("SENADO" == toupper(casa)) {
    proc_tram_data <- process_proposicao_senado_df(proposicao_df = proposicao_df, tramitacao_df=tramitacao_df)
    prop_id <- proc_tram_data[1,"codigo_materia"]
  }
  
  if((!is.null(proc_tram_data)) & (!is.null(out_folderpath))) {
      readr::write_csv(proc_tram_data, paste0(out_folderpath,'/',casa,'/',prop_id,'-fases-tramitacao-',casa,'.csv'))
  }
  return(proc_tram_data)
}

#' @title Retorna energia de uma proposição no congresso.
#' @description Recebido o dataframe da tramitação contendo as colunas: data_hora e evento,
#' retorna um valor que indica a energia da proposição
#' @param tramitacao_df Dataframe da tramitação contendo as colunas: data_hora e evento
#' @param casa Casa onde o PL está tramitando ('camara'/'senado').
#' @param days_ago Quantidade de dias a serem analizados. O padrão é 30
#' @param pivot_day Dia de partida de onde começará a análise. O padrão é o dia atual
#' @return Energia de uma proposição.
#' @importFrom magrittr '%>%'
#' @export
get_energia <- function(tramitacao_df, casa, days_ago = 30, pivot_day = Sys.Date()) {
  if ("CAMARA" == toupper(casa)) {
    get_energia_camara(tramitacao_df, days_ago, pivot_day)
  } else if ("SENADO" == toupper(casa)) {
    get_energia_senado(tramitacao_df, days_ago, pivot_day)
  }
}
