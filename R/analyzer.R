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

#' @title Retorna energia de uma proposição
#' @description Recebido um id de uma proposição e a casa retorna um inteiro para indicar a energia
#' este inteiro é a soma dos eventos
#' @param bill_id Identificador deuma proposição.
#' @param house senado ou camara.
#' @return Energia de uma proposição.
#' @examples
#' get_energia(91341, 'senado')
#' @export
get_energia <- function(bill_id, house) {
  
  if (tolower(house) == 'senado') {
    data <- 
      readr::read_csv(paste0(here::here("data/vis/tramitacao/"), bill_id, "-data-senado.csv"))
  }else {
    data <-
      readr::read_csv(paste0(here::here("data/vis/tramitacao/"), bill_id, "-data-camara.csv"))
  }
  
  data %>%
    dplyr::filter(group == "Evento") %>%
    nrow()
}