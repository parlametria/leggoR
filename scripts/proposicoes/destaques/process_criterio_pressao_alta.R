library(tidyverse)
library(lubridate)

#' @title Processa o critério de seleção das proposição com pressão alta
#' @description Realiza o processamento para identificar e selecionar as proposições
#' com pressão alta
#' @param pressao_datapath Datapath de pressão
#' @return Dataframe contendo as proposições que passam no critério,
#' com a informação da pressão em cada uma
process_criterio_pressao_alta <- function(
  pressao_datapath = here::here("leggo_data/pressao.csv")) {

  .pressao_minima <- 0
  .limite_dias <- 90
  .hoje <- lubridate::ymd(Sys.Date())

  pressao <- read_csv(pressao_datapath)

  proposicoes_com_pressao <- pressao %>%
    mutate(dias = lubridate::interval(date, .hoje) %>% as.numeric('days')) %>%
    filter(dias <= .limite_dias, popularity > .pressao_minima) %>%
    group_by(id_leggo) %>%
    summarise(maximo_pressao_periodo = max(popularity)) %>%
    ungroup()

  return(proposicoes_com_pressao)
}
