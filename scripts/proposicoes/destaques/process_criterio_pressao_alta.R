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

#' @title Checa e processa o critério com pressão alta
#' @description Checa se o arquivo de pressão existe antes
#' de processar o critério de pressão alta das proposições
#' @param pressao_datapath Datapath de pressão
#' @return Dataframe contendo as proposições que passam no critério,
#' com a informação da pressão em cada uma. Caso o csv de pressão não exista,
#' será retornado um dataframe vazio.
check_and_process_criterio_pressao_alta <-
  function(pressao_filepath) {
    if (file.exists(pressao_filepath)) {
      proposicoes_pressao_alta <-
        process_criterio_pressao_alta(pressao_filepath) %>%
        mutate(criterio_pressao_alta = T) %>%
        select(id_leggo, criterio_pressao_alta, maximo_pressao_periodo)
      
      return(proposicoes_pressao_alta)
    } else {
      print("O csv de pressão não existe. Veja como gerá-lo no leggo-geral.")
      return(
        tibble(
          id_leggo = character(),
          criterio_pressao_alta = logical(),
          maximo_pressao_periodo = numeric()
        )
      )
    }
  }
