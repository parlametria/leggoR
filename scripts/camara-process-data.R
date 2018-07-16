args = commandArgs(trailingOnly=TRUE)
library(rcongresso)
library(tidyverse)
library(magrittr)
library(lubridate)
library(fuzzyjoin)
source(here::here("R/camara-lib.R"))


#' @title Processa dados de um proposição da câmara.
#' @description Recebido um pl_id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/camara.
#' @param pl_id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' process_proposicao(257161)
#' @export
process_proposicao <- function(pl_id) {
  data_path <- here::here('data/camara/')
  tramitacao_pl <- rcongresso::fetch_tramitacao(pl_id)

  csv_path <- paste(c(data_path,'tramitacao-camara-', pl_id, '.csv'),  collapse = '') 
  proposicao_csv_path <- paste(c(data_path,'proposicao-camara-', pl_id, '.csv'),  collapse = '')

  # Extract phases, events and writh CSV
  tramitacao_pl %<>%
    rename_df_columns %>%
    extract_phases_in_camara() %>%
    fill(fase) %>%
    extract_events_in_camara() %>%
    extract_locais_in_camara() %>%
    extract_fase_casa_in_camara() %>%
    refact_date() %>%
    sort_by_date() %>%
    readr::write_csv(csv_path)

  # Print evento freq table
  tramitacao_pl %>% select(evento) %>% group_by(evento) %>%
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>%
    arrange(-frequência)

  proposicao_pl <-
    fetch_proposicao_renamed(pl_id)
  
  data.frame(lapply(proposicao_pl, as.character), stringsAsFactors=FALSE) %>%
  readr::write_csv(proposicao_csv_path)

  relatorias <- extract_relatorias_in_camara(as.data.frame(read_csv(csv_path)))
 
  tramitacao_pl
}

#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao_in_camara(id) %>%
    rename_df_columns
  
  df[, !sapply(df, is.list)]
}

