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

  recebimento_phase <- c(500)
  phase_one <- c(100)
  phase_two <- c(320)
  phase_three <- c(335, 336, 420, 431)
  encaminhamento_phase <- c(180)
  phase_four <- c(128)
  phase_five <- c(502, 251)

  important_events <- frame_data(~ evento, ~ regex,
             "requerimento_audiencia_publica", '^apresentação do requerimento.*requer a realização d.* audiências? públicas?',
             "aprovacao_audiencia_publica", '^aprovado requerimento.*requer a realização d.* audiências? públicas?',
             "aprovacao_parecer", 'aprovado.*parecer',
             "requerimento_redistribuicao", '^apresentação do requerimento de redistribuição',
             "requerimento_apensacao", '^apresentação do requerimento de apensação',
             "requerimento_urgencia", '^apresentação do requerimento de urgência',
             "requerimento_prorrogacao", '^apresentação do requerimento de prorrogação de prazo de comissão temporária')

  special_commission <- c('120')

  # Extract phases, events and writh CSV
  tramitacao_pl %<>%
    rename_df_columns %>%
    extract_phases_in_camara(recebimento_phase, phase_one, phase_two, phase_three, encaminhamento_phase, phase_four, phase_five) %>%
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
    fetch_proposicao_camara(id) %>%
    rename_df_columns

  df[, !sapply(df, is.list)]
}

extract_evento_in_camara <- function(df) {
  camara_codes <- get_environment_camara_json()
  eventos <- camara_codes$eventos
  novo_despacho_regex <- eventos$regex$novo_despacho
  redistribuicao_regex <- eventos$regex$redistribuicao
  redistribuicao_text <- eventos$text$distribuicao %>% tolower()
  df %>%
    mutate(
      evento = 
        case_when((str_detect(tolower(despacho), regex(redistribuicao_regex, ignore_case = TRUE)) |
                     str_detect(tolower(despacho), regex(novo_despacho_regex, ignore_case = TRUE))) &
                  tolower(descricao_tramitacao) == redistribuicao_text ~ "redistribuicao"))
}
