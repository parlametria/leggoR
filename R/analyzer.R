library(rcongresso)
library(magrittr)
library(lubridate)
library(fuzzyjoin)
library(tidyverse)
source(here::here("R/camara-lib.R"))
source(here::here("R/senado-analyzeR.R"))
source(here::here("R/fetcher.R"))
source(here::here("R/congresso-lib.R"))

process_proposicao <- function(id, casa) {
  if ("CAMARA" == toupper(casa)) {
    process_proposicao_camara(id)
  } else if ("SENADO" == toupper(casa)) {
    senado_env <-
      jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
    senado_constants <- senado_env$constants
    process_proposicao_senado(id)
  }
}

#' @title Processa dados de uma proposição do senado.
#' @description Recebido um bill_id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/Senado.
#' @param bill_id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' process_proposicao_senado(91341)
#' @export
process_proposicao_senado <- function(bill_id) {
  bill_passage <-
    read_csv(paste0(
      here::here("data/Senado/"),
      bill_id,
      "-tramitacao-senado.csv"
    )) %>% arrange(data_tramitacao)
  
  phase_one <- c('^Este processo contém')
  recebimento_phase <- 'recebido na|nesta comissão'
  phase_two <- c(91)
  phase_three <- c(42, 14, 78, 90)
  encaminhamento_phase <- c(89, 158, 159, 160, 161, 162, 163)
  phase_four <- c(52)
  
  bill_passage <-
    extract_fase_Senado(
      bill_passage,
      phase_one,
      recebimento_phase,
      phase_two,
      phase_three,
      encaminhamento_phase,
      phase_four
    ) %>%
    arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    fill(fase)
  
  bill_passage$situacao_descricao_situacao <-
    to_underscore(bill_passage$situacao_descricao_situacao) %>%
    str_replace_all("\\s+", "_")
  
  bill_passage <-
    extract_fase_casa_Senado(bill_passage, phase_one) %>%
    arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    fill(casa) %>%
    filter(!is.na(casa))
  
  bill_passage <- extract_evento_Senado(bill_passage)
  
  index_of_camara <-
    ifelse(
      length(which(
        bill_passage$situacao_codigo_situacao == 52
      )) == 0,
      nrow(bill_passage),
      which(bill_passage$situacao_codigo_situacao == 52)[1]
    )
  bill_passage <-
    bill_passage[1:index_of_camara,] %>%
    extract_locais() %>%
    extract_fase_global(bill_id) %>%
    filter(!is.na(fase))
  
  bill_passage %>%
    write_csv(paste0(
      here::here("data/Senado/"),
      bill_id,
      "-fases-tramitacao-senado.csv"
    ))
  
  bill_passage_visualization <-
    bill_passage %>%
    select(data_tramitacao, local, fase, evento, casa, global)
  
  # Print evento freq table
  bill_passage_visualization %>% select(evento) %>% group_by(evento) %>%
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>%
    arrange(-frequência)
  
  
  bill_passage_visualization %>%
    write_csv(paste0(
      here::here("data/Senado/"),
      bill_id,
      "-visualizacao-tramitacao-senado.csv"
    ))
}

#' @title Processa dados de um proposição da câmara.
#' @description Recebido um pl_id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/camara.
#' @param pl_id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' process_proposicao_camara(257161)
#' @export
process_proposicao_camara <- function(pl_id) {
  data_path <- here::here('data/camara/')
  tramitacao_pl <- rcongresso::fetch_tramitacao(pl_id)
  
  csv_path <-
    paste(c(data_path, 'tramitacao-camara-', pl_id, '.csv'),  collapse = '')
  proposicao_csv_path <-
    paste(c(data_path, 'proposicao-camara-', pl_id, '.csv'),  collapse = '')
  
  # Extract phases, events and writh CSV
  tramitacao_pl %<>%
    rename_df_columns %>%
    extract_events_in_camara() %>%
    extract_locais_in_camara() %>%
    extract_fase_casa_in_camara() %>%
    extract_situacao_comissao() %>%
    refact_date() %>%
    sort_by_date() %>%
    readr::write_csv(csv_path)
  
  # Print evento freq table
  tramitacao_pl %>% select(evento) %>% group_by(evento) %>%
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>%
    arrange(-frequência)
  
  proposicao_pl <-
    fetch_proposicao_renamed(pl_id)
  
  data.frame(lapply(proposicao_pl, as.character), stringsAsFactors = FALSE) %>%
    readr::write_csv(proposicao_csv_path)
  
  relatorias <-
    extract_relatorias_in_camara(as.data.frame(read_csv(csv_path)))
  
  tramitacao_pl
}

#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao_camara(id) %>%
    rename_df_columns
  
  df[,!sapply(df, is.list)]
}

extract_evento_in_camara <- function(df) {
  camara_codes <- get_environment_camara_json()
  eventos <- camara_codes$eventos
  novo_despacho_regex <- eventos$regex$novo_despacho
  redistribuicao_regex <- eventos$regex$redistribuicao
  redistribuicao_text <- eventos$text$distribuicao %>% tolower()
  df %>%
    mutate(evento =
             case_when((str_detect(
               tolower(despacho),
               regex(redistribuicao_regex, ignore_case = TRUE)
             ) |
               str_detect(
                 tolower(despacho),
                 regex(novo_despacho_regex, ignore_case = TRUE)
               )) &
               tolower(descricao_tramitacao) == redistribuicao_text ~ "redistribuicao"
             ))
}
