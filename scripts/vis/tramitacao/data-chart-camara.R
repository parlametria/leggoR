library(data.table)
library(tidyverse)
library(lubridate)
library(here)
source(here::here("R/camara-lib.R"))

#' @title Adiciona a fase para o vistime
#' @description Adiciona o label fase com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' read_csv(paste0(here::here('data/camara/tramitacao-camara-'), bill_id, '.csv')) %>% data_fase()
#' @export
data_fase <- function(df) {
  df %>%
    mutate(end_data = lead(data_hora, default=Sys.time())) %>%
    group_by(fase, sequence = rleid(fase)) %>%
      summarise(start = min(data_hora),
              end = max(end_data)) %>%
      ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    rename(label = fase) %>%
    mutate(group = "Fase",
           color = case_when(label == "iniciativa" ~ "#7fc97f",
                             label == "relatoria" ~ "#fdc086",
                             label == "discussao_deliberacao" ~ "#beaed4",
                             label == "virada_de_casa" ~ "#ffff99",
                             label == "final" ~ "#f4fa58"))
}

#' @title Adiciona o local para o vistime
#' @description Adiciona o label local com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' read_csv(paste0(here::here('data/camara/tramitacao-camara-'), bill_id, '.csv')) %>% data_local()
#' @export
data_local <- function(df) {
  df <-
    df %>%
    filter((!(grepl('^S', sigla_orgao) | sigla_orgao == 'MESA')))

  df %>%
    mutate(end_data = lead(data_hora, default=Sys.time())) %>%
    group_by(local, sequence = rleid(local)) %>%
    summarise(start = min(data_hora),
              end = max(end_data)) %>%
    filter(end - start > 0) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    rename(label = local) %>%
    mutate(group = "Local",
           color = case_when(label == "Plenário" ~ "#5496cf",
                             label == "PL672616" ~ "#ff9c37",
                             label == "CCP" ~ "#8bca42",
                             label == "CTASP" ~ "#ea81b1"))
}

#' @title Adiciona o evento para o vistime
#' @description Adiciona o label evento com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' read_csv(paste0(here::here('data/camara/tramitacao-camara-'), bill_id, '.csv')) %>% data_evento()
#' @export
data_evento <- function(df) {
  df %>%
    filter(!is.na(evento)) %>%
    mutate(start = data_hora, end = data_hora, group = 'Evento') %>%
    group_by(evento) %>%
    rename(label = evento) %>%
    select(label, start, end, group) %>%
    mutate(color = '#a9a9a9') %>%
    unique()
}

#' @title Adiciona a local para o vistime
#' @description Adiciona o label local com suas respectivas cores no formato
#' suportado pelo vistime
#' @param bill_id id da proposição
#' @param proposicao Dataframe com a proposição
#' @param tramitacao Dataframe com a tramitacao
#' @examples
#' read_csv(paste0(here::here('data/camara/tramitacao-camara-'), bill_id, '.csv')) %>% format_fase_global()
#' @export
format_fase_global <- function(bill_id, proposicao, tramitacao) {
  data_prop <- extract_autor_in_camara(bill_id) %>% tail(1)
  casa_origem <- if_else(data_prop$casa_origem == "Câmara dos Deputados", "Tramitação - Casa de Origem", "Tramitação - Casa Revisora")
  tramitacao <- 
    tramitacao %>%
    mutate(end_data = lead(data_hora, default=Sys.time()))
  
  end <- 
    tramitacao %>%
    arrange(desc(end_data)) %>%
    select(end_data)
  
  virada_de_casa <- 
    tramitacao %>%
    filter(fase == "virada_de_casa") %>%
    arrange(data_hora) %>%
    select(data_hora)
  
  if(nrow(virada_de_casa) == 0) {
    frame_data(~ label, ~ start, ~ end, ~ time_interval, ~ group,  ~ color, 
               casa_origem, proposicao$data_apresentacao, end[1, ][[1]], 0, 'global', "#f37340")
  }else {
    casa_atual <- if_else(casa_origem == "Tramitação - Casa de Origem", "Tramitação - Casa Revisora", "Tramitação - Casa de Origem")
    frame_data(~ label, ~ start, ~ end, ~ time_interval, ~ group,  ~ color, 
               casa_origem, proposicao$data_apresentacao, virada_de_casa[1, ][[1]], 0, 'global', "#f37340",
               casa_atual, virada_de_casa[1, ][[1]], end[1, ][[1]], 0, 'global', "#546452")
  }
}

#' @title Formata tabela para o vistime
#' @description Formata a tabela final que será usado para fazer a visualização
#' usando o vistime
#' @param bill_id id da proposição
#' @examples
#' build_vis_csv(2121442)
#' @export
build_vis_csv <- function(bill_id) {
  tramitacao <- read_csv(paste0(here::here('data/camara/tramitacao-camara-'), bill_id, '.csv'))
  proposicao <- read_csv(paste0(here::here('data/camara/proposicao-camara-'), bill_id, '.csv'))
  
  data_path <- here::here('data/vis/tramitacao/')
  file_path <- paste0(data_path, bill_id, '-data-camara.csv')

  data <- bind_rows(data_evento(tramitacao), data_fase(tramitacao), data_local(tramitacao), format_fase_global(bill_id, proposicao %>% tail(1), tramitacao))
  readr::write_csv(data, file_path)
}
