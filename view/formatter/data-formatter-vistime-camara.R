library(data.table)
library(tidyverse)
library(lubridate)
library(here)
source(here::here("R/camara-lib.R"))

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
    mutate(group = "Comissão",
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

#' @title Adiciona a fase global para o vistime
#' @description Adiciona a fase global com suas respectivas cores no formato
#' suportado pelo vistime
#' @param bill_id id da proposição
#' @param tramitacao Dataframe com a tramitacao
#' @examples
#' read_csv(paste0(here::here('data/camara/tramitacao-camara-'), bill_id, '.csv')) %>% data_fase_global(bill_id, .)
#' @export
data_fase_global <- function(bill_id, tramitacao) {
  data_prop <- extract_autor_in_camara(bill_id) %>% tail(1)
  tipo_casa <- if_else(data_prop$casa_origem == "Câmara dos Deputados", "Origem", "Revisão")
  
  tramitacao %>%
    mutate(end_data = lead(data_hora, default=Sys.time())) %>%
    group_by(local, sequence = rleid(local)) %>%
    summarise(start = min(data_hora),
              end = max(end_data)) %>%
    filter(end - start > 0) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    rename(label = local) %>%
    mutate(group = "Global",
           label = case_when(label == "CD-MESA-PLEN" ~ "Apresentação",
                             TRUE ~ label),
           color = case_when(stringr::str_detect(label, "Plenário") ~ "#5496cf",
                             stringr::str_detect(label, "Comissão Especial") ~ "#ff9c37",
                             stringr::str_detect(label, "CCP") ~ "#8bca42",
                             stringr::str_detect(label, "CTASP") ~ "#ea81b1"),
           label = paste(label, '-', tipo_casa, '(Câmara)'))
}

data_situacao_comissao <- function(df) {
  df %>% 
  dplyr::select(data_hora, situacao_comissao) %>%
  dplyr::filter(!is.na(situacao_comissao)) %>%
  mutate(end_data = lead(data_hora, default=Sys.time())) %>%
  group_by(situacao_comissao, sequence = data.table::rleid(situacao_comissao)) %>%
  summarise(start = min(data_hora),
            end = max(end_data)) %>%
  filter(end - start > 0) %>%
  ungroup() %>%
  arrange(sequence) %>%
  select(-sequence) %>% 
  rename(label = situacao_comissao) %>% 
  mutate(group = "Situação na comissão",
         color = case_when(label == "Recebimento" ~ "#5496cf",
                           label == "Análise do relator" ~ "#ff9c37",
                           label == "Discussão e votação" ~ "#8bca42",
                           label == "Encaminhamento" ~ "#ea81b1"))
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

  data <- 
    bind_rows(data_fase_global(bill_id, tramitacao), 
                     data_local(tramitacao),
                    data_situacao_comissao(tramitacao), 
                    data_evento(tramitacao)) %>%
    filter(group != "Comissão")
  
  readr::write_csv(data, file_path)
}
