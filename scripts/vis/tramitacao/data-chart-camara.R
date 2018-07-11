library(data.table)
library(tidyverse)
library(lubridate)
library(here)

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


data_situacao_comissao <- function(df) {
  recebimento <- c(500)
  analise_do_relator <- c(320)
  discussao_votacao <- c(322, 240)
  encaminhamento <- c(180)
  
  reg <- 
    unlist(get_comissoes_camara()) %>%
    paste(collapse='|') %>%
    regex(ignore_case=TRUE)
  
  # Designa o tipo de situação para cada comissão
  df %<>%
    dplyr::mutate(
      local =
        dplyr::case_when(str_detect(local, reg) ~ str_extract(local, reg))
    ) %>% 
    dplyr::filter(!is.na(local)) %>% 
    dplyr::mutate(
      situacao_comissao =
        dplyr::case_when(id_tipo_tramitacao %in% recebimento ~ "Recebimento",
                         id_tipo_tramitacao %in% analise_do_relator ~ "Análise do relator",
                         id_tipo_tramitacao %in% discussao_votacao ~ "Discussão e votação",
                         id_tipo_tramitacao %in% encaminhamento ~ "Encaminhamento")
    ) %>%
    dplyr::select(data_hora, despacho, local, situacao_comissao) %>% 
    tidyr::fill(situacao_comissao)
  
  # Agrupamento
  df %>%
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

build_vis_csv <- function(bill_id) {
  tramitacao <- read_csv(paste0(here::here('data/camara/tramitacao-camara-'), bill_id, '.csv'))

  data_path <- here::here('data/vis/tramitacao/')
  file_path <- paste0(data_path, bill_id, '-data-camara.csv')

  data <- bind_rows(data_evento(tramitacao), data_fase(tramitacao), data_situacao_comissao(tramitacao), data_local(tramitacao))
  readr::write_csv(data, file_path)
}
