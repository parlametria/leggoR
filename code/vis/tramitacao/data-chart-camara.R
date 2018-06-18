args = commandArgs(trailingOnly=TRUE)
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
  df %>% 
    mutate(end_data = lead(data_hora, default=Sys.time())) %>%
    group_by(sigla_orgao, sequence = rleid(sigla_orgao)) %>%
    summarise(start = min(data_hora),
              end = max(end_data)) %>%
    filter(end - start > 0) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    rename(label = sigla_orgao) %>%
    mutate(group = "Local",
           color = case_when(label == "MESA" ~ "#E2E3D9",
                             label == "PLEN" ~ "#5496cf",
                             label == "PL672616" ~ "#ff9c37",
                             label == "CCP" ~ "#8bca42",
                             label == "SEPRO" ~ "#ca4242",
                             label == "CTASP" ~ "#ea81b1"))
}

data_evento <- function(df) {
  df %>%
    filter(!is.na(evento)) %>% 
    mutate(start = data_hora, end = data_hora, group = 'Evento') %>% 
    group_by(evento) %>%
    rename(label = evento) %>%
    select(label, start, end, group) %>%
    mutate(color = '#a9a9a9')
}

build_vis_csv <- function(bill_id = 2121442) {
  tramitacao <- read_csv(paste0(here::here('data/camara/tramitacao_camara_'), bill_id, '.csv'))
  
  data_path <- here::here('data/vis/tramitacao/')
  file_path <- paste0(data_path, bill_id, '-data-camara.csv')
  
  data <- bind_rows(data_evento(tramitacao), data_fase(tramitacao), data_local(tramitacao))
  readr::write_csv(data, file_path)
} 

if(length(args) == 1){
  build_vis_csv(args[1])
} 


