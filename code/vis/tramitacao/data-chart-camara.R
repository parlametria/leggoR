library(data.table)
library(dplyr)
library(lubridate)
library(here)

tramitacao <- read_csv('data/tramitacao_camara_2121442.csv')

data_path <- here::here('data/vis/tramitacao/')
file_path <- paste(c(data_path,'data-camara.csv'),  collapse = '')

data_fase <- function(df) {
  df %<>%
    mutate(end_data = lead(data_hora)) %>% 
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
  
  df$end[nrow(df)] <- ymd(Sys.Date())
  df
}

data_local <- function(df) {
  df %<>% 
    mutate(end_data = lead(data_hora)) %>%
    group_by(sigla_orgao, sequence = rleid(sigla_orgao)) %>%
    summarise(start = min(data_hora),
              end = max(end_data)) %>%
    ungroup() %>%
    arrange(sequence) %>% select(-sequence) %>%
    rename(label = sigla_orgao) %>%
    mutate(group = "Local",
           color = case_when(label == "iniciativa" ~ "#7fc97f",
                             label == "relatoria" ~ "#fdc086",
                             label == "discussao_deliberacao" ~ "#beaed4",
                             label == "virada_de_casa" ~ "#ffff99",
                             label == "final" ~ "#f4fa58"))
  
  df$end[nrow(df)] <- ymd(Sys.Date())
  df
}

data_evento <- function(df) {
  df %<>%
    group_by(evento) %>%
    filter(!is.na(evento)) %>% 
    rename(label = evento) %>%
    mutate(start = data_hora, end = data_hora, group = 'Evento') %>%
    select(label, start, end, group) %>% 
    mutate(color = 'Color')
  df
}

data <- bind_rows(data_evento(t), data_fase(t), data_local(t))
readr::write_csv(data, file_path)
