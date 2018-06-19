library(tidyverse)
library(here)

# Create data frame to display local inline
format_local <- function(df) {
  locais_pouco_importantes <- c('SGM', 'SLSF','SSCLSF','ATA-PLEN', 'SEXPE')
  
  df <- 
    df %>%
    filter(!(grepl('^S', local) | local == 'ATA-PLEN')) %>%
    mutate(
      data_tramitacao = as.Date(data_tramitacao),
      local = as.character(local)
    )
  
  df <-
    df %>%
    mutate(z = cumsum(local != lag(local, default='NULL')), 
           end_data = lead(data_tramitacao)) %>%
    group_by(local, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = if_else(is.na(max(end_data)),max(data_tramitacao),max(end_data)),
              time_interval = end - start) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    filter(time_interval > 0) %>%
    rename(label=local) %>%
    mutate(group = "Local")

  df %>%
    mutate(color = case_when((grepl('^S', label) | 
                               label == 'ATA-PLEN') ~ "#e2e3d9",
                               label == 'PLEN' ~ "#5496cf",
                               label == 'CCJ' ~ "#ffffcc",
                               label == 'CAE' ~ "#a1dab4",
                               label == 'CDR' ~ "#225ea8"))
}

# Create data frame to display phases inline
format_fase <- function(df) {
  df <- 
    # Improve the phases names and convert data_tramitacao to Date
    df %>%
    mutate(
      data_tramitacao = as.Date(data_tramitacao),
      fase = as.character(fase)
    )
  
  df <- df %>%
    mutate(z = cumsum(fase != lag(fase, default='NULL')), 
           end_data = lead(data_tramitacao)) %>%
    group_by(fase, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = if_else(is.na(max(end_data)),max(data_tramitacao),max(end_data)),
              time_interval = end - start) %>%
    ungroup() %>% 
    arrange(sequence) %>% 
    select(-sequence) %>%
    #filter(time_interval > 0) %>%
    rename(label=fase) %>%
    mutate(group = "Sub-Fase")
  
  df %>% 
    mutate(color = case_when(label == "iniciativa" ~ "#7fc97f",
                           label == "relatoria" ~ "#fdc086",
                           label == "discussao_deliberacao" ~ "#beaed4",
                           label == "virada_de_casa" ~ "#ffff99",
                           label == "final" ~ "#f4fa58"))
}

#Create data to display events inlines
format_eventos <- function(df) {
  df <-
    # Improve the phases names and convert data_tramitacao to Date
    df %>%
    mutate(
      data_tramitacao = as.Date(data_tramitacao),
      evento = as.character(evento)
    )
  
  df <- 
    df %>%
    mutate(z = cumsum(evento != lag(evento, default='NULL')),
           end_data = lead(data_tramitacao)) %>%
    group_by(evento, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = start,
              time_interval = end - start) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    filter(!is.na(evento)) %>%
    rename(label=evento) %>%
    mutate(group = "Evento", color = "#a9a9a9")
  
}

build_vis_csv <- function(bill_id) {
  data <- read_csv(paste0(here::here("data/Senado/"), bill_id,"-bill-passage-visualization-senado.csv"))
  
  rbind(format_local(data), format_fase(data), format_eventos(data)) %>%
    write_csv(paste0(here::here("data/vis/tramitacao/"), bill_id, "-data-senado.csv"))
}