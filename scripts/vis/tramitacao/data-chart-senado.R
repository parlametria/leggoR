library(tidyverse)
library(here)

# Create data frame to display local inline
format_local <- function(df) {
  
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
    mutate(group = "Fase")
  
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

format_fase_global <- function(bill_id, data_tramitacao) {
  data_prop <- read_csv(paste0(here::here("data/Senado/"), bill_id,"-proposicao-senado.csv"))
  casa_origem <- if_else(data_prop$nome_casa_origem == "Senado Federal", "Tramitação - Casa de Origem", "Tramitação - Casa Revisora")
  end <- 
    data_tramitacao %>%
    arrange(desc(data_tramitacao)) %>%
    select(data_tramitacao)
  
  virada_de_casa <- 
    data_tramitacao %>%
    filter(fase == "virada_de_casa") %>%
    arrange(data_tramitacao) %>%
    select(data_tramitacao)
  
  if(nrow(virada_de_casa) == 0) {
    frame_data(~ label, ~ start, ~ end, ~ time_interval, ~ group,  ~ color, 
                casa_origem, data_prop$data_apresentacao, end[1, ][[1]], 0, 'global', "#f37340")
  }else {
    casa_atual <- if_else(casa_origem == "Tramitação - Casa de Origem", "Tramitação - Casa Revisora", "Tramitação - Casa de Origem")
    frame_data(~ label, ~ start, ~ end, ~ time_interval, ~ group,  ~ color, 
               casa_origem, data_prop$data_apresentacao, virada_de_casa[1, ][[1]], 0, 'global', "#f37340",
               casa_atual, virada_de_casa[1, ][[1]], end[1, ][[1]], 0, 'global', "#546452")
  }
}

build_vis_csv <- function(bill_id) {
  data_tramitacao <- read_csv(paste0(here::here("data/Senado/"), bill_id,"-visualizacao-tramitacao-senado.csv"))
  
  rbind(format_local(data_tramitacao), format_fase(data_tramitacao), format_eventos(data_tramitacao), format_fase_global(bill_id, data_tramitacao)) %>%
    write_csv(paste0(here::here("data/vis/tramitacao/"), bill_id, "-data-senado.csv"))
}