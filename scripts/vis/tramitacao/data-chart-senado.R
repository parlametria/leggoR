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
    mutate(group = "Sub-fase Comissão")
  
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
      start = as.Date(data_tramitacao),
      end = as.Date(data_tramitacao),
      label = as.character(evento)
    )
  
  df <- 
    df %>%
    filter(!is.na(label)) %>%
    select(-c(local, evento, fase, data_tramitacao, casa)) %>%
    mutate(group = "Evento", color = "#a9a9a9", time_interval = 0)
  
  df
}

#Create data to display global phase inlines
format_fase_global <- function(bill_id, data_tramitacao) {
  data_prop <- read_csv(paste0(here::here("data/Senado/"), bill_id,"-proposicao-senado.csv"))
  casa_origem <- if_else(data_prop$nome_casa_origem == "Senado Federal", "Tramitação - Casa de Origem (Senado)", "Tramitação - Casa Revisora (Câmara)")
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
               casa_origem, data_prop$data_apresentacao, virada_de_casa[1, ][[1]], 0, 'Global', "#f37340",
               casa_atual, virada_de_casa[1, ][[1]], end[1, ][[1]], 0, 'Global', "#546452")
  }
}

#Create data to display casa inlines
format_fase_casa <- function(df) {
  df <-
    # Improve the phases names and convert data_tramitacao to Date
    df %>%
    mutate(
      data_tramitacao = as.Date(data_tramitacao)
    )
  
  df %>%
    mutate(z = cumsum(casa != lag(casa, default='NULL')),
           end_data = lead(data_tramitacao)) %>%
    group_by(casa, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = if_else(is.na(max(end_data)),max(data_tramitacao),max(end_data)),
              time_interval = end - start) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    filter(!is.na(casa)) %>%
    rename(label=casa) %>%
    mutate(group = "Casa",
           color = case_when(label == "Plenário" ~ "#5496cf",
                             label == "Comissões" ~ "#938ecc",
                             label == "Apresentação" ~ "#d6952a"))
}


build_vis_csv <- function(bill_id) {
  data_tramitacao <- read_csv(paste0(here::here("data/Senado/"), bill_id,"-visualizacao-tramitacao-senado.csv"))
  
  eventos <- 
    data_tramitacao %>%
    filter(!is.na(evento))
  
  data_tramitacao <-
    data_tramitacao %>%
    filter(is.na(evento))
  rbind(format_eventos(eventos),
        format_fase_global(bill_id, data_tramitacao), 
        format_fase_casa(data_tramitacao),
        format_local(data_tramitacao), 
        format_fase(data_tramitacao)) %>%
    write_csv(paste0(here::here("data/vis/tramitacao/"), bill_id, "-data-senado.csv"))
}