library(tidyverse)

data <- read_csv("data/Senado/91341-bill-passage-visualization-senado.csv")

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
    mutate(color = case_when(grepl('^S', label) | label == 'ATA-PLEN' ~ "#e2e3d9"))
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
    filter(time_interval > 0) %>%
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

local_df <- format_local(data)

rbind(format_local(data), format_fase(data), format_eventos(data)) %>%
  write_csv("data/vis/tramitacao/data-senado.csv")
