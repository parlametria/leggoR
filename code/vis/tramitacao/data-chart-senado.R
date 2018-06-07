library(tidyverse)

data <- read_csv("data/91341-bill-passage-visualization-senado.csv")

# Create data frame to display local inline
format_local <- function(df) {
  df <- 
    df %>%
    mutate(
      data_tramitacao = as.Date(data_tramitacao),
      local = as.character(origem_tramitacao_local_sigla_local)
    )
  
  df %>%
    mutate(z = cumsum(local != lag(local, default='NULL')), 
           end_data = lead(data_tramitacao)) %>%
    group_by(local, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = max(end_data),
              time_interval = end - start) %>%
    ungroup() %>% 
    arrange(sequence) %>% 
    select(-sequence) %>%
    filter(time_interval > 0) %>%
    rename(label=local) %>%
    mutate(group = "Local")
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
  
  df %>%
    mutate(z = cumsum(fase != lag(fase, default='NULL')), 
           end_data = lead(data_tramitacao)) %>%
    group_by(fase, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = max(end_data),
              time_interval = end - start) %>%
    ungroup() %>% 
    arrange(sequence) %>% 
    select(-sequence) %>%
    filter(time_interval > 0) %>%
    rename(label=fase) %>%
    mutate(group = "Fase")
}

rbind(format_local(data), format_fase(data)) %>%
  write_csv("data/vis/tramitacao/data-senado.csv")
