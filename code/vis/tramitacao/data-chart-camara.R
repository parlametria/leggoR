library(data.table)
library(dplyr)
library(lubridate)

data_fase <- function(df) {
  df %<>%
    mutate(end_data = lead(data_hora)) %>% 
    group_by(fase, sequence = rleid(fase)) %>%
      summarise(start = min(data_hora),
              end = max(end_data)) %>% 
      ungroup() %>% 
    arrange(sequence) %>%
    select(-sequence) %>%
    setnames("fase", "label") %>% 
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
    setnames("sigla_orgao", "label") %>%
    mutate(group = "Local",
           color = case_when(label == "iniciativa" ~ "#7fc97f",
                             label == "relatoria" ~ "#fdc086",
                             label == "discussao_deliberacao" ~ "#beaed4",
                             label == "virada_de_casa" ~ "#ffff99",
                             label == "final" ~ "#f4fa58"))
  
  df$end[nrow(df)] <- ymd(Sys.Date())
  df
}
