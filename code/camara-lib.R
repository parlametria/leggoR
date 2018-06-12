library(here)
source(here("code/congresso-lib.R"))

get_nome_ementa_Camara <- function(bill_id) {
  require(dplyr)
  require(rcongresso)
  
  fetch_proposicao(bill_id) %>% select(ementa, siglaTipo, numero)
}

tail_descricao_despacho_Camara <- function(df, qtd=1) {
  require(dplyr)
  
  df %>% 
    arrange(data_hora) %>% 
    tail(qtd) %>% 
    select(data_hora, descricao_tramitacao, despacho)
}

extract_phases_Camara <- function(dataframe, phase_one, phase_two, phase_three, phase_four, phase_five) {
  require(magrittr)
  
  dataframe %<>%
    mutate(fase = case_when(detect_phase(id_tipo_tramitacao, phase_one) ~ 'iniciativa',
                            detect_phase(id_tipo_tramitacao, phase_two) ~ 'relatoria',
                            detect_phase(id_tipo_tramitacao, phase_three) ~ 'discussao_deliberacao',
                            detect_phase(id_tipo_tramitacao, phase_four) ~ 'virada_de_casa',
                            detect_phase(id_tipo_tramitacao, phase_five) ~ 'final',
                            detect_phase(id_situacao, 937) ~ 'final'))
}

extract_n_last_events_Camara <- function(df, num) {
  require(tidyverse)
  
  df %>%
    filter(!is.na(evento)) %>%
    arrange(data_hora) %>%
    tail(n = num) %>%
    select(data_hora, evento)
}

rename_df_columns <- function(df) {
  require(magrittr)
  
  names(df) %<>% to_underscore
  df
}

extract_events_Camara <- function(tramitacao_df, events_df) {
  require(magrittr)
  require(tidyverse)
  
  tramitacao_df %<>%
    mutate(despacho_lower = tolower(despacho)) %>%
    regex_left_join(importants_events, by = c(despacho_lower = "regex")) %>%
    select(-c(despacho_lower, regex))
  tramitacao_df %<>%
    mutate(evento = case_when(id_tipo_tramitacao == special_commission ~ 'criacao_comissao_temporaria', 
                              TRUE ~ evento))
}

refact_date <- function(df) {
  require(lubridate)
  
  mutate(df, data_hora = ymd_hm(data_hora))
}

# sort the 'tramitacao' dataframe by date
sort_by_date <- function(df) {
  require(tidyverse)
  
  arrange(df, data_hora, sequencia)
}
