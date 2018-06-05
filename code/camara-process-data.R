library(rcongresso)
library(tidyverse)
library(dplyr)
library(magrittr)

pl6726_id <- fetch_id_proposicao(tipo = "PL", numero = 6726, ano = 2016)
tramitacao_pl_6726 <- fetch_tramitacao(id_prop = pl6726_id)

phase_one <- c(100, 500)
phase_two <- c(320)
phase_three <- c(924)
phase_four <- c(128)
phase_five <- c(502, 251)

detect_phase <- function(text, exp) {
    text %in% exp
}

extract_phase <- function(dataframe) {
  dataframe %<>%
    mutate(fase = case_when(detect_phase(id_tipo_tramitacao, phase_one) ~ 'iniciativa',
                            detect_phase(id_tipo_tramitacao, phase_two) ~ 'relatoria',
                            detect_phase(id_situacao, phase_three) ~ 'discussao_deliberacao',
                            detect_phase(id_tipo_tramitacao, phase_four) ~ 'virada_de_casa',
                            detect_phase(id_tipo_tramitacao, phase_five) ~ 'final',
                            detect_phase(id_situacao, 937) ~ 'final'))
}

to_underscore <- function(x) {
  x %<>%
    gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", .) %>%
    gsub(".", "_", ., fixed = TRUE) %>%
    gsub("([a-z])([A-Z])", "\\1_\\2", .) %>%
    tolower
}

rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

tramitacao_pl_6726 %<>% rename_df_columns %>% extract_phase
tramitacao_pl_6726 %<>% fill(fase)
