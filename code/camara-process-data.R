library(rcongresso)
library(tidyverse)
library(dplyr)
library(magrittr)
library(stringr)
library(htmlTable)
library(lubridate)
library(here)


data_path <- here('data/camara/')

pl6726_id <- fetch_id_proposicao(tipo = "PL", numero = 6726, ano = 2016)
tramitacao_pl_6726 <- fetch_tramitacao(id_prop = pl6726_id)

csv_path <- paste(c(data_path,'tramitacao_camara_', pl6726_id, '.csv'),  collapse = '')
proposicao_csv_path <- paste(c(data_path,'proposicao_camara_', pl6726_id, '.csv'),  collapse = '')

phase_one <- c(100, 500)
phase_two <- c(320)
phase_three <- c(322)
phase_four <- c(128)
phase_five <- c(502, 251)

special_commission <- c('120')
public_hearing_requirement_exp <- '^apresentação do requerimento.*requer a realização d.* audiências? públicas?'
approved_public_hearing_requirement_exp <- '^aprovado requerimento.*requer a realização d.* audiências? públicas?'
approved_opinion_exp <- 'aprovado.*parecer'
redistribution_requirement_exp <- '^apresentação do requerimento de redistribuição'
apensacao_requirement_exp <- '^apresentação do requerimento de apensação'
urgence_requirement_exp <- '^apresentação do requerimento de urgência'
extends_deadline_exp <- '^apresentação do requerimento de prorrogação de prazo de comissão temporária'

to_underscore <- function(x) {
  x %<>%
    gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", .) %>%
    gsub(".", "_", ., fixed = TRUE) %>%
    gsub("([a-z])([A-Z])", "\\1_\\2", .) %>%
    tolower
}


extract_phases <- function(dataframe) {
  dataframe %<>%
    mutate(fase = case_when(detect_phase(id_tipo_tramitacao, phase_one) ~ 'iniciativa',
                            detect_phase(id_tipo_tramitacao, phase_two) ~ 'relatoria',
                            detect_phase(id_tipo_tramitacao, phase_three) ~ 'discussao_deliberacao',
                            detect_phase(id_tipo_tramitacao, phase_four) ~ 'virada_de_casa',
                            detect_phase(id_tipo_tramitacao, phase_five) ~ 'final',
                            detect_phase(id_situacao, 937) ~ 'final'))
}

detect_event <- function(text, exp) {
  str_detect(tolower(text), exp)
}
rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

detect_phase <- function(text, exp) {
  text %in% exp
}

extract_events <- function(dataframe) {
  dataframe %<>%
    mutate(evento = case_when(detect_event(id_tipo_tramitacao, special_commission) ~ 'criacao_comissao_temporaria',
                              detect_event(despacho, public_hearing_requirement_exp) ~ 'requerimento_audiencia_publica',
                              detect_event(despacho, approved_public_hearing_requirement_exp) ~ 'aprovacao_audiencia_publica',
                              detect_event(despacho, approved_opinion_exp) ~ 'aprovacao_parecer',
                              detect_event(despacho, redistribution_requirement_exp) ~ 'requerimento_redistribuicao',
                              detect_event(despacho, apensacao_requirement_exp) ~ 'requerimento_apensacao',
                              detect_event(despacho, urgence_requirement_exp) ~ 'requerimento_urgencia',
                              detect_event(despacho, extends_deadline_exp) ~ 'requerimento_prorrogacao'))
}

refact_date <- function(df) {
  mutate(df, data_hora = ymd_hm(data_hora))
}

# sort the 'tramitacao' dataframe by date
sort_by_date <- function(df) {
  arrange(df, data_hora, sequencia)
}

# Extract phases, events and writh CSV
tramitacao_pl_6726 %<>%
  rename_df_columns %>%
  extract_phases %>%
  fill(fase) %>%
  extract_events %>%
  refact_date() %>% 
  sort_by_date() %>% 
  readr::write_csv(csv_path)

# Print evento freq table
tramitacao_pl_6726$evento %>%
  table %>%
  as.data.frame %>%
  arrange(desc(Freq)) %>%
  htmlTable(header=c('evento', 'frequência'), rnames=FALSE)

#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao(pl6726_id) %>%
    rename_df_columns
  df
}

proposicao_pl_6726 <- 
  fetch_proposicao_renamed(pl6726_id) %>%
  readr::write_csv(proposicao_csv_path)

