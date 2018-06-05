library(rcongresso)
library(tidyverse)
library(dplyr)
library(magrittr)
library(stringr)
library(here)

data_path <- here('data/')

pl6726_id <- fetch_id_proposicao(tipo = "PL", numero = 6726, ano = 2016)
tramitacao_pl_6726 <- fetch_tramitacao(id_prop = pl6726_id)

csv_path <- paste(c(data_path,'tramitacao_camara_', pl6726_id, '.csv'),  collapse = '')

phase_one <- c(100, 500)
phase_two <- c(320)
phase_three <- c(924)
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

rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

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



detect_event <- function(text, exp) {
  str_detect(tolower(text), exp)
}

extract_event <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(evento = case_when(detect_event(id_tipo_tramitacao, special_commission) ~ 'criacao_comissao_temporaria',
                              detect_event(despacho, public_hearing_requirement_exp) ~ 'requerimento_audiencia_publica',
                              detect_event(despacho, approved_public_hearing_requirement_exp) ~ 'aprovacao_audiencia_publica',
                              detect_event(despacho, approved_opinion_exp) ~ 'aprovacao_parecer',
                              detect_event(despacho, redistribution_requirement_exp) ~ 'requerimento_redistribuicao',
                              detect_event(despacho, apensacao_requirement_exp) ~ 'requerimento_apensacao',
                              detect_event(despacho, urgence_requirement_exp) ~ 'requerimento_urgencia',
                              detect_event(despacho, extends_deadline_exp) ~ 'requerimento_prorrogacao'))
}

tramitacao_pl_6726 %<>% rename_df_columns %>% extract_phase
tramitacao_pl_6726 %<>% fill(fase)
tramitacao_pl_6726 <- extract_event(tramitacao_pl_6726)
readr::write_csv(tramitacao_pl_6726, csv_path)