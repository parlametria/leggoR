library(here)
library(rcongresso)
library(tidyverse)
library(dplyr)
library(stringr)
library(rebus)

pl6726_id <- fetch_id_proposicao(tipo = "PL", numero = 6726, ano = 2016)
tramitacao_pl_6726 <- fetch_tramitacao(id_prop = pl6726_id)

phase_one <- c(100, 500)
phase_two <- c(320)
phase_three <- c(194)
phase_four <- c(128)
phase_five <- c(502, 251)

special_commission <- c('120')
public_hearing_requirement_exp <- '^apresentação do requerimento de audiência pública'
approved_public_hearing_requirement_exp <- '^pprovado requerimento.audiências? públicas?'


to_underscore <- function(x) {
  x2 <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x3 <- gsub(".", "_", x2, fixed = TRUE)
  x4 <- gsub("([a-z])([A-Z])", "\\1_\\2", x3)
  x5 <- tolower(x4)
  x5
}

rename_df_columns <- function(df) {
  new_names <- names(df) %>% to_underscore()
  names(df) <- new_names

  df
}

detect_phase <- function(text, exp) {
    text %in% exp
}

extract_phase <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(fase = case_when(detect_phase(id_tipo_tramitacao, phase_one) ~ 'iniciativa',
                            detect_phase(id_tipo_tramitacao, phase_two) ~ 'relatoria',
                            detect_phase(id_tipo_tramitacao, phase_three) ~ 'discussao_deliberacao',
                            detect_phase(id_tipo_tramitacao, phase_four) ~ 'virada_casa',
                            detect_phase(id_tipo_tramitacao, phase_five) ~ 'final',
                            detect_phase(id_situacao, 937) ~ 'final'))
}

replace_phase_with_last <-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

detect_event <- function(text, exp) {
  str_detect(tolower(text), exp)
}

extract_event <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(evento = case_when(detect_event(id_tipo_tramitacao, special_commission) ~ 'criacao_comissao_temporaria',
                              detect_event(despacho, public_hearing_requirement_exp) ~ 'requerimento_audiencia_publica',
                              detect_event(despacho, approved_public_hearing_requirement_exp) ~ 'aprovacao_audiencia_publica'))
}


tramitacao_pl_6726 <- rename_df_columns(tramitacao_pl_6726)
tramitacao_pl_6726 <- extract_phase(tramitacao_pl_6726)
tramitacao_pl_6726$fase <- replace_phase_with_last(tramitacao_pl_6726$fase)
tramitacao_pl_6726 <- extract_event(tramitacao_pl_6726)
