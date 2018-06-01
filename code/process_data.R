library(here)
library(rcongresso)
library(tidyverse)
library(dplyr)
library(DescTools)
library(stringr)

pl6726_id <- fetch_id_proposicao(tipo = "PL", numero = 6726, ano = 2016)
tramitacao_pl_6726 <- fetch_tramitacao(id_prop = pl6726_id)
proposicao_pl_6726 <- fetch_proposicao(id = pl6726_id)

phase_one <- c('Recebimento')
phase_two <- c('Designação de Relator')
phase_three <- c('Pronta para Pauta')
phase_four <- c('Enviada ao Senado Federal')
#TODO implements phase_five expressions.
phase_five <- c()

detect_phase <- function(text, exp) {
    str_detect(text, coll(exp, ignore_case = TRUE, locale = 'pt-br'))
}

extract_phase <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(phase = case_when(detect_phase(descricao_tramitacao, phase_one) ~ 1,
                             detect_phase(descricao_tramitacao, 'Apresentação de Proposição') ~ 1,
                             detect_phase(descricao_tramitacao, phase_two) ~ 2,
                             detect_phase(descricao_situacao, phase_three) ~ 3,
                             detect_phase(descricao_situacao, phase_four) ~ 4))
}

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

tramitacao_pl_6726 <- rename_df_columns(tramitacao_pl_6726)
tramitacao_pl_6726 <- extract_phase(tramitacao_pl_6726)

