library(here)
library(rcongresso)
library(tidyverse)
library(dplyr)
library(DescTools)
library(stringr)

pl6726_id <- fetch_id_proposicao(tipo = "PL", numero = 6726, ano = 2016)
tramitacao_pl_6726 <- fetch_tramitacao(id_prop = pl6726_id)

phase_one <- c('Recebimento', 'Apresentação de Proposição')
phase_two <- c('Designação de Relator')
phase_three <- c('Pronta para Pauta')
phase_four <- c('Enviada ao Senado Federal')
#TODO implements phase_five expressions.
phase_five <- c()

detect_phase <- function(text, exp) {
    text %in% exp
}

extract_phase <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(fase = case_when(detect_phase(descricao_tramitacao, phase_one) ~ 'iniciativa',
                             detect_phase(descricao_tramitacao, phase_two) ~ 'relatoria',
                             detect_phase(descricao_situacao, phase_three) ~ 'debate_deliberacao',
                             detect_phase(descricao_situacao, phase_four) ~ 'virada_casa'))
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

replace_phase_with_last <-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

tramitacao_pl_6726 <- rename_df_columns(tramitacao_pl_6726)
tramitacao_pl_6726 <- extract_phase(tramitacao_pl_6726)
tramitacao_pl_6726$fase <- replace_phase_with_last(tramitacao_pl_6726$fase)

