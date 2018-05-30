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
    mutate(phase = case_when(detect_phase(descricaoTramitacao, phase_one) ~ 'iniciativa',
                             detect_phase(descricaoTramitacao, phase_two) ~ 'relatoria',
                             detect_phase(descricaoSituacao, phase_three) ~ 'discussao_deliberacao',
                             detect_phase(descricaoSituacao, phase_four) ~ 'virada_de_casa'))
}

tramitacao_pl_6726 <- extract_phase(tramitacao_pl_6726)
