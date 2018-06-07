library(tidyverse)
library(here)
source(here("Documents/agora-digital/code/senado-lib.R"))

bill_id <- 127753

bill_passage <- read_csv(paste0("data/", bill_id, "-passage-senado.csv")) %>% arrange(data_tramitacao)

phase_one <- c('^Este processo contém')
phase_two <- c(91, 99)
phase_three <- c(42, 110, 88)
phase_four <- c(52)

detect_phase <- function(text, exp) {
  text %in% exp
}

extract_phase <- function(dataframe) {
  dataframe <- dataframe %>%
        mutate(fase = case_when( grepl(phase_one, texto_tramitacao) ~ 'iniciativa',
                                 detect_phase(situacao_codigo_situacao, phase_two) ~ 'relatoria',
                                 detect_phase(situacao_codigo_situacao, phase_three) ~ 'discussao_deliberacao',
                                 detect_phase(situacao_codigo_situacao, phase_four) ~ 'virada_de_casa'))
}

bill_passage <- 
  extract_phase(bill_passage) %>% 
  arrange(data_tramitacao, numero_ordem_tramitacao) %>%
  fill(fase)

bill_passage$situacao_descricao_situacao <- 
  to_underscore(bill_passage$situacao_descricao_situacao) %>% 
  str_replace_all("\\s+","_")


phase_aprovacao_audiencia <- 110
phase_aprovacao_parecer <- 89
phase_aprovacao_substitutivo <- 113

extract_event <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(evento = case_when( (situacao_codigo_situacao == phase_aprovacao_audiencia) ~ 'aprovacao_audiencia_publica',
                               (situacao_codigo_situacao == phase_aprovacao_parecer) ~ 'aprovacao_parecer',
                               (situacao_codigo_situacao == phase_aprovacao_substitutivo) ~ 'aprovacao_substitutivo',
                               TRUE ~ situacao_descricao_situacao))
}

bill_passage <- extract_event(bill_passage)

bill_passage_visualization <- 
  bill_passage %>%
  select(data_tramitacao, origem_tramitacao_local_sigla_local, fase, evento)

bill_passage_visualization %>%
  write_csv(paste0("data/", "bill-passage-", bill_id, "-visualization-senado.csv"))

