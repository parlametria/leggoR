library(rcongresso)
library(tidyverse)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(fuzzyjoin)
source(here::here("code/camara-lib.R"))

data_path <- here::here('data/camara/')

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

importants_events <- 
  frame_data(~ evento, ~ regex,
           "requerimento_audiencia_publica", '^apresentação do requerimento.*requer a realização d.* audiências? públicas?',
           "aprovacao_audiencia_publica", '^aprovado requerimento.*requer a realização d.* audiências? públicas?',
           "aprovacao_parecer", 'aprovado.*parecer',
           "requerimento_redistribuicao", '^apresentação do requerimento de redistribuição',
           "requerimento_apensacao", '^apresentação do requerimento de apensação',
           "requerimento_urgencia", '^apresentação do requerimento de urgência', 
           "requerimento_prorrogacao", '^apresentação do requerimento de prorrogação de prazo de comissão temporária')

# Extract phases, events and writh CSV
tramitacao_pl_6726 %<>%
  rename_df_columns %>%
  extract_phases_Camara(phase_one, phase_two, phase_three, phase_four, phase_five) %>%
  fill(fase) %>%
  extract_events_Camara(importants_events) %>%
  refact_date() %>% 
  sort_by_date() %>%
  readr::write_csv(csv_path)

# Print evento freq table
tramitacao_pl_6726 %>% select(evento) %>% group_by(evento) %>% 
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>% 
    arrange(-frequência)

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

