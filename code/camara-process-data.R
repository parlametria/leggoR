args = commandArgs(trailingOnly=TRUE)
library(rcongresso)
library(tidyverse)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(fuzzyjoin)
source(here::here("code/camara-lib.R"))

process_project <- function(pl_id = 2121442) {
  data_path <- here::here('data/camara/')
  tramitacao_pl <- fetch_tramitacao(id_prop = pl_id)
  
  csv_path <- paste(c(data_path,'tramitacao_camara_', pl_id, '.csv'),  collapse = '')
  proposicao_csv_path <- paste(c(data_path,'proposicao_camara_', pl_id, '.csv'),  collapse = '')
  
  phase_one <- c(100, 500)
  phase_two <- c(320)
  phase_three <- c(322)
  phase_four <- c(128)
  phase_five <- c(502, 251)
  
  important_events <- frame_data(~ evento, ~ regex,
             "requerimento_audiencia_publica", '^apresentação do requerimento.*requer a realização d.* audiências? públicas?',
             "aprovacao_audiencia_publica", '^aprovado requerimento.*requer a realização d.* audiências? públicas?',
             "aprovacao_parecer", 'aprovado.*parecer',
             "requerimento_redistribuicao", '^apresentação do requerimento de redistribuição',
             "requerimento_apensacao", '^apresentação do requerimento de apensação',
             "requerimento_urgencia", '^apresentação do requerimento de urgência', 
             "requerimento_prorrogacao", '^apresentação do requerimento de prorrogação de prazo de comissão temporária')
  
  special_commission <- c('120')
  
  # Extract phases, events and writh CSV
  tramitacao_pl %<>%
    rename_df_columns %>%
    extract_phases_Camara(phase_one, phase_two, phase_three, phase_four, phase_five) %>%
    fill(fase) %>%
    extract_events_Camara(important_events, special_commission) %>%
    refact_date() %>% 
    sort_by_date() %>%
    readr::write_csv(csv_path)
  
  # Print evento freq table
  tramitacao_pl %>% select(evento) %>% group_by(evento) %>% 
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>% 
    arrange(-frequência)
 
  proposicao_pl <- 
    fetch_proposicao_renamed(pl_id) %>%
    readr::write_csv(proposicao_csv_path)
}

#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao(id) %>%
    rename_df_columns
  df
}

if(length(args) == 1){
  process_project(args[1])
} 

relatorias <- extract_relatorias_camara(as.data.frame(read_csv(csv_path)))
