library(tidyverse)
library(here)
library(lubridate)

#' @title Processa o critério de proposições com requerimento de urgência apresentado e/ou aprovado;
#' @description Processa o critério que retorna informações de proposições com requerimento de 
#' urgência aprovado ou apresentado;
#' @param proposicoes_datapath Datapath de proposições
#' @param trams_datapath Datapath de tramitações
#' @return Dataframe contendo as proposições que passam no critério.

process_criterio_requerimento_urgencia <- function(trams_datapath = here::here("leggo_data/trams.csv"),
                                                   props_datapath = here::here("leggo_data/proposicoes.csv") ){
  
  trams <- read_csv(trams_datapath, col_types = cols(id_ext = col_character())) %>%  
    select(evento, id_ext, casa, data)
  
  props <- read_csv(props_datapath, col_types = cols(id_ext = col_character())) %>%
    select(id_ext, casa, id_leggo)
  
 # Filtra eventos dos últimos 4 anos 
  hoje <- Sys.time()
  trams <- trams %>%
    mutate(idade = lubridate::interval(data, hoje) %>%
             as.numeric('years')) %>%
    dplyr::filter(idade <= 4)
  
  proposicoes_requerimento_urgencia <- trams %>% 
  left_join(props, by = c("id_ext", "casa")) %>%  
  filter(str_detect(evento, "requerimento_urgencia_apresentado|requerimento_urgencia_aprovado"))
  
  # remove as repetições de proposições 
  proposicoes_sem_repetidos <- proposicoes_requerimento_urgencia %>% arrange(id_leggo, id_ext, casa, data) %>% 
    distinct(id_leggo, id_ext, casa, .keep_all = TRUE)
  
  proposicoes_requerimento_urgencia_final <- proposicoes_sem_repetidos %>% spread(evento, data)
  
  return(proposicoes_requerimento_urgencia_final)
  
}