library(rcongresso)
library(tidyverse)
library(dplyr)
library(magrittr)
library(fuzzyjoin)


regexes <- 
  frame_data(~ status, ~ regex,
           "indeferido", '^Indefiro',
           "deferido", '^Defiro')

fetch_requerimentos <- function(id, mark_status=TRUE) {
  relacionadas <- 
    fetch_relacionadas(id)$uri %>% 
    strsplit('/') %>% 
    vapply(last, '') %>% 
    unique %>% 
    fetch_proposicao
  
  requerimentos <- 
    relacionadas %>% 
    filter(str_detect(.$descricaoTipo, '^Requerimento'))
  
  if(!mark_status) return(requerimentos)
  
  tramitacoes <- 
    requerimentos$id %>% 
    fetch_tramitacao
  
  relacionadas <-
    tramitacoes %>% 
    # mark tramitacoes rows based on regexes
    regex_left_join(regexes, by=c(despacho="regex")) %>% 
    group_by(id_prop) %>% 
    # fill down marks
    fill(status) %>%
    # get last mark on each tramitacao
    do(tail(., n=1)) %>%
    ungroup %>% 
    select(id_prop, status) %>% 
    # and mark proposicoes based on last tramitacao mark
    left_join(relacionadas, by=c('id_prop' = 'id'))
}

