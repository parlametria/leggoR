library(rcongresso)
library(tidyverse)
library(dplyr)
library(magrittr)
library(fuzzyjoin)


regexes <- 
  frame_data(~ deferimento, ~ regex,
           "indeferido", '^Indefiro',
           "deferido", '^(Defiro)|(Aprovado)')

fetch_requerimentos_relacionados <- function(id, mark_deferimento=TRUE) {
  relacionadas <- 
    fetch_relacionadas(id)$uri %>% 
    strsplit('/') %>% 
    vapply(last, '') %>% 
    unique %>% 
    fetch_proposicao
  
  requerimentos <- 
    relacionadas %>% 
    filter(str_detect(.$sigleTipo, '^REQ'))
  
  if(!mark_deferimento) return(requerimentos)
  
  tramitacoes <- 
    requerimentos$id %>% 
    fetch_tramitacao
  
  relacionadas <-
    tramitacoes %>% 
    # mark tramitacoes rows based on regexes
    regex_left_join(regexes, by=c(despacho="regex")) %>% 
    group_by(id_prop) %>% 
    # fill down marks
    fill(deferimento) %>%
    # get last mark on each tramitacao
    do(tail(., n=1)) %>%
    ungroup %>% 
    select(id_prop, deferimento) %>% 
    # and mark proposicoes based on last tramitacao mark
    left_join(relacionadas, by=c('id_prop' = 'id'))
}

