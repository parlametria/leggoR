library(tidyverse)
library(perfilparlamentar)

#' @title Processa votações sumarizadas
#' @description Recupera informações de quantidade de votos
#' por parlamentar usando o pacote perfilparlamentar
#' @param votos_datapath Caminho para o csv de votos.
#' @param votacoes_datapath Caminho para o csv de votações.
#' @param data_inicio Data inicial do recorte de tempo.
#' @param data_final Data final do recorte de tempo.
#' @return Dataframe de parlamentares e número de votos
#' @example
#' votacoes_sumarizadas <- processa_votacoes_sumarizadas(votos_datapath, votacoes_datapath, data_inicio, data_final)
processa_votacoes_sumarizadas <-
  function(votos_datapath,
           votacoes_datapath,
           data_inicio = "2019-02-01",
           data_final = "2022-12-31") {
    votacoes <- read_csv(votacoes_datapath) %>%
      filter(data >= data_inicio, data <= data_final)
    
    votos <- read_csv(votos_datapath) %>% 
      filter(id_votacao %in% (votacoes %>% pull(id_votacao))) 
    
    votacoes_sumarizadas <- perfilparlamentar::processa_votacoes_sumarizadas(votos)
   
    data_votacoes <- votacoes %>%
      mutate(ultima_data = max(data)) %>% 
      select(ultima_data) %>%
      head(1)

    votacoes_sumarizadas <- votacoes_sumarizadas %>% 
      mutate(ultima_data_votacao = data_votacoes$ultima_data)
    
    return(votacoes_sumarizadas)
  }
