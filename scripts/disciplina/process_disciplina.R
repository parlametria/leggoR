library(tidyverse)
library(perfilparlamentar)

#' @title Processa o Disciplina para o Parlametria
#' @description Recupera informações de disciplina partidária
#' para o parlametria usando o pacote perfilparlamentar
#' @param votos_datapath Caminho para o csv de votos.
#' @param orientacoes_datapath Caminho para o csv de orientações.
#' @param votacoes_datapath Caminho para o csv de votações.
#' @param data_inicio Data inicial do recorte de tempo.
#' @param data_final Data final do recorte de tempo.
#' @return Dataframe de parlamentares e a disciplina calculada.
#' @example
#' disciplina <- processa_disciplina(votos_datapath, orientacoes_datapath, votacoes_datapath, data_inicio, data_final)
processa_disciplina <-
  function(votos_datapath,
           orientacoes_datapath,
           votacoes_datapath,
           data_inicio = "2019-02-01",
           data_final = "2022-12-31") {
  votacoes <- read_csv(votacoes_datapath) %>%
    filter(data >= data_inicio, data <= data_final)
  
  votos <- read_csv(votos_datapath) %>% 
    filter(id_votacao %in% (votacoes %>% pull(id_votacao)))
  
  orientacoes <- read_csv(orientacoes_datapath)
  
  disciplina <- perfilparlamentar::processa_disciplina_partidaria(votos, orientacoes)
  
  return(disciplina)
}
