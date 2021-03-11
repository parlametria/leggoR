library(tidyverse)
library(perfilparlamentar)

#' @title Processa o Disciplina para o Parlametria
#' @description Recupera informações de disciplina partidária
#' para o parlametria usando o pacote perfilparlamentar
#' @param votos_datapath Caminho para o csv de votos.
#' @param orientacoes_datapath Caminho para o csv de orientações
#' @return Dataframe de parlamentares e a disciplina calculada.
#' @example
#' disciplina <- processa_disciplina(votos_datapath, orientacoes_datapath)
processa_disciplina <- function(votos_datapath, orientacoes_datapath) {
  votos <- read_csv(votos_datapath)
  orientacoes <- read_csv(orientacoes_datapath)
  
  disciplina <- perfilparlamentar::processa_disciplina_partidaria(votos, orientacoes)
  
  return(disciplina)
}
