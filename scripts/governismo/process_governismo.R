library(tidyverse)
library(perfilparlamentar)
library(pscl)

#' @title Processa o Governimo para o Parlametria
#' @description Recupera informações de governimo para o parlametria usando o pacote perfilparlamentar
#' @param votos_datapath Caminho para o csv de votos.
#' Deve ter pelo menos 4 colunas: id_votacao, casa, id_parlamentar, voto.
#' @return Dataframe de parlamentares e o governismo calculado.
#' @example
#' governismo <- processa_governismo(votos_datapath)
processa_governismo <- function(votos_datapath) {
  votos <- read_csv(votos_datapath,
                    col_types = cols(
                      .default = col_character(),
                      voto = col_number()
                    ))

  votos_camara <- votos %>%
    filter(casa == "camara") %>%
    distinct(id_votacao, id_parlamentar, .keep_all = TRUE)

  governismo_camara <- perfilparlamentar::processa_governismo(votos_camara) %>%
    select(id_parlamentar, governismo = D1) %>%
    mutate(casa = "camara", governismo = -governismo)

  votos_senado <- votos %>%
    filter(casa == "senado") %>%
    distinct(id_votacao, id_parlamentar, .keep_all = TRUE)

  governismo_senado <- perfilparlamentar::processa_governismo(votos_senado) %>%
    select(id_parlamentar, governismo = D1) %>%
    mutate(casa = "senado", governismo = -governismo)

  governismo_alt <- bind_rows(governismo_camara, governismo_senado) %>%
    group_by(casa) %>%
    mutate(governismo = scales::rescale(governismo, to = c(-10, 10))) %>%
    ungroup() %>%
    mutate(enum_casa = if_else(casa == "camara", 1, 2)) %>%
    mutate(id_parlamentar_parlametria = paste0(enum_casa, id_parlamentar)) %>%
    select(id_parlamentar, id_parlamentar_parlametria, casa, governismo)

  return(governismo_alt)
}

