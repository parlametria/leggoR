library(tidyverse)
library(perfilparlamentar)
library(pscl)

#' @title Processa o Governimo para o Parlametria
#' @description Recupera informações de governimo para o parlametria usando o pacote perfilparlamentar
#' @param votos_datapath Caminho para o csv de votos.
#' Deve ter pelo menos 4 colunas: id_votacao, casa, id_parlamentar, voto.
#' @return Dataframe com as PL's que serviram de entrada para o pipeline
#' contendo id_camara,id_senado,apelido,tema,advocacy_link,keywords,tipo_agenda
#' @example
#' tabela_pls <- processa_lista_pls_interesses(url)
processa_governismo <- function(votos_datapath) {
  votos <- read_csv(votos_datapath,
                    col_types = cols(
                      .default = col_character(),
                      voto = col_number()
                    ))

  governismo_camara <- perfilparlamentar::processa_governismo(votos %>% filter(casa == "camara")) %>%
    select(id_parlamentar, governismo = D1) %>%
    mutate(casa = "camara", governismo = -governismo)

  governismo_senado <- perfilparlamentar::processa_governismo(votos %>% filter(casa == "senado")) %>%
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

