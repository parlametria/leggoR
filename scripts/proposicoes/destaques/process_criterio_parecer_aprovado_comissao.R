library(tidyverse)
library(here)

#' @title Processa critério de parecer aprovado em comissão.
#' @description Processa o critério que retorna informações de proposições que tiveram algum parecer
#' aprovado em uma comissão.
#' @param tramitacoes_datapath Caminho para o CSV de tramitações.
#' @return Dataframe com informações de proposições com parecer aprovado em comissões.
#' @examples
#' process_criterio_parecer_aprovado_comissao()
process_criterio_parecer_aprovado_comissao <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  tramitacoes_datapath = here::here("leggo_data/trams.csv")) {
  # tramitacoes <- read_csv("~/leggo_data/trams.csv")
  # proposicoes <- read_csv("~/leggo_data/proposicoes.csv")
  tramitacoes <- read_csv(tramitacoes_datapath,
                          col_types = cols(data_audiencia = col_datetime(),
                                           data = col_datetime(),
                                           sequencia = col_integer(),
                                           .default = col_character()))

  proposicoes <- read_csv(proposicoes_datapath,
                          col_types = cols(id_ext = col_character())) %>%
    select(id_ext, casa, id_leggo)

  ## Lista de locais que não são comissões
  locais_filtro <- c("mesa", "ata-plen", "plen", "seadi")

  eventos_agrupados <- tramitacoes %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::filter(!tolower(sigla_local) %in% locais_filtro) %>%
    group_by(id_ext, casa, sigla_local) %>%
    summarise(eventos = paste(evento, collapse = ";")) %>%
    ungroup()

  eventos_parecer_aprovado <- eventos_agrupados %>%
    dplyr::filter(str_detect(eventos, "aprovacao_parecer"),
                  !str_detect(eventos, "parecer_pela_rejeicao"))

  proposicoes_aprovadas <- eventos_parecer_aprovado %>%
    group_by(id_ext, casa) %>%
    summarise(comissoes_aprovadas = paste(sigla_local, collapse = ";")) %>%
    ungroup()

  proposicoes_alt <- proposicoes_aprovadas %>%
    left_join(proposicoes, by = c("id_ext", "casa")) %>%
    distinct() %>%
    select(id_leggo, id_ext, casa, comissoes_aprovadas)

  return(proposicoes_alt)
}
