library(perfilparlamentar)
library(tidyverse)

source(here::here("scripts/votos/fetcher_votos.R"))
source(here::here("scripts/votos/utils.R"))

#' @title Baixa os votos na Câmara
#' @description Se receber um dataframe de votações, retorna os votos para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param votacoes_datapath Caminho para dataframe de votações
#' @param votos_datapath Caminho para dataframe de votos
#' @param proposicoes_datapath Caminho para dataframe de proposições
#' @param entidades_datapath Caminho para dataframe de entidades
processa_votos <- function(anos = c(2019, 2020),
                           votacoes_datapath = NULL,
                           votos_datapath = NULL,
                           proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
                           entidades_datapath = here::here("leggo_data/entidades.csv")) {

  new_votacoes_camara <- processa_votacoes_camara_anos(anos) %>%
    mutate(casa = "camara",
           id_proposicao = as.character(id_proposicao))

  new_votacoes_senado <- processa_votacoes_senado_anos(anos) %>%
    mutate(casa = "senado") %>%
    select(
      id_votacao = codigo_sessao,
      id_proposicao,
      data = datetime,
      obj_votacao = objeto_votacao,
      casa
    )

  votos_camara <- NULL
  votos_senado <- NULL


  votacoes_atuais <- read_votacoes(votacoes_datapath)

  if (nrow(votacoes_atuais) > 0) {

    votacoes_a_processar_camara <-
      anti_join(
        new_votacoes_camara,
        votacoes_atuais %>%
          filter(casa == "camara"),
        by = c("id_proposicao", "id_votacao")
      )

    votacoes_a_processar_senado <-
      anti_join(
        new_votacoes_senado,
        votacoes_atuais %>%
          filter(casa == "senado"),
        by = c("id_proposicao", "id_votacao")
      )

  } else {

    votacoes_a_processar_camara <- new_votacoes_camara
    votacoes_a_processar_senado <- new_votacoes_senado
  }

  proposicoes <- read_csv(proposicoes_datapath, col_types = cols(.default = "c")) %>%
    select(id_leggo, id_proposicao = id_ext, casa)

  votacoes_a_processar_camara <-
    left_join(votacoes_a_processar_camara, proposicoes, by = c("id_proposicao", "casa"))

  votacoes_a_processar_senado <-
    left_join(votacoes_a_processar_senado, proposicoes, by = c("id_proposicao", "casa"))

  if (nrow(votacoes_a_processar_camara) > 0) {
    votos_camara <-
      fetch_votos_camara(anos, votacoes_a_processar_camara) %>%
      perfilparlamentar::enumera_voto()
  }

  if (nrow(votacoes_a_processar_senado) > 0) {
    votos_senado <-
      fetch_votos_senado(anos, votacoes_a_processar_senado, entidades_datapath) %>%
      mutate(id_parlamentar = as.integer(id_parlamentar)) %>%
      select(-id_proposicao) %>%
      perfilparlamentar::enumera_voto()
  }

  votacoes_camara <-
    bind_rows(votacoes_atuais %>%
                filter(casa == "camara"),
              votacoes_a_processar_camara)

  votacoes_senado <-
    bind_rows(votacoes_atuais %>%
                filter(casa == "senado"),
              votacoes_a_processar_senado)

  votacoes <- votacoes_camara %>%
    bind_rows(votacoes_senado)

  votacoes <- votacoes %>%
    mutate(data = gsub("T", " ", data)) %>%
    distinct(id_votacao, .keep_all=T)

  votos_atuais <- read_votos(votos_datapath)

  if (!is.null(votos_camara)) {
    votos_atuais <- votos_atuais %>% bind_rows(votos_camara)
  }

  if (!is.null(votos_senado)) {
    votos_atuais <- votos_atuais %>% bind_rows(votos_senado)
  }

  votos_atuais <- votos_atuais %>%
    mutate(casa_enum = if_else(casa == "camara", 1, 2)) %>%
    mutate(id_parlamentar_parlametria = paste0(casa_enum, id_parlamentar)) %>%
    select(id_votacao, id_parlamentar, id_parlamentar_parlametria, partido, voto, casa) %>%
    distinct()

  votacoes <- votacoes %>%
    mutate(is_nominal = if_else(id_votacao %in% votos_atuais$id_votacao, TRUE, FALSE))

  write_csv(votacoes, votacoes_datapath)
  write_csv(votos_atuais, votos_datapath)
}
