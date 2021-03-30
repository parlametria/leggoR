library(perfilparlamentar)
library(tidyverse)

#' @title Baixa os votos na Câmara
#' @description Se receber um dataframe de votações, retorna os votos para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param votacoes Dataframe de votações
#' @return Dataframe com votos da câmara
fetch_votos_camara <-
  function(anos = c(2019, 2020),
           votacoes = NULL) {
    if (is.null(votacoes)) {
      votos <- perfilparlamentar::processa_votos_camara_anos(anos)
    } else {
      votos <-
        purrr::map_df(votacoes$id_votacao,
                      function(x) {
                        data <- tryCatch({
                          df <- fetch_votos_por_votacao_camara(x)
                        }, error = function(e) {
                          print(e)
                          return(
                            tibble(
                              id_proposicao = character(),
                              id_votacao = character(),
                              id_parlamentar = character(),
                              partido = character(),
                              voto = character()
                            )
                          )
                        })
                        return(data)
                      })
    }

    votos <- votos %>%
      mutate(casa = "camara")

    return(votos)
  }

#' @title Baixa os votos no Senado
#' @description Se receber um dataframe de votações, retorna os votos para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param votacoes Dataframe de votações
#' @return Dataframe com votos do senado
fetch_votos_senado <-
  function(anos = c(2019, 2020),
           votacoes = NULL,
           entidades_filepath = here::here("leggo_data/entidades.csv")) {
    if (is.null(votacoes)) {
      votos <- perfilparlamentar::processa_votos_senado_anos(anos)
    } else {
      votos <- purrr::map2_df(
        votacoes$id_proposicao,
        votacoes$id_votacao,
        ~ fetch_votos_por_proposicao_votacao_senado(.x, .y)
      )
    }

    senadores_df <- read_csv(entidades_filepath, col_types = cols(.default = "c")) %>%
      filter(casa == 'senado', is_parlamentar == 1) %>%
      select(id_parlamentar = id_entidade, nome_eleitoral = nome) %>%
      distinct()

    votos <- processa_votos_senado(votos, senadores_df)

    votos <- votos %>%
      mutate(casa = "senado")

    return(votos)
  }

