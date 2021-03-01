library(perfilparlamentar)
library(tidyverse)

#' @title Baixa os orientacoes na Câmara
#' @description Se receber um dataframe de votações, retorna as orientações para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param votacoes Dataframe de votações
#' @return Dataframe com orientacoes da câmara
fetch_orientacoes_camara <-
  function(anos = c(2019, 2020),
           votacoes = NULL) {
    if (is.null(votacoes)) {
      orientacoes <- perfilparlamentar::process_orientacao_por_ano_camara(anos)
    } else {
      orientacoes <- 
        purrr::map_df(votacoes$id_votacao,
                      function(x) {
                        data <- tryCatch({
                          df <- fetch_orientacoes_votacao_camara(x)
                        }, error = function(e) {
                          print(e)
                          return(
                            tibble(
                              ano = numeric(),
                              id_proposicao = character(),
                              id_votacao = character(),
                              orientacao = character(),
                              tipo_lideranca = character(),
                              partido_bloco = character(),
                              casa = character()
                            )
                          )
                        })
                        return(data)
                      })
    }
    
    orientacoes <- orientacoes %>%
      mutate(casa = "camara")
    
    return(orientacoes)
  }
