library(tidyverse)

#' @title Retorna o critério de proposições mais comentadas no twitter
#' @description Retorna o critério de proposições mais comentadas no twitter
#' de acordo com uma agenda e intervalo de datas.
#' @param interesse Interesse alvo
#' @param data_inicial Data inicial
#' @param data_final Data final
#' @return Dataframe contendo as proposições mais comentadas de um interesse.
.fetch_proposicoes_mais_comentadas_twitter <-
  function(interesse = "leggo",
           data_inicial = "2000-01-01",
           data_final = Sys.Date()) {
    print(paste0(
      "Baixando proposições mais comentadas da agenda ",
      interesse,
      "..."
    ))
    url <- paste0(
      "https://twitter-api.parlametria.org.br/api/proposicoes/mais-comentadas?interesse=",
      interesse,
      "&data_inicial=",
      data_inicial,
      "&data_final=",
      data_final
    )

    proposicoes <- RCurl::getURL(url) %>%
      jsonlite::fromJSON()

    if (!is.null(nrow(proposicoes))) {
      proposicoes <- proposicoes %>%
        select(id_leggo = id_proposicao_leggo, num_parlamentares_tweets) %>%
        mutate(num_parlamentares_tweets = as.numeric(num_parlamentares_tweets)) %>%
        filter(num_parlamentares_tweets > 1)

      return(proposicoes)

    } else {
      return(tibble())
    }

  }

#' @title Retorna o critério de proposições mais comentadas no twitter
#' @description Retorna o critério de proposições mais comentadas no twitter
#' em um intervalo de datas para todas as agendas.
#' @param data_inicial Data inicial
#' @param data_final Data final
#' @return Dataframe contendo as proposições mais comentadas de todos os interesses.
fetch_proposicoes_mais_comentadas_twitter <-
  function(data_inicial = "2000-01-01",
           data_final = Sys.Date(),
           interesses = c("leggo", "congresso-remoto", "transparencia-e-integridade")) {
    df <-
      purrr::map_df(
        interesses,
        ~ .fetch_proposicoes_mais_comentadas_twitter(.x, data_inicial, data_final)
      )

    if (is_empty(names(df))) {
      df <- tibble(
        id_leggo = character()
      )
    }

    return(df)
  }
