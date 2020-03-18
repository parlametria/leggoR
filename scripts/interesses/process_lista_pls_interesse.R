library(tidyverse)
library(here)

#' @title Processa todas as PLs de diversos interesses e que são analisadas pelo LEGGO
#' @description Junta todos as PL's listadas para os interesses analisados pelo LEGGO
#' @return Dataframe com as PL's que serviram de entrada para o pipeline
#' contendo id_camara,id_senado,apelido,tema,advocacy_link,keywords,tipo_agenda
#' @example
#' tabela_pls <- processa_lista_pls_interesses(url)
processa_lista_pls_interesses <- function(url) {
  interesses <- readr::read_csv(url)

  pls_para_analise <- purrr::pmap_dfr(list(interesses$interesse, interesses$url),
                  function(interesse, url) {
                    pls <- readr::read_csv(url)
                    return(pls)
                  })

  return(pls_para_analise)
}
