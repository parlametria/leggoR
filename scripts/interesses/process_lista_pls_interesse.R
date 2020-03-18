library(tidyverse)
library(here)

#' @title Processa todas as PLs de diversos interesses e que são analisadas pelo LEGGO
#' @description Junta todos as PL's listadas para os interesses analisados pelo LEGGO
#' @param url URL para lista de interesses do leggo
#' @return Dataframe com as PL's que serviram de entrada para o pipeline
#' contendo id_camara,id_senado,apelido,tema,advocacy_link,keywords,tipo_agenda
#' @example
#' tabela_pls <- processa_lista_pls_interesses(url)
processa_lista_pls_interesses <- function(url) {
  interesses <- readr::read_csv(url)

  pls_para_analise <- purrr::pmap_dfr(list(interesses$interesse, interesses$url),
                  function(interesse, url) {
                    pls <- readr::read_csv(url) %>%
                      dplyr::mutate(interesse = interesse)
                    return(pls)
                  })

  return(pls_para_analise)
}

#' @title Mapeia pls e interesses analisados pelo Leggo
#' @description Realiza o mapeamento entre pls e interesses analisados pelo Leggo
#' @param url URL para lista de interesses do leggo
#' @param proposicoes_filepath Caminho para o arquivo de proposições processadas
#' @return Dataframe com o mapeamento entre pls e interesses
#' contendo id_ext, casa, id_leggo, interesse
#' @example
#' tabela_pls <- processa_lista_pls_interesses(url)
processa_interesses_leggo <- function(url, proposicoes_filepath) {
  pls_interesse <- processa_lista_pls_interesses(url) %>%
    dplyr::select(id_camara, id_senado, interesse)

  pls_interesse_camara <- pls_interesse %>%
    mutate(id_ext = id_camara) %>%
    filter(!is.na(id_ext)) %>%
    select(id_ext, interesse)

  pls_interesse_senado <- pls_interesse %>%
    mutate(id_ext = id_senado) %>%
    filter(!is.na(id_ext)) %>%
    select(id_ext, interesse)

  pls_interesse_processed <- pls_interesse_camara %>%
    dplyr::bind_rows(pls_interesse_senado)

  proposicoes_capturadas <- read_csv(proposicoes_filepath) %>%
    dplyr::inner_join(pls_interesse_processed, by = "id_ext") %>%
    select(id_ext, casa, id_leggo, interesse)

  return(proposicoes_capturadas)
}
