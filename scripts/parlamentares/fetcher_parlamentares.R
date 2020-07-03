library(tidyverse)

#' @title Baixa os dados dos deputados para uma lista de legislaturas
#' @description Recebe um vetor com os ids das legislaturas e retorna os deputados
#' @param legislaturas Vetor com os ids das legislaturas
#' @return Dataframe contendo informações sobre os deputados das legislaturas.
.fetch_deputados <- function(legislaturas = c(55, 56)) {
  ids_deputados <-
    purrr::map_df(legislaturas, ~ rcongresso::fetch_ids_deputados_by_leg(.x)) %>%
    dplyr::distinct()
  
  print("Buscando deputados...")
  
  deputados <- rcongresso::fetch_all_deputados(ids_deputados)
  
  return(deputados)
}

#' @title Baixa os dados dos senadores para uma lista de legislaturas
#' @description Recebe um vetor com os ids das legislaturas e retorna os senadores
#' @param legislaturas Vetor com os ids das legislaturas
#' @return Dataframe contendo informações sobre os senadores das legislaturas 
#' passadas.
.fetch_senadores <- function(legislaturas = c(55, 56)) {
  ids_senadores <-
    rcongresso::fetch_ids_senadores(legis_initial = legislaturas[[1]],
                                    legis_final = legislaturas[[length(legislaturas)]]) %>%
    dplyr::distinct()
  
  print("Buscando senadores...")
  
  senadores <- rcongresso::fetch_all_senadores(ids_senadores)
  
  return(senadores)
}
