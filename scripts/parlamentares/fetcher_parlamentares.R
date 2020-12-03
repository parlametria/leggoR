library(tidyverse)

#' @title Baixa os dados dos deputados para uma lista de legislaturas
#' @description Recebe um vetor com os ids das legislaturas e retorna os deputados
#' @param legislaturas Vetor com os ids das legislaturas
#' @return Dataframe contendo informações sobre os deputados das legislaturas.
.fetch_deputados <- function(legislaturas = c(55, 56)) {
  deputados <-
    purrr::map_df(legislaturas, function(x) {
      ids_deputados <- rcongresso::fetch_ids_deputados_by_leg(x) %>%
        dplyr::distinct()
      
      print(paste0("Buscando deputados da legislatura ", x, "..."))
      
      deputados <-
        rcongresso::fetch_all_deputados(ids_deputados) %>%
        dplyr::mutate(legislatura = x)
      
      
      return(deputados)
    })
  
  return(deputados)
}

#' @title Baixa os dados dos senadores para uma lista de legislaturas
#' @description Recebe um vetor com os ids das legislaturas e retorna os senadores
#' @param legislaturas Vetor com os ids das legislaturas
#' @return Dataframe contendo informações sobre os senadores das legislaturas
#' passadas.
.fetch_senadores <- function(legislaturas = c(55, 56)) {
  senadores <-
    purrr::map_df(legislaturas, function(x) {
      ids_senadores <-
        rcongresso::fetch_ids_senadores(legis_initial = x,
                                        legis_final = x) %>%
        dplyr::distinct()
      
      print(paste0("Buscando senadores da legislatura ", x, "..."))
      
      senadores <-
        rcongresso::fetch_all_senadores(ids_senadores) %>%
        dplyr::mutate(legislatura = x)
      
      
      return(senadores)
    })
  
  return(senadores)
}
