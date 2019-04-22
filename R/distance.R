#' @title Lê todos os arquivos csv de uma pasta e os retorna em um único csv
#' @description Recebe um datapath, lê todos os arquivos no formato csv e os une em um csv único
#' @param distancias_datapath Caminho da pasta contendo os arquivos
#' @return Dataframe contendo todas a união de todos os csv's do caminho informado
#' @examples
#' read_distances_files(here::here("data/distancias/"))
read_distances_files <- function(distancias_datapath) {
  list.files(path = distancias_datapath,
             pattern = "*.csv", full.names = T) %>% 
  purrr::map_df(~ readr::read_csv(., col_types = "ncc")) %>% 
  return()
}

#' @title Adiciona a distância às emendas
#' @description Recebe o dataframe de emendas e o caminho para os arquivos csv das distancias calculadas
#' @param emendas_df Dataframe das emendas das proposições
#' @param distancias_datapath Caminho da pasta contendo os arquivos
#' @return Dataframe contendo todas a união de todos os csv's do caminho informado
#' @importFrom magrittr %>% 
#' @export
#' @examples
#' add_distances_to_emendas(emendas_df, here::here("data/distancias/"))
add_distances_to_emendas <- function(emendas_df, distancias_datapath = here::here("data/distancias/")) {
  distances_df <- read_distances_files(distancias_datapath) %>% 
    dplyr::rename("codigo_emenda" = "CdProposition") %>% 
    dplyr::group_by(codigo_emenda) %>% 
    dplyr::summarise(distancia = min(Distance))
  
  emendas_df <- 
    dplyr::left_join(
      emendas_df %>% 
        dplyr::select(-distancia),
      distances_df, 
      by="codigo_emenda") %>%
    dplyr::mutate(distancia = as.numeric(distancia),
                  distancia = dplyr::if_else(is.na(distancia),-1,distancia))
  
  return(emendas_df)
}
