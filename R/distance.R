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

#' @title Padroniza tabela com distâncias das emendas
#' @description Lê o arquivo jus_all_dist.csv gerado pelo script inter_emd_int.py
#' do leggo-content
#' @param distancias_datapath Caminho da pasta contendo os arquivos
#' @param write_datapath Caminho para a pasta de escrita
#' @return Dataframe contendo todas a tabela de emendas formatada
#' @examples
#' format_table_distances_to_emendas(here::here("../leggo-content/util/data/jus_all_dist"), "data/distancias/")
#' @export
format_table_distances_to_emendas <- function(distancias_datapath, write_datapath) {
  out_file_name <- stringr::str_split(distancias_datapath,'/')[[1]] %>% tail(1)
  
  formatted_dists_df <- readr::read_csv(as.character(distancias_datapath), 
                  col_types = list(readr::col_integer(),
                                   readr::col_character(),
                                   readr::col_double())) %>% 
      dplyr::mutate(array = strsplit(comparacao, "_")) %>% 
      dplyr::mutate(id_emenda = sapply(array, head, 1),
                    num_linha_proposicao = sapply(array, tail, 1)) %>% 
      dplyr::select(id_emenda, num_linha_proposicao, Distance = distancia)
  
  print(paste("Saving distances file:",out_file_name))
  
  readr::write_csv(formatted_dists_df, paste0(write_datapath, '/', out_file_name))
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
#' @export
add_distances_to_emendas <- function(emendas_df, distancias_datapath = here::here("data/distancias/")) {
  distances_df <- read_distances_files(distancias_datapath)
  emendas_with_distances <- tibble::tribble(~id_ext, ~codigo_emenda, ~data_apresentacao, 
                                            ~numero, ~local, ~autor, ~casa, ~tipo_documento, 
                                            ~inteiro_teor, ~distancia)
  
  if (nrow(distances_df) > 0) {
    distances_df <- distances_df %>% 
      dplyr::group_by(id_emenda) %>% 
      dplyr::summarise(distancia = min(Distance)) 
    
    emendas_with_distances <- 
      dplyr::left_join(
        emendas_df,
        distances_df, 
        by=c("codigo_emenda"="id_emenda")) %>%
      dplyr::mutate(distancia = as.numeric(distancia),
                    distancia = dplyr::if_else(is.na(distancia),-1,distancia))
  }
  
  return(emendas_with_distances)
}
