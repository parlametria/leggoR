#' @title Lê todos os arquivos csv de uma pasta e os retorna em um único csv
#' @description Recebe um datapath, lê todos os arquivos no formato csv e os une em um csv único
#' @param distancias_datapath Caminho da pasta contendo os arquivos
#' @return Dataframe contendo todas a união de todos os csv's do caminho informado
#' @examples
#' read_distances_files(here::here("data/distancias/"))
read_distances_files <- function(distancias_datapath) {
  list.files(path = distancias_datapath,
             pattern = "*.csv", full.names = T) %>% 
  purrr::map_df(~ agoradigital::read_distance_file(.)) %>% 
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
      tidyr::separate(col = comparacao, 
                      into=c('id_emenda','id_proposicao','casa','rest','num_linha_proposicao'),
                      sep="_") %>% 
      dplyr::select(id_emenda, id_proposicao, casa, num_linha_proposicao, distancia)
  
  print(paste("Saving distances file:",out_file_name))
  
  readr::write_csv(formatted_dists_df, paste0(write_datapath, '/', out_file_name))
}

#' @title Adiciona a distância às emendas
#' @description Recebe o dataframe de novas emendas, o das emendas já analisadas e o caminho para os arquivos csv das distancias calculadas
#' @param novas_emendas_df Dataframe das novas emendas das proposições
#' @param emendas_analisadas_df Dataframe das emendas das proposições que já foram analisadas
#' @param distancias_datapath Caminho da pasta contendo os arquivos
#' @return Dataframe contendo a análise (distância calculada) para todas as emendas (antigas e novas)
#' @importFrom magrittr %>% 
#' @export
#' @examples
#' add_distances_to_emendas(novas_emendas_df, emendas_analisadas_df, here::here("data/distancias/"))
#' @export
add_distances_to_emendas <- function(novas_emendas_df, emendas_analisadas_df, distancias_datapath) {
  distances_df <- read_distances_files(distancias_datapath)
  novas_emendas_com_distancias <- tibble::tibble()
  
  if (nrow(distances_df) > 0) {
    summarised_distances_df <- distances_df %>% 
      dplyr::group_by(id_emenda, id_proposicao, casa) %>% 
      dplyr::summarise(distancia = min(distancia)) %>% 
      dplyr::ungroup()
    
    novas_emendas_com_distancias <- 
      dplyr::left_join(
        novas_emendas_df,
        summarised_distances_df, 
        by=c("codigo_emenda"="id_emenda", "id_ext"="id_proposicao", "casa")) %>%
      dplyr::mutate(distancia = as.numeric(distancia),
                    distancia = dplyr::if_else(is.na(distancia),-1,distancia))
  } else {
    novas_emendas_com_distancias <- novas_emendas_df %>%
      dplyr::mutate(distancia = -1)
  }
  
  emendas_sem_novas_analises <- emendas_analisadas_df %>% 
    dplyr::anti_join(novas_emendas_df %>% 
                       dplyr::select(id_ext, codigo_emenda, casa),
                     by = c('id_ext','codigo_emenda','casa'))
  
  new_emendas_analisadas <- dplyr::bind_rows(emendas_sem_novas_analises, novas_emendas_com_distancias)
  
  return(new_emendas_analisadas)
}
