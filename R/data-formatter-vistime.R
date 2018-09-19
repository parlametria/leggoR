source(here::here("R/data-formatter-vistime-camara.R"))
source(here::here("R/data-formatter-vistime-senado.R"))

#' @title Formata tabela para o vistime
#' @description Formata a tabela final que será usado para fazer a visualização
#' usando o vistime
#' @param tram_df dataframe da tramitação do PL na respectiva casa
#' @param house casa onde a proposição está tramitando
#' @param output_folder pasta onde será escrito o arquivo
#' @examples
#' prop_data <- agoradigital::import_proposicao(2121442, 'camara', 'data/')
#' proc_tram_data <- agoradigital::process_proposicao(prop_data$proposicao, prop_data$tramitacao, 'camara', 'data/')
#' build_vis_csv(proc_tram_data, 'camara', 'data/')
#' @export
build_vis_csv <- function(tram_df, house, output_folder=NULL) {
  if ("CAMARA" == toupper(house)) {
    build_vis_csv_camara(tram_df, output_folder)
  } else if ("SENADO" == toupper(house)) {
    build_vis_csv_senado(tram_df,output_folder)
  }
}

#' @title Formata tabela para mostrar só os locais
#' @description Formata a tabela final que será usado para fazer a visualização
#' dos locais no frnt
#' @param data dataframe da tramitação do vistime
#' @param bill_id Id da proposição
#' @param output_folder pasta onde será escrito o arquivo
#' @examples
#' prop_data <- agoradigital::import_proposicao(2121442, 'camara', 'data/')
#' proc_tram_data <- agoradigital::process_proposicao(prop_data$proposicao, prop_data$tramitacao, 'camara', 'data/')
#' get_locais(agoradigital::build_vis_csv(proc_tram_data, 'camara', 'data/'), 2121442, 'data/')
#' @export
get_locais <- function(data, bill_id, output_folder=NULL) {
  data <-
    data %>%
    dplyr::filter(group == 'Global' & 
                    !stringr::str_detect(label, 'Apresentação') & 
                    !stringr::str_detect(label, 'Mesa')) %>%
    dplyr::select(label, start, end)
    
  data %>%
    write_csv(paste0(output_folder,'/vis/tramitacao/',bill_id,"-locais.csv"))
  
  data
}