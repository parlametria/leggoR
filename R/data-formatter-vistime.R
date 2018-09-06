source(here::here("view/formatter/data-formatter-vistime-camara.R"))
source(here::here("view/formatter/data-formatter-vistime-senado.R"))

#' @title Formata tabela para o vistime
#' @description Formata a tabela final que será usado para fazer a visualização
#' usando o vistime
#' @param tram_df dataframe da tramitação do PL na respectiva casa
#' @param house casa onde a proposição está tramitando
#' @param output_folder onde será escrito o resultado
#' @examples
#' build_vis_csv(fetch_tramitacao(2121442, 'camara', T), 'camara')
#' @export
build_vis_csv <- function(tram_df, house, output_folder=NULL) {
  if ("CAMARA" == toupper(house)) {
    build_vis_csv_camara(tram_df, output_folder)
  } else if ("SENADO" == toupper(house)) {
    build_vis_csv_senado(tram_df,output_folder)
  }
}