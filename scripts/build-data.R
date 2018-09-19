source(here::here("R/analyzer.R"))
source(here::here("R/data-formatter-vistime.R"))
args = commandArgs(trailingOnly=TRUE)

#' @title Cria todos os csvs de um proposição.
#' @description Recebido um id e uma casa a função roda os scripts para
#' importar, processar e adaptar para visualização; os dados daquela proposição.
#' @param id Identificador da proposição que pode ser recuperado no site da casa legislativa.
#' @param house Casa a que pertence essa proposição.
#' @param apelido Apelido da proposição
#' @param tema Tema da proposição
#' @importFrom %<>% magrittr
#' @examples
#' build_csvs(129808, "senado", "Cadastro Positivo", "Agenda Nacional", "data/")
#' build_csvs(257161, "camara", "Lei Geral do Licensiamento Ambiental", "data/")
#' @export
build_csvs <- function(id, house, apelido='', tema='', output_folder=NULL) {
  print(paste("Processando id",id,"da casa",house))
  prop_data <- import_proposicao(id, house, apelido, tema, output_folder)
  proc_tram_data <- process_proposicao(prop_data$proposicao, prop_data$tramitacao, house, output_folder)
  proc_progresso_data <- get_progresso(prop_data$tramitacao, prop_data$proposicao, house, output_folder)
  build_vis_csv(proc_tram_data, house, output_folder)
  as.tibble(NULL)
}

#' @title Cria todos os csvs de todas as proposições da tabela.
#' @description Recebido um dataframe com as colunas id e casa a função roda os scripts para
#' importar, processar e adaptar para visualização; os dados daquela proposição.
#' @param df Dataframe com as colunas id e casa
#' @examples
#' readr::read_csv("data/tabela_geral_ids_casa.csv") %>% build_all_csvs()
#' @export
build_all_csvs <- function(df, output_folder=NULL) {
  if ('casa' %in% names(df)) {
    pmap(list(df$id, df$casa, df$apelido, df$tema), function(a, b, c, d) build_csvs(a, b, c, d, output_folder))
  } else {
    map(df$id_camara, ~ build_csvs(.x, 'camara', '', '', output_folder))
    map(df$id_senado, ~ build_csvs(.x, 'senado', '', '', output_folder))
  }
  as.tibble(NULL)
}

if(length(args) == 2){
  build_csvs(args[1], args[2])
}
