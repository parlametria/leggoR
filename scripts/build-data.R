source(here::here("R/analyzers/analyzer.R"))
args = commandArgs(trailingOnly=TRUE)

#' @title Cria todos os csvs de um proposição.
#' @description Recebido um id e uma casa a função roda os scripts para
#' importar, processar e adaptar para visualização; os dados daquela proposição.
#' @param id Identificador da proposição que pode ser recuperado no site da casa legislativa.
#' @param house Casa a que pertence essa proposição.
#' @examples
#' build_csvs(91341, "senado")
#' build_csvs(257161, "camara")
#' @export
build_csvs <- function(id, house) {
  if("SENADO" == toupper(house)){
    source(here::here("R/fetcher.R"))
    import_proposicao(id)
  } else if("CAMARA" != toupper(house)){
    return(NULL)
  }
  process_proposicao(id, house)
  source(here::here(paste0("view/formatter/data-formatter-vistime-", tolower(house), ".R")))
  build_vis_csv(id)

  as.tibble(NULL)
}

#' @title Cria todos os csvs de todas as proposições da tabela.
#' @description Recebido um dataframe com as colunas id e casa a função roda os scripts para
#' importar, processar e adaptar para visualização; os dados daquela proposição.
#' @param df Dataframe com as colunas id e casa
#' @examples
#' readr::read_csv("data/tabela_geral_ids_casa.csv") %>% build_all_csvs()
#' @export
build_all_csvs <- function(df) {
  if ('casa' %in% names(df)) {
    df %>%
      rowwise() %>%
      do(build_csvs(.$id, .$casa)) 
  }else {
    df %>%
      rowwise() %>%
      do(build_csvs(.$id_camara, 'camara')) 
    
    df %>%
      rowwise() %>%
      do(build_csvs(.$id_senado, 'senado'))
  }
}

if(length(args) == 2){
  build_csvs(args[1], args[2])
}
