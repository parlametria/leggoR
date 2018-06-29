args = commandArgs(trailingOnly=TRUE)

#' @title Cria todos os csvs de um proposição na câmara.
#' @description Recebido um id e uma casa a função roda os scripts para 
#' importar e processar os dados daquela proposição no câmara.
#' @param id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' build_camara(257161)
#' @export
build_camara <- function(id){
  source(here::here("code/camara-process-data.R"))
  process_proposicao(id)
}

#' @title Cria todos os csvs de um proposição no senado.
#' @description Recebido um id e uma casa a função roda os scripts para 
#' importar e processar os dados daquela proposição no senado.#'
#' @param id Identificador da proposição que pode ser recuperado no site do senado.
#' @examples
#' build_senado(91341)
#' @export
build_senado <- function(id){
  source(here::here("code/importa-dados-Senado.R"))
  import_proposicao(id)
  source(here::here("code/processa-dados-Senado.R"))
  process_proposicao(id)
}

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
  if("CAMARA" == toupper(house)){
    build_camara(id)
  } else if("SENADO" == toupper(house)){
    build_senado(id)
  }
  source(here::here(paste0("code/vis/tramitacao/data-chart-", tolower(house), ".R")))
  build_vis_csv(id)
  
  as.tibble(NULL)
}

df <- frame_data(~ id, ~ casa,
                  132865, 'senado',
                  91341, 'senado',
                 41703, 'senado',
                 127753, 'senado',
                 129808, 'senado',
                 257161, 'camara',
                 345311, 'camara',
                 2056568, 'camara',
                 46249, 'camara',
                 2121442, 'camara',
                 2160860, 'camara',
                 2120775, 'camara'
                 )

build_all_csvs <- function(df) {
  df %>%
    rowwise() %>%
    do (build_csvs(.$id, .$casa))
  
}

if(length(args) == 2){
  build_csvs(args[1], args[2])
} 
