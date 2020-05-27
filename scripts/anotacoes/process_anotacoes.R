#' @title Baixa as anotações dos interesses do leggo
#' @description A partir de uma lista de anotações com interesses atrelados, 
#' baixa os dados de anotações e mapeia os interesses correspondentes.
#' @param url URL para lista de anotações por interesse do leggo
#' @return Dataframe com o mapeamento entre anotações e interesses
#' contendo data_criacao, data_ultima_modificacao, proposicao, 
#' autor, titulo, anotacao, interesse 
#' @example
#' anotacoes <- .processa_lista_anotacoes(url)
.processa_lista_anotacoes <- function(url) {
  
  if (is.null(url) | url == "") {
    stop("URL para planilha de anotações precisa ser diferente de vazio e não nula.")
  }
  
  library(tidyverse)
  
  colunas <-
    c(
      "data_criacao",
      "data_ultima_modificacao",
      "proposicao",
      "autor",
      "categoria",
      "titulo",
      "anotacao",
      "peso",
      "interesse"
    )
  
  lista <- readr::read_csv(url)
  
  anotacoes <-
    purrr::pmap_dfr(list(lista$interesse, lista$url),
                    function(interesse, url) {
                      df <- readr::read_csv(url) %>%
                        dplyr::mutate(interesse = interesse)
                      return(df)
                    })
  
  names(anotacoes) <- colunas
  
  return(anotacoes)
}

#' @title Processa as anotações dos interesses do leggo
#' @description Mapeia os identificadores da proposição à anotação a partir do seu
#' interesse e da proposição selecionada na planilha de anotações.
#' @param url URL para lista de anotações por interesse do leggo
#' @param pls_interesses_datapath Caminho do arquivo do dataframe que possui
#' os identificadores, interesses e proposições mapeados.
#' @param proposicoes_datapath Caminho do caminho do dataframe que possui 
#' o identificador do leggo para as proposições.
#' @return Dataframe com o mapeamento entre anotações e interesses
#' contendo data_criacao, data_ultima_modificacao, proposicao, 
#' autor, titulo, anotacao, interesse, id_ext, casa, id_leggo. 
#' @example
#' anotacoes <- processa_anotacoes(url, pls_interesses_datapath, proposicoes_datapath)
processa_anotacoes <- function(url, pls_interesses_datapath, proposicoes_datapath) {
  library(tidyverse)
  
  anotacoes <- .processa_lista_anotacoes(url)
  
  anotacoes <- anotacoes %>%
    dplyr::mutate_at(vars("data_criacao", "data_ultima_modificacao"),
                     ~ lubridate::mdy_hms(.))
  
  pls_interesses <- readr::read_csv(pls_interesses_datapath,
                                    col_types = cols(.default = "c")) %>%
    dplyr::select(proposicao, id_camara, id_senado, interesse)
  
  anotacoes_com_ids <- anotacoes %>% 
    dplyr::left_join(pls_interesses, by = c("proposicao", "interesse")) %>% 
    dplyr::select(-proposicao)
  
  anotacoes_com_ids_camara <- anotacoes_com_ids %>%
    dplyr::mutate(id_ext = id_camara) %>%
    dplyr::filter(!is.na(id_ext)) %>%
    dplyr::select(-c(id_senado, id_camara))
  
  anotacoes_com_ids_senado <- anotacoes_com_ids %>%
    dplyr::mutate(id_ext = id_senado) %>%
    dplyr::filter(!is.na(id_ext)) %>%
    dplyr::select(-c(id_senado, id_camara))
  
  anotacoes_com_ids <- anotacoes_com_ids_camara %>%
    dplyr::bind_rows(anotacoes_com_ids_senado)
  
  proposicoes <-
    readr::read_csv(proposicoes_datapath, col_types = cols(.default = "c")) %>%
    dplyr::select(id_leggo, id_ext)
  
  anotacoes_com_ids_processed <- proposicoes %>%
    dplyr::inner_join(anotacoes_com_ids, by = "id_ext") %>% 
    dplyr::select(-id_ext) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      categoria = iconv(categoria, 
                        from="UTF-8", 
                        to="ASCII//TRANSLIT") %>% 
        tolower())
  
  return(anotacoes_com_ids_processed)
}