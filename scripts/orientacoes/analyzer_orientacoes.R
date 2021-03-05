library(perfilparlamentar)
library(tidyverse)

source(here::here("scripts/orientacoes/fetcher_orientacoes.R"))

#' @title Baixa as orientacoes na Câmara e Senado
#' @description Se receber um dataframe de votações, retorna as orientacoes para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro. 
#' @param anos Anos de interesse
#' @param votacoes_datapath Caminho para dataframe de votacoes
#' @param orientacoes_datapath Caminho para dataframe de orientacoes
#' @param votos_datapath Caminho para dataframe de votos

process_orientacoes <- function(anos = c(2019, 2020),
                                  votacoes_datapath = NULL,
                                  orientacoes_datapath = NULL){
  
  # TODO: Hadri vai fazer a lógica de orientação senado.
  
  if(is.null(votacoes_datapath)) {
    orientacoes_camara <- perfilparlamentar::process_orientacao_por_ano_camara(anos)
  } else {
    # se o datapath de votacoes já existir 
    if(!is.null(votacoes_datapath) && file.exists(votacoes_datapath)){
        votacoes_atuais <-
          read_csv(votacoes_datapath, col_types = cols(.default = "c"))  
    } else {
      votacoes_atuais <- tibble(
        id_leggo = character(),
        id_votacao = character(),
        id_proposicao = character(),
        data = character(),
        obj_votacao = character(),
        casa = character()
      )
    }
    
    # se o datapath de orientacoes já existir 
    if(!is.null(orientacoes_datapath) && file.exists(orientacoes_datapath)){
      orientacoes_atuais <-
        read_csv(orientacoes_datapath, col_types = cols(.default = "c"))      
    } else {
      orientacoes_atuais <- tibble(
        id_votacao = character(),
        orientacao = character(),
        tipo_lideranca = character(),
        partido_bloco = character(),
        casa = character()
      )
    }
  }
  
  votacoes_a_processar_camara <- votacoes_atuais %>%
      filter(casa == "camara") %>%
      anti_join(orientacoes_atuais %>%
                distinct(id_votacao)) %>%
      filter(is_nominal == T)
  
  new_orientacoes_camara <- purrr::map_df(
      votacoes_a_processar_camara$id_votacao,
          ~ perfilparlamentar::fetch_orientacoes_votacao_camara(.x)) 
      %>% mutate(casa = "camara")  
  
  
  orientacoes_camara <- orientacoes_atuais %>%
      filter(casa == "camara") %>%
      bind_rows(new_orientacoes_camara) 
  
  
  #TODO: Unir os da câmara aos do senado
  # orientacoes <- bind_rows(orientacoes_camara,
  #                          orientacoes_senado)   
  
  #TODO: Escrever csv de orientacoes 
  # write_csv(orientacoes, orientacoes_datapath)
  write_csv(orientacoes_camara, orientacoes_datapath)
  
}
