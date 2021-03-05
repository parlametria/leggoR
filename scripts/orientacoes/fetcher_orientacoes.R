library(perfilparlamentar)
library(tidyverse)

#' @title Baixa os orientacoes no Senado
#' @description Se receber um dataframe de votações, retorna as orientações para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param votacoes Dataframe de votações
#' @return Dataframe com orientacoes da câmara
# fetch_orientacoes_senado(votos)
fetch_orientacoes_senado <- function(anos = c(2019, 2020)){
  
    votos <- perfilparlamentar::processa_votos_senado_anos(anos)
    
    votacoes <- votos %>% 
      distinct(id_votacao)
    
    orientacoes <- purrr::map_df(
      votacoes$id_votacao,
      function(x) {
        votos_votacao <- votos %>% 
          filter(id_votacao == x)
        
        return(perfilparlamentar::process_orientacao_votos_senado(votos_votacao))
      }
    )
    
    return(orientacoes)
}

#' @title Baixa os orientacoes no Senado
#' @description Se receber um dataframe de votações, retorna as orientações para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param votacoes Dataframe de votações
#' @return Dataframe com orientacoes da câmara
fetch_orientacoes_senado_por_proposicao_e_votacao <- function(id_proposicao, id_votacao) {
    
    senadores_df <- read_csv("leggo_data/entidades.csv") %>% 
      filter(casa == "senado", is_parlamentar == 1) %>% 
      select(id_parlamentar = id_entidade, nome_eleitoral = nome)
    
    votos <- 
      perfilparlamentar::fetch_votos_por_proposicao_votacao_senado(id_proposicao, id_votacao) %>% 
      processa_votos_senado(senadores_df) %>% 
      enumera_voto()
   
    orientacoes <-process_orientacao_votos_senado(votos)
    
    return(orientacoes)
  }

