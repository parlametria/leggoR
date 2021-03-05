library(perfilparlamentar)
library(tidyverse)

source(here::here("scripts/orientacoes/fetcher_orientacoes.R"))
source(here::here("scripts/votos/utils.R"))
source(here::here("scripts/orientacoes/utils.R"))

#' @title Processa as orientacoes na Câmara
#' @description Recebe as votações e orientações atuais e baixa as novas orientações.
#' @param votacoes_atuais Dataframe de votacoes
#' @param orientacoes_atuais Dataframe de orientacoes
#' @return Dataframe com as novas orientações, na camara
.process_orientacoes_camara <-
  function(votacoes_atuais, orientacoes_atuais) {
    votacoes_a_processar_camara <- votacoes_atuais %>%
      filter(casa == "camara") %>% 
      anti_join(orientacoes_atuais %>%
                  filter(casa == "camara") %>%
                  distinct(id_votacao),
                by = "id_votacao") %>%
      filter(is_nominal == T)
    
    new_orientacoes_camara <- purrr::map_df(
      votacoes_a_processar_camara$id_votacao,
      ~ perfilparlamentar::fetch_orientacoes_votacao_camara(.x)
    ) %>%
      mutate(casa = "camara") %>%
      rename(voto = orientacao) %>%
      enumera_voto() %>% 
      rename(orientacao = voto)
      
    
    return(new_orientacoes_camara)
  }

#' @title Processa as orientacoes no Senado
#' @description Recebe os votos das votações e orientações atuais e
#' processa as novas orientações.
#' @param votos_atuais Dataframe de votos
#' @param orientacoes_atuais Dataframe de orientacoes
#' @return Dataframe com as novas orientações, no Senado
.process_orientacoes_votos_senado <-
  function(votos_atuais, orientacoes_atuais) {
    votos_senado <- votos_atuais %>%
      filter(casa == "senado") %>%
      anti_join(orientacoes_atuais %>%
                  filter(casa == "senado") %>%
                  distinct(id_votacao),
                by = "id_votacao")
    
    
    votacoes_a_processar_senado <- votos_senado %>%
      distinct(id_votacao)
    
    new_orientacoes_senado <-
      purrr::map_df(votacoes_a_processar_senado$id_votacao,
                    function(x) {
                      votos_votacao <- votos_senado %>%
                        filter(id_votacao == x)
                      return(process_orientacao_votos_senado(votos_votacao))
                    })
    
    new_orientacoes_senado <- new_orientacoes_senado %>% 
      mutate(casa = "senado") %>% 
      rename(orientacao = voto)
    
    return(new_orientacoes_senado)
  }

#' @title Baixa as orientacoes na Câmara e Senado
#' @description Se receber um dataframe de votações, retorna as orientacoes para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param votacoes_datapath Caminho para dataframe de votacoes
#' @param votos_datapath Caminho para dataframe de votos
#' @param orientacoes_datapath Caminho para dataframe de orientacoes
process_orientacoes <- function(anos = c(2019, 2020, 2021),
                                votacoes_datapath = NULL,
                                votos_datapath = NULL,
                                orientacoes_datapath = NULL) {
  if (is.null(votacoes_datapath) & is.null(votos_datapath)) {
    new_orientacoes_camara <- process_orientacao_anos_camara(anos)
    new_orientacoes_senado <- fetch_orientacoes_senado(anos)
    
  } else {
    votacoes_atuais <- read_votacoes(votacoes_datapath)
    votos_atuais <- read_votos(votos_datapath)
    orientacoes_atuais <- read_orientacoes(orientacoes_datapath)
    
    new_orientacoes_camara <-
      .process_orientacoes_camara(votacoes_atuais %>% filter(casa == "camara", is_nominal == T) %>%  head(3), orientacoes_atuais)
    
    new_orientacoes_senado <-
      .process_orientacoes_votos_senado(votos_atuais %>% filter(id_votacao %in% c("5948", "5946", "5947")), orientacoes_atuais)
  }
  
  orientacoes <- orientacoes_atuais %>%
    bind_rows(new_orientacoes_camara) %>%
    bind_rows(new_orientacoes_senado)
  
  write_csv(orientacoes, orientacoes_datapath)
  
}
