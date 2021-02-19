library(perfilparlamentar)
library(tidyverse)

source(here::here("scripts/votos/fetcher_votos.R"))

#' @title Baixa os votos na Câmara
#' @description Se receber um dataframe de votações, retorna os votos para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param votacoes_datapath Caminho para dataframe de votações
#' @param votos_datapath Caminho para dataframe de votos
processa_votos <- function(anos = c(2019, 2020),
                           votacoes_datapath = NULL,
                           votos_datapath = NULL) {
  
  new_votacoes_camara <- processa_votacoes_camara_anos(anos) %>%
    mutate(casa = "camara",
           id_proposicao = as.character(id_proposicao))
  
  new_votacoes_senado <- processa_votacoes_senado_anos(anos) %>%
    mutate(casa = "senado") %>%
    select(
      id_votacao = codigo_sessao,
      id_proposicao,
      data = datetime,
      obj_votacao = objeto_votacao,
      casa
    )
  
  votos_camara <- NULL
  votos_senado <- NULL
  
  if (!is.null(votacoes_datapath) && file.exists(votacoes_datapath)) {
    votacoes_atuais <-
      read_csv(votacoes_datapath, col_types = cols(.default = "c"))
    
    votacoes_a_processar_camara <-
      anti_join(
        votacoes_atuais %>%
          filter(casa == "camara"),
        new_votacoes_camara,
        by = c("id_proposicao", "id_votacao")
      )
    
    
    votacoes_a_processar_senado <-
      anti_join(
        votacoes_atuais %>%
          filter(casa == "senado"),
        new_votacoes_senado,
        by = c("id_proposicao", "id_votacao")
      )
    
  } else {
    votacoes_atuais <- tibble(
      id_votacao = character(),
      id_proposicao = character(),
      data = character(),
      obj_votacao = character(),
      casa = character()
    )
    
    votacoes_a_processar_camara <- new_votacoes_camara
    votacoes_a_processar_senado <- new_votacoes_senado
  }
  
  if (nrow(votacoes_a_processar_camara) > 0) {
    votos_camara <-
      fetch_votos_camara(anos, votacoes_a_processar_camara %>% head(10))
  }
  
  if (nrow(votacoes_a_processar_senado) > 0) {
    votos_senado <-
      fetch_votos_senado(anos, votacoes_a_processar_senado) %>% 
      mutate(id_parlamentar = as.integer(id_parlamentar) %>% head(10)) %>% 
      select(-id_proposicao)
  }
  
  votacoes_camara <-
    bind_rows(votacoes_atuais %>%
                filter(casa == "camara"),
              votacoes_a_processar_camara)
  
  votacoes_senado <-
    bind_rows(votacoes_atuais %>%
                filter(casa == "senado"),
              votacoes_a_processar_senado)
  
  votacoes <- votacoes_camara %>%
    bind_rows(votacoes_senado)
  
  if (!is.null(votos_datapath) && file.exists(votos_datapath)) {
    votos_atuais <-
      read_csv(votos_datapath, col_types = cols(id_parlamentar="i",
                                                id_parlamentar_parlametria="i",
                                                .default = "c"))
    
  } else {
    votos_atuais <- tibble(
      id_votacao = character(),
      id_parlamentar = integer(),
      id_parlamentar_parlametria = integer(),
      partido = character(),
      voto = character(),
      casa = character()
    )
  }
  
  if (!is.null(votos_camara)) {
    votos_atuais <- votos_atuais %>% bind_rows(votos_camara)
  }
  
  if (!is.null(votos_senado)) {
    votos_atuais <- votos_atuais %>% bind_rows(votos_senado)
  }
  
  votos_atuais <- votos_atuais %>% 
    mutate(casa_enum = if_else(casa == "camara", 1, 2)) %>% 
    mutate(id_parlamentar_parlametria = paste0(casa_enum, id_parlamentar)) %>% 
    select(id_votacao, id_parlamentar, id_parlamentar_parlametria, partido, voto, casa)
  
  write_csv(votacoes, votacoes_datapath)
  write_csv(votos_atuais, votos_datapath)
}
