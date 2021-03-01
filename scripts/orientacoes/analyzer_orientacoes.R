library(perfilparlamentar)
library(tidyverse)

source(here::here("scripts/orientacoes/fetcher_orientacoes.R"))

#' @title Baixa as orientacoes na Câmara
#' @description Se receber um dataframe de votações, retorna os votos para aquele conjunto de
#' votações. Caso contrário, baixa tudo dos anos passados como parâmetro.
#' @param anos Anos de interesse
#' @param orientacoes_datapath Caminho para dataframe de orientacoes
processa_orientacoes <- function(anos = c(2019, 2020),
                           votacoes_datapath = NULL,
                           orientacoes_datapath = NULL,
                           proposicoes_datapath = here::here("leggo_data/proposicoes.csv")) {
  
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
  
  orientacoes_camara <- NULL
  votos_senado <- NULL
  
  if (!is.null(votacoes_datapath) && file.exists(votacoes_datapath)) {
    votacoes_atuais <-
      read_csv(votacoes_datapath, col_types = cols(.default = "c"))
    
    votacoes_a_processar_camara <-
      anti_join(
        new_votacoes_camara,
        votacoes_atuais %>%
          filter(casa == "camara"),
        by = c("id_proposicao", "id_votacao")
      )
    
    
    votacoes_a_processar_senado <-
      anti_join(
        new_votacoes_senado,
        votacoes_atuais %>%
          filter(casa == "senado"),
        by = c("id_proposicao", "id_votacao")
      )
    
  } else {
    votacoes_atuais <- tibble(
      id_leggo = character(),
      id_votacao = character(),
      id_proposicao = character(),
      data = character(),
      obj_votacao = character(),
      casa = character()
    )
    
    votacoes_a_processar_camara <- new_votacoes_camara
    votacoes_a_processar_senado <- new_votacoes_senado
  }
  
  proposicoes <- read_csv(proposicoes_datapath, col_types = cols(.default = "c")) %>% 
    select(id_leggo, id_proposicao = id_ext, casa)
  
  votacoes_a_processar_camara <- 
    left_join(votacoes_a_processar_camara, proposicoes, by = c("id_proposicao", "casa"))
  
  votacoes_a_processar_senado <- 
    left_join(votacoes_a_processar_senado, proposicoes, by = c("id_proposicao", "casa"))
  
  if (nrow(votacoes_a_processar_camara) > 0) {
    orientacoes_camara <-
      fetch_orientacoes_camara(anos, votacoes_a_processar_camara)
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
  
  votacoes <- votacoes %>% 
    mutate(data = gsub("T", " ", data)) %>% 
    distinct()
  
  if (!is.null(orientacoes_datapath) && file.exists(orientacoes_datapath)) {
    orientacoes_atuais <-
      read_csv(orientacoes_datapath)
    
  } else {
    orientacoes_atuais <- tibble(
      ano = numeric(),
      id_proposicao = character(),
      id_votacao = character(),
      orientacao = character(),
      tipo_lideranca = character(),
      partido_bloco = character(),
      casa = character()
    )
  }
  
  if (!is.null(orientacoes_camara)) {
    orientacoes_atuais <- orientacoes_atuais %>% bind_rows(orientacoes_camara)
  }
  
  write_csv(votacoes, votacoes_datapath)
  write_csv(orientacoes_atuais, orientacoes_datapath)
}
