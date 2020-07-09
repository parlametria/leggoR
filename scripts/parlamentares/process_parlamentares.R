source(here::here("scripts/parlamentares/fetcher_parlamentares.R"))

#' @title Baixa e processa os dados dos deputados para uma lista de legislaturas
#' @description Recebe um vetor com os ids das legislaturas e retorna os deputados
#' @param legislaturas Vetor com os ids das legislaturas
#' @return Dataframe contendo informações sobre os deputados das legislaturas.
.process_deputados <- function(legislaturas = c(55, 56)) {
  deputados <- .fetch_deputados(legislaturas)
  
  deputados_alt <- deputados %>%
    mutate(nome_civil = tolower(nome_civil) %>% tools::toTitleCase(),
           id = as.character(id)) %>%
    mutate(ultimo_status_nome = tolower(ultimo_status_nome) %>% tools::toTitleCase()) %>%
    mutate(
      ultimo_status_nome_eleitoral = tolower(ultimo_status_nome_eleitoral) %>% tools::toTitleCase()
    ) %>%
    mutate(em_exercicio = if_else(ultimo_status_situacao == "Exercício", 1, 0),
           casa = "camara",
           id_parlamentar_parlametria = paste0(1, id)) %>%
    select(
      legislatura, 
      id_parlamentar = id,
      id_parlamentar_parlametria,
      casa,
      nome_eleitoral = ultimo_status_nome_eleitoral,
      nome_civil,
      cpf,
      sexo,
      partido = ultimo_status_sigla_partido,
      uf = ultimo_status_sigla_uf,
      situacao = ultimo_status_condicao_eleitoral,
      em_exercicio
    ) %>%
    distinct()
  
  return(deputados_alt)
}

#' @title Baixa e processa os dados dos senadores para uma lista de legislaturas
#' @description Recebe um vetor com os ids das legislaturas e retorna os senadores
#' @param legislaturas Vetor com os ids das legislaturas
#' @return Dataframe contendo informações sobre os senadores das legislaturas.
.process_senadores <- function(legislaturas = c(55, 56)) {
  senadores <- .fetch_senadores(legislaturas)
  
  senadores_alt <- senadores %>%
    mutate(casa = "senado",
           id_parlamentar_parlametria = paste0(2, id_parlamentar),
           sexo = if_else(str_detect(genero, "Feminino"), "F", "M")) %>% 
    select(
      legislatura,
      id_parlamentar,
      id_parlamentar_parlametria,
      casa,
      nome_eleitoral,
      nome_civil = nome_completo,
      sexo,
      partido,
      uf,
      situacao
    ) %>%
    distinct()
  
  senadores_em_exercicio <-
    rcongresso::fetch_ids_senadores_em_exercicio() %>%
    mutate(em_exercicio = 1) %>% 
    rename(id_parlamentar = id)
  
  senadores_alt <- senadores_alt %>%
    left_join(senadores_em_exercicio,
              by = "id_parlamentar") %>%
    mutate(em_exercicio = if_else(is.na(em_exercicio), 0, em_exercicio))
  
  return(senadores_alt)
}

#' @title Baixa e processa os dados dos parlamentares para uma lista de legislaturas
#' @description Recebe um vetor com os ids das legislaturas e retorna os deputados e
#' senadores
#' @param legislaturas Vetor com os ids das legislaturas
#' @return Dataframe contendo informações sobre os deputados e senadores das legislaturas.
.process_parlamentares <- function(legislaturas = c(55, 56)) {
  deputados <- .process_deputados(legislaturas)
  senadores <- .process_senadores(legislaturas)
  
  parlamentares <- deputados %>%
    bind_rows(senadores) %>%
    distinct() %>% 
    arrange(legislatura)
  
  return(parlamentares)
}

#' @title Atualiza os dados dos parlamentares nas legs 55 e 56
#' @description Recebe um caminho para o csv dos parlamentares com todas as legislaturas e 
#' atualiza os dados dos parlamentares das últimas legislaturas (55 e 56).
#' @param parlamentares_filepath Caminho para o arquivo de parlamentares.
#' @return Dataframe contendo informações atualizadas.
.update_parlamentares <- function(parlamentares_filepath) {
  legs <- c(55, 56)
  
  current_parlamentares <- read_csv(parlamentares_filepath, col_types = cols(.default = "c")) %>% 
    filter(!legislatura %in% legs)
  
  new_parlamentares <- .process_parlamentares(legs) %>% 
    mutate(legislatura = as.character(legislatura))
  
  parlamentares <- bind_rows(current_parlamentares, new_parlamentares) %>% 
    distinct()
  
  return(parlamentares)
}
