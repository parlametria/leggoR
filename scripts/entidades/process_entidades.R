library(tidyverse)

.generate_entidades_extras <- function() {
  entidades <- 
    tibble::tribble(~ legislatura, ~ id_entidade, ~ id_entidade_parlametria, ~ casa, ~ nome, ~ sexo, ~ partido, ~ uf, ~situacao, ~ em_exercicio,
                    "56", "1", "31", 'planalto', 'Poder Executivo', NA, NA, NA, NA, "1",
                    "56", "1", "11", 'camara', 'Mesa Diretora', NA, NA, NA, NA, "1",
                    "56", "1", "21", 'senado', 'Mesa Diretora', NA, NA, NA, NA, "1") %>% 
    mutate(is_parlamentar =  0)
}

.process_entidades <- function(parlamentares_filepath) {
  parlamentares <-
    read_csv(parlamentares_filepath, col_types = cols(.default = "c")) %>%
    select(
      legislatura,
      id_entidade = id_parlamentar,
      id_entidade_parlametria = id_parlamentar_parlametria,
      casa,
      nome = nome_eleitoral,
      sexo,
      partido,
      uf,
      situacao,
      em_exercicio
    ) %>%
    mutate(is_parlamentar = 1)
  
  entidades <-
    parlamentares %>% bind_rows(.generate_entidades_extras()) %>% 
    mutate(em_exercicio = if_else(is.na(em_exercicio), as.character(0), em_exercicio))
  
  return(entidades)
  
}
