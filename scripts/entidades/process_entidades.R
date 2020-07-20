library(tidyverse)

.generate_entidades_extras <- function() {
  entidades <-
    tibble::tribble(~ legislatura, ~ id_entidade, ~ id_entidade_parlametria, ~ casa, ~ nome,
                    "56", "1", "31", 'planalto', 'Poder Executivo',
                    "56", "2", "32", 'camara', 'Mesa Diretora da Câmara',
                    "56", "3", "33", 'senado', 'Mesa Diretora do Senado',
                    "56", "4", "34", 'outros', 'Iniciativa Popular',
                    "56", "5", "35", 'outros', 'João Alberto Soares Neto - Advogado',
                    "56", "6", "36", 'senado', 'CPI dos Maus-tratos',
                    "56", "7", "37", 'camara', 'Comissão de Legislação Participativa',
                    "56", "8", "38", 'camara', 'Comissão Parlamentar de Inquérito destinada a apurar denúncias de turismo sexual e exploração sexual de crianças e adolescentes, conforme diversas matérias publicadas pela imprensa.',
                    "56", "9", "39", 'senado', 'Comissão de Constituição, Justiça e Cidadania',
                    "56", "10", "310", 'senado', 'Comissão de Direitos Humanos e Legislação Participativa',
                    "56", "11", "311", 'senado', 'Comissão de Meio Ambiente',
                    "56", "12", "312", 'senado', 'Comissão Especial do Extrateto',
                    "56", "13", "313", 'senado', 'CPI - Pedofilia - 2008',
                    "56", "14", "314", 'senado', 'CT - Modernização da Lei de Licitações e Contratos',
                    "56", "16", "316", 'camara', 'Câmara dos Deputados',
                    "56", "78", "378", 'senado', 'Senado Federal',
                    "56", "17", "317", 'senado', 'Programa e-Cidadania'
                    ) %>%
    mutate(sexo = NA,
           partido = NA,
           uf = NA,
           situacao = NA,
           em_exercicio = NA,
           is_parlamentar =  0) %>%
    select(legislatura, id_entidade, id_entidade_parlametria, casa, nome, sexo, partido, uf, situacao, em_exercicio, is_parlamentar)
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
