library(tidyverse)

#' @description Lê e cruza dados de disciplina partidária e parlamentares
#' @return Dataframe com informações de disciplina partidária para os parlamentares da legislatura 56
read_disciplina_partidaria <- function(disciplina_path = here::here("reports/disciplina-partidaria/data/disciplina.csv"),
                                       entidades_path = here::here("reports/disciplina-partidaria/data/entidades.csv")) {
  disciplina <- read_csv(disciplina_path)

  parlamentares <- read_csv(entidades_path) %>%
    filter(is_parlamentar == 1, legislatura == 56) %>%
    select(id_entidade_parlametria, nome, partido, uf, em_exercicio)

  disciplina_alt <- disciplina %>%
    left_join(parlamentares,
              by = c("id_parlamentar_parlametria" = "id_entidade_parlametria"))

  return(disciplina_alt)
}
