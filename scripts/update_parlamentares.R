#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_parlamentares.R <export_path> <casa> <somente_legislatura_atual_flag>
"
## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 1
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

export_path <- args[1]
casa <- args[2]
only_current_legislature_flag <- args[3]

## Install local repository R package version
devtools::install()


update_deputados <- function(casa = "camara") {
  ids_deputados <- tibble::tibble()

  if (!is.na(only_current_legislature_flag) &
      only_current_legislature_flag == 0) {
    ids_deputados <- rcongresso::fetch_ids_deputados_by_leg(55)
  }

  ids_deputados <- ids_deputados %>%
    dplyr::bind_rows(rcongresso::fetch_ids_deputados_by_leg(56)) %>%
    dplyr::distinct()

  print("Buscando deputados...")

  deputados <- rcongresso::fetch_all_deputados(ids_deputados)

  deputados_alt <- deputados %>%
    dplyr::mutate(nome_civil = tolower(nome_civil) %>% tools::toTitleCase()) %>%
    dplyr::mutate(ultimo_status_nome = tolower(ultimo_status_nome) %>% tools::toTitleCase()) %>%
    dplyr::mutate(
      ultimo_status_nome_eleitoral = tolower(ultimo_status_nome_eleitoral) %>% tools::toTitleCase()
    ) %>%
    dplyr::mutate(em_exercicio = dplyr::if_else(ultimo_status_situacao == "Exercício", 1, 0)) %>%
    dplyr::select(
      id,
      nome_eleitoral = ultimo_status_nome_eleitoral,
      nome_civil,
      cpf,
      sexo,
      partido = ultimo_status_sigla_partido,
      uf = ultimo_status_sigla_uf,
      situacao = ultimo_status_condicao_eleitoral,
      em_exercicio
    )
  readr::write_csv(deputados_alt,
                   paste0(export_path, '/', casa, "/deputados.csv"))
}

update_senadores <- function(casa = "senado") {
  ids_senadores <- tibble::tibble()

  if (!is.na(only_current_legislature_flag) &
      only_current_legislature_flag == 0) {
    ids_senadores <-
      rcongresso::fetch_ids_senadores(legis_initial = 55, legis_final = 56)
  }

  ids_senadores <- ids_senadores %>%
    dplyr::bind_rows(rcongresso::fetch_ids_senadores(legis_initial = 56, legis_final = 56)) %>%
    dplyr::distinct()

  print("Buscando senadores...")

  senadores <- rcongresso::fetch_all_senadores(ids_senadores)

  senadores_alt <- senadores %>%
    dplyr::select(
      id = id_parlamentar,
      nome_eleitoral,
      nome_civil = nome_completo,
      sexo = genero,
      partido,
      uf,
      situacao
    ) %>%
    dplyr::distinct()

  senadores_em_exercicio <-
    rcongresso::fetch_ids_senadores_em_exercicio() %>%
    dplyr::mutate(em_exercicio = 1)

  senadores_alt <- senadores_alt %>%
    dplyr::left_join(senadores_em_exercicio,
                     by = "id") %>%
    dplyr::mutate(em_exercicio = dplyr::if_else(is.na(em_exercicio), 0, em_exercicio))

  readr::write_csv(senadores_alt,
                   paste0(export_path, '/', casa, "/senadores.csv"))
}

if (is.na(casa)) {

  update_deputados()
  update_senadores()

} else {

  if (casa == "camara") {

    update_deputados()

  } else if (casa == "senado") {

    update_senadores()

  } else {
    print("Parâmetro inválido!")
  }

}



