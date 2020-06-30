#!/usr/bin/env Rscript
library(magrittr)
library(tidyverse)

if(!require(optparse)){
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly=TRUE)

message("Use --help para mais informações\n")
option_list = list(
  make_option(
    c("-o", "--out"),
    type = "character",
    default = here::here("data/"),
    help = "Caminho do diretório que terão os arquivos de saída [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("-c", "--casa"),
    type = "character",
    default = NA,
    help = "Casa que se deseja atualizar os parlamentares. O default é baixar câmara e senado",
    metavar = "character"
  ),
  make_option(
    c("-f", "--flag"),
    type = "character",
    default = 1,
    help = "Flag para baixar somente os dados da legislatura atual. [default= %default]",
    metavar = "character"
  )
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

export_path <- opt$out
casa <- opt$casa
only_current_legislature_flag <- opt$flag

## Install local repository R package version
devtools::install(upgrade = "never")

if (!str_detect(export_path, "\\/$")) {
  export_path <- paste0(export_path, "/") 
}

dir.create(paste0(export_path, casa), showWarnings = FALSE)

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

  # deputados_alt <- deputados %>%
  #   dplyr::mutate(nome_civil = tolower(nome_civil) %>% tools::toTitleCase()) %>%
  #   dplyr::mutate(ultimo_status_nome = tolower(ultimo_status_nome) %>% tools::toTitleCase()) %>%
  #   dplyr::mutate(
  #     ultimo_status_nome_eleitoral = tolower(ultimo_status_nome_eleitoral) %>% tools::toTitleCase()
  #   ) %>%
  #   dplyr::mutate(em_exercicio = dplyr::if_else(ultimo_status_situacao == "Exercício", 1, 0)) %>%
  #   dplyr::select(
  #     id,
  #     nome_eleitoral = ultimo_status_nome_eleitoral,
  #     nome_civil,
  #     cpf,
  #     sexo,
  #     partido = ultimo_status_sigla_partido,
  #     uf = ultimo_status_sigla_uf,
  #     situacao = ultimo_status_condicao_eleitoral,
  #     em_exercicio
  #   )
  readr::write_csv(deputados,
                   paste0(export_path, casa, "/parlamentares.csv"))
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

  # senadores_alt <- senadores %>%
  #   dplyr::select(
  #     id = id_parlamentar,
  #     nome_eleitoral,
  #     nome_civil = nome_completo,
  #     sexo = genero,
  #     partido,
  #     uf,
  #     situacao
  #   ) %>%
  #   dplyr::distinct()
  # 
  # senadores_em_exercicio <-
  #   rcongresso::fetch_ids_senadores_em_exercicio() %>%
  #   dplyr::mutate(em_exercicio = 1)
  # 
  # senadores_alt <- senadores_alt %>%
  #   dplyr::left_join(senadores_em_exercicio,
  #                    by = "id") %>%
  #   dplyr::mutate(em_exercicio = dplyr::if_else(is.na(em_exercicio), 0, em_exercicio))

  readr::write_csv(senadores,
                   paste0(export_path, casa, "/parlamentares.csv"))
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



