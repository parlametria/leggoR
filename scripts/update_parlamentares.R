#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_parlamentares.R <export_path> <casa>
"
## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 1
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

export_path <- args[1]
casa <- args[2]

## Install local repository R package version
devtools::install()

if (is.na(casa)) {

  ids_deputados <- rcongresso::fetch_ids_deputados()
  print("Buscando deputados...")
  deputados <- rcongresso::fetch_all_deputados(ids_deputados) %>%
    dplyr::mutate(nome_civil = tolower(nome_civil) %>% tools::toTitleCase()) %>%
    dplyr::mutate(ultimo_status_nome = tolower(ultimo_status_nome) %>% tools::toTitleCase()) %>%
    dplyr::mutate(ultimo_status_nome_eleitoral = tolower(ultimo_status_nome_eleitoral) %>% tools::toTitleCase())
  readr::write_csv(deputados, paste0(export_path, "/deputados.csv"))

  #Implementar fetch_all_senadores
  print("Buscando senadores...")
} else {

  if (casa == "camara") {

    ids_deputados <- rcongresso::fetch_ids_deputados()
    print("Buscando deputados...")
    deputados <- rcongresso::fetch_all_deputados(ids_deputados)
    readr::write_csv(deputados, paste0(export_path, "/deputados.csv"))

  } else if (casa == "senado") {

    print("Buscando senadores...")

    #Implementar fetch_all_senadores

  } else {
    print("Parâmetro inválido!")
  }

}

