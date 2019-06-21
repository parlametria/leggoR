#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_leggo_data.R <pls_ids_filepath> <export_path>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 2
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
pls_ids_filepath <- args[1]
export_path <- args[2]

## Install local repository R package version
devtools::install()
library(magrittr)

# Read current data csvs

## Read PLs list
pls_ids <- readr::read_csv(pls_ids_filepath,
                           col_types = list(
                             id_camara = readr::col_double(),
                             id_senado = readr::col_double(),
                             apelido = readr::col_character(),
                             tema = readr::col_character()
                           ))

current_docs <- readr::read_csv(paste0(export_path,'/documentos.csv'),
                                        col_types = list(
                                            .default = readr::col_character(),
                                            id_documento = readr::col_double(),
                                            id_principal = readr::col_double(),
                                            numero = readr::col_integer(),
                                            ano = readr::col_integer(),
                                            data_apresentacao = readr::col_datetime(format = ""),
                                            codTipo = readr::col_integer(),
                                            statusProposicao.codSituacao = readr::col_integer(),
                                            statusProposicao.codTipoTramitacao = readr::col_integer(),
                                            statusProposicao.dataHora = readr::col_datetime(format = ""),
                                            statusProposicao.sequencia = readr::col_integer()
                                        ))

current_autores <- readr::read_csv(paste0(export_path, '/autores.csv'),
                                   col_types = list(
                                     .default = readr::col_character(),
                                     id_autor = readr::col_character(),
                                     nome = readr::col_character(),
                                     cod_tipo = readr::col_integer(),
                                     tipo = readr::col_character(),
                                     uri = readr::col_character(),
                                     id_documento = readr::col_character(),
                                     casa = readr::col_character()
                                   ))

# Check for new data
all_pls_ids <- agoradigital::get_all_leggo_props_ids(pls_ids)

current_docs_ids <- current_docs %>%
  dplyr::select(id_documento,
                id_principal,
                casa)

print(paste("Verificando se há novos documentos..."))

new_docs_ids <- agoradigital::find_new_documentos(all_pls_ids, current_docs)

print(paste("Foram encontrados",nrow(new_docs_ids), "novos documentos."))

if (nrow(new_docs_ids) > 0) {
  new_docs_data <- tibble::tibble()
  new_autores_data <- tibble::tibble()
  
  new_docs_data <- agoradigital::fetch_documentos_data(new_docs_ids) %>%
    dplyr::mutate_all(~ as.character(.))
  
  if (nrow(new_docs_data) == 0) {
    print(paste("Não foi possível baixar os novos documentos encontrados =("))
    quit(status=1)
  }

  print(paste("Adicionando ",nrow(new_docs_data)," novos documentos."))
  updated_docs <- rbind(current_docs, new_docs_data)
  readr::write_csv(updated_docs, paste0(export_path , "/documentos.csv"))

  new_autores_data <- agoradigital::fetch_autores_documentos(new_docs_data) %>%
    dplyr::mutate_all(~ as.character(.))

  print(paste("Adicionando ",nrow(new_autores_data)," novos autores de documentos."))
  updated_autores_docs <- rbind(current_autores, new_autores_data)
  readr::write_csv(updated_autores_docs, paste0(export_path , "/autores.csv"))
}
