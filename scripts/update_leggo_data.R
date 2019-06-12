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

## Read PLs list
pls_ids <- readr::read_csv(pls_ids_filepath,
                           col_types = list(
                             id_camara = readr::col_double(),
                             id_senado = readr::col_double(),
                             apelido = readr::col_character(),
                             tema = readr::col_character()
                           ))


# Read current proposições
current_props <- readr::read_csv(paste0(export_path,'/proposicoes.csv'),
                                 col_types = list(
                                     id_ext = readr::col_double(),
                                     sigla_tipo = readr::col_character(),
                                     numero = readr::col_double(),
                                     ementa = readr::col_character(),
                                     data_apresentacao = readr::col_datetime(format = ""),
                                     casa = readr::col_character(),
                                     casa_origem = readr::col_character(),
                                     apelido = readr::col_character(),
                                     tema = readr::col_character()
                                 ))

# Check if there are any new proposições
curr_props_ids <- current_props %>%
  dplyr::select(id_principal = id_ext,
                casa)


updated_props <- update_proposicoes(curr_props_ids, pls_ids)

# Identify relacionadas
all_documents <- pls_ids %>%
  purrr::map_df(~ rcongresso::fetch_ids_relacionadas(.x))

# Read current relacionadas
current_relacionadas <- readr::read_csv(paste0(export_path,'/relacionadas.csv'),
                                        col_types = list(
                                          id_principal = readr::col_double(),
                                          id_ext = readr::col_double(),
                                          sigla_tipo = readr::col_character(),
                                          numero = readr::col_double(),
                                          ementa = readr::col_character(),
                                          data_apresentacao = readr::col_datetime(format = ""),
                                          casa = readr::col_character(),
                                          casa_origem = readr::col_character(),
                                          regime_tramitacao = readr::col_character(),
                                          forma_apreciacao = readr::col_character()
                                        ))

new_props <-

 %>%
	dplyr::mutate(row_num = 1:dplyr::n()) %>%
	dplyr::select(row_num,id_camara,id_senado,apelido,tema) %>%
	agoradigital::export_data(export_path)
