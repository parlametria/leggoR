#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript process_leggo_data.R <input_path> <output_path>
"

#Functions

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 2
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
input_path <- args[1]
output_path <- args[2]

## Install local repository R package version
devtools::install()
library(magrittr)

# Read current data csvs

## Read PLs list
documentos <- readr::read_csv(paste0(input_path,'/documentos.csv'),
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

autores <- readr::read_csv(paste0(input_path, '/autores.csv'),
                                   col_types = list(
                                     .default = readr::col_character(),
                                     id_autor = readr::col_character(),
                                     nome = readr::col_character(),
                                     cod_tipo = readr::col_integer(),
                                     tipo = readr::col_character(),
                                     sigla_tipo = readr::col_character(),
                                     partido = readr::col_character(),
                                     uri = readr::col_character(),
                                     id_documento = readr::col_character(),
                                     casa = readr::col_character()
                                   ))

print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))

atores_df <- create_tabela_atores(documentos, autores)

atores_prop <-
  atores_df %>%
  dplyr::mutate(is_important = sigla_local %in% c(camara_env$comissoes$siglas_comissoes) |
                  stringr::str_detect(tolower(sigla_local), 'pl') |
                                        stringr::str_detect(tolower(sigla_local), 'pec') |
                                                              stringr::str_detect(tolower(sigla_local), 'mpv'))

readr::write_csv(atores_prop, paste0(output_path, '/atores.csv'), na = "")
