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

all_pls_ids <- agoradigital::get_all_leggo_props_ids(pls_ids)

# # Read current proposições
# current_props <- readr::read_csv(paste0(export_path,'/proposicoes.csv'),
#                                  col_types = list(
#                                      id_ext = readr::col_double(),
#                                      sigla_tipo = readr::col_character(),
#                                      numero = readr::col_double(),
#                                      ano = readr::col_character(),
#                                      ementa = readr::col_character(),
#                                      data_apresentacao = readr::col_datetime(format = ""),
#                                      casa = readr::col_character(),
#                                      casa_origem = readr::col_character(),
#                                      apelido = readr::col_character(),
#                                      tema = readr::col_character()
#                                  ))
#
# # Check if there are any new proposições
# curr_props_ids <- current_props %>%
#   dplyr::select(id_principal = id_ext,
#                 casa)
#
#
# updated_props <- update_proposicoes(curr_props_ids, pls_ids) %>%
#   dplyr::rename(apelido = apelido_materia, id_ext = prop_id) %>%
#   select(-autor_nome)
#
# # Identify relacionadas
# all_documents <- pls_ids %>%
#   purrr::map_df(~ rcongresso::fetch_ids_relacionadas(.x))

# Read current relacionadas
current_relacionadas <- readr::read_csv(paste0(export_path,'/relacionadas.csv'),
                                        col_types = list(
                                            .default = readr::col_character(),
                                            id_relacionada = readr::col_double(),
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

current_relacionadas_ids <- current_relacionadas %>%
  dplyr::select(id_relacionada,
                id_principal,
                casa)

new_relacionadas_ids <- agoradigital::find_new_relacionadas(all_pls_ids, current_relacionadas_ids)
new_relacionadas_data <- tibble::tibble()

if (nrow(new_relacionadas_ids) > 0) {
  new_relacionadas_data <- agoradigital::fetch_relacionadas_data(new_relacionadas_ids) %>%
    dplyr::mutate_all(~ as.character(.))

  print(paste("Adicionando ",nrow(new_relacionadas_data)," novas matérias relacionadas."))
  updated_relacionadas <- rbind(current_relacionadas, new_relacionadas_data)
  readr::write_csv(updated_relacionadas, paste0(export_path , "/relacionadas.csv"))
}



