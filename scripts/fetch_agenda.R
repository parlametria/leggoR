#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript fetch_agenda.R <pls_ids_filepath> <initial_date> <end_date> <export_folder_path> <current_pautas_filepath>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 5
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

ids <- args[1]
initial_date <- args[2]
end_date <- args[3]
export_folder_path <- args[4]
current_pautas_filepath <- args[5]

## Install local repository R package version
devtools::install(upgrade = "never")

if (!file.exists(current_pautas_filepath)) {
    pautas <- tibble::tribble( ~data, ~sigla, ~id_ext, ~local, ~casa, ~semana, ~ano)
} else {
    pautas <- readr::read_csv(current_pautas_filepath,
                         col_types = list(
                           .default = readr::col_character(),
                           data = readr::col_datetime(format = ""),
                           id_ext = readr::col_double(),
                           semana = readr::col_double(),
                           ano =readr::col_double()
                         ))
}

## Read PLs list and export their data
pls <- readr::read_csv(ids,
                       col_types = list(
                         .default = readr::col_character(),
                         id_camara = readr::col_double(),
                         id_senado = readr::col_double()
                      ))

## Read PLs list and export their data
agoradigital::extract_pauta(agoradigital::junta_agendas(initial_date, end_date), pls, export_folder_path, pautas)
