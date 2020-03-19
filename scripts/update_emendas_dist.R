#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_emendas_dist.R <unformatted_distances_folderpath> <distances_folderpath> <emendas_raw_filepath> <processed_emendas_filepath>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
num_args <- 4
if (length(args) != num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

unformatted_distances_folderpath <- args[1]
distances_folderpath <- args[2]
emendas_raw_filepath <- args[3]
processed_emendas_filepath <- args[4]

dir.create(distances_folderpath, showWarnings = FALSE)

## Read emendas csv, add their distances and export the new emendas csv file
files <- list.files(path = unformatted_distances_folderpath,pattern = "*.csv",
                    full.names = TRUE,
                    recursive = FALSE)
emendas_distances_list <- purrr::map(files, ~ agoradigital::format_table_distances_to_emendas(
  distancias_datapath = .x, write_datapath = distances_folderpath))
readr::read_csv(
  emendas_raw_filepath,
  col_types = list(
    id_ext = readr::col_double(),
    codigo_emenda = readr::col_double(),
    data_apresentacao = readr::col_date(format = ""),
    numero = readr::col_double(),
    local = readr::col_character(),
    autor = readr::col_character(),
    casa = readr::col_character(),
    tipo_documento = readr::col_character(),
    inteiro_teor = readr::col_character()
  )
) %>%
  agoradigital::add_distances_to_emendas(distances_folderpath) %>%
  readr::write_csv(processed_emendas_filepath)
