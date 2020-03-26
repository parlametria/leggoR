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
novas_emendas_filepath <- args[3]
processed_emendas_filepath <- args[4]

dir.create(distances_folderpath, showWarnings = FALSE)

## Read emendas csv, add their distances and export the new emendas csv file
files <- list.files(path = unformatted_distances_folderpath,pattern = "*.csv",
                    full.names = TRUE,
                    recursive = FALSE)
emendas_distances_list <- purrr::map(files, ~ agoradigital::format_table_distances_to_emendas(
  distancias_datapath = .x, write_datapath = distances_folderpath))
novas_emendas <- agoradigital::read_novas_emendas(novas_emendas_filepath)
emendas_ja_analisadas <- agoradigital::read_emendas(processed_emendas_filepath)
agoradigital::add_distances_to_emendas(novas_emendas, emendas_ja_analisadas, distances_folderpath) %>%
  readr::write_csv(processed_emendas_filepath)
