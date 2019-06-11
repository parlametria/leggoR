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

## Read emendas csv, add their distances and export the new emendas csv file
files <- list.files(path=unformatted_distances_folderpath, pattern="*.csv", full.names=TRUE, recursive=FALSE)
emendas_distances_list <- purrr::map(files, ~ agoradigital::format_table_distances_to_emendas(distancias_datapath = .x, write_datapath=distances_folderpath))
readr::read_csv(emendas_raw_filepath) %>%
  agoradigital::add_distances_to_emendas(distances_folderpath) %>% 
  readr::write_csv(processed_emendas_filepath)
