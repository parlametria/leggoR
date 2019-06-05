#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_emendas_dist.R <emendas_raw_filepath> <distances_folderpath> <output_filename> <path_para_jus_all_dist>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
num_args <- 4
if (length(args) != num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

emendas_raw_filepath <- args[1]
distances_folderpath <- args[2]
output_filename <- args[3]
path_para_jus_all_dist <- args[4]

## Install local repository R package version
devtools::install()

## Read emendas csv, add their distances and export the new emendas csv file
agoradigital::format_table_distances_to_emendas(path_para_jus_all_dist, distances_folderpath)
readr::read_csv(emendas_raw_filepath) %>%
  agoradigital::add_distances_to_emendas(distances_folderpath) %>% 
  readr::write_csv(output_filename, append = T)