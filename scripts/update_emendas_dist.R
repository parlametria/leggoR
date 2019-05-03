#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_emendas_dist.R <emendas_raw_filepath> <distances_folderpath> <output_filepath>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 3
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
emendas_raw_filepath <- args[1]
distances_folderpath <- args[2]
output_filepath <- args[3]

## Install local repository R package version
devtools::install()

## Read emendas csv, add their distances and export the new emendas csv file
readr::read_csv(emendas_raw_filepath) %>%
  agoradigital::add_distances_to_emendas(distances_folderpath) %>% 
  readr::write_csv(output_filepath)