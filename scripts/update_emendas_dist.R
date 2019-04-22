#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_emendas_list.R <emendas_filepath> <distances_folderpath>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 2
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
emendas_filepath <- args[1]
distances_folderpath <- args[2]

## Install local repository R package version
devtools::install()

## Read emendas csv, add their distances and export the new emendas csv file
readr::read_csv(emendas_filepath) %>%
  agoradigital::add_distances_to_emendas(distances_folderpath) %>% 
  readr::write_csv(emendas_filepath)