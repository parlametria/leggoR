#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript fetch_updated_bills_data.R <pls_ids_filepath> <export_path>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 2
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
pls_ids_filepath <- args[1]
export_path <- args[2]

## Read PLs list and export their data
readr::read_csv(pls_ids_filepath) %>% agoradigital::export_data(export_path)
