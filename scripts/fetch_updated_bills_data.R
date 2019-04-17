#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript fetch_updated_bills_data.R <pls_ids_filepath> <distances_folderpath> <export_path>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 3
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
pls_ids_filepath <- args[1]
distances_folderpath <- args[2]
export_path <- args[3]

## Install local repository R package version
devtools::install()

## Read PLs list and export their data
readr::read_csv(pls_ids_filepath) %>%
	dplyr::mutate(row_num = 1:dplyr::n()) %>%
	dplyr::select(row_num,id_camara,id_senado,apelido,tema) %>%
	agoradigital::export_data(export_path, distances_folderpath)
