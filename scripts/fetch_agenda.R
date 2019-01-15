#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript fetch_agenda.R <pls_ids_filepath> <initial_date> <end_date> <export_path>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 4
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

ids <- args[1]
initial_date <- args[2]
end_date <- args[3]
export_path <- args[4]

## Install local repository R package version
devtools::install()

## Read PLs list and export their data
agoradigital::extract_pauta(agoradigital::junta_agendas(initial_date, end_date), readr::read_csv(ids), export_path)
