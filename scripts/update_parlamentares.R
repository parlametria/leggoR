#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_parlamentares.R <export_path>
"
## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 1
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

export_path <- args[1]

## Install local repository R package version
devtools::install()

# Read current data csvs
deputados <- rcongresso::fetch_all_deputados()

readr::write_csv(deputados, paste0(export_path, "/deputados.csv"))
