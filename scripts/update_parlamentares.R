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
current_deputados <- readr::read_csv(paste0(export_path,'/deputados.csv'),
                                          col_types = list(
                                          .default = readr::col_character(),
                                          data_falecimento = readr::col_date(format = ""),
                                          data_nascimento = readr::col_date(format = ""),
                                          id = readr::col_double(),
                                          ultimo_status_gabinete_andar = readr::col_double(),
                                          ultimo_status_gabinete_sala = readr::col_double(),
                                          ultimo_status_id = readr::col_double(),
                                          ultimo_status_id_legislatura = readr::col_double()
                                    ))
