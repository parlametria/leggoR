#!/usr/bin/env Rscript
library(magrittr)
library(tidyverse)

source(here::here("scripts/entidades/process_entidades.R"))

if (!require(optparse)) {
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

futile.logger::flog.info('Início do processamento de Entidades')
args = commandArgs(trailingOnly = TRUE)

option_list = list(
  make_option(
    c("-p", "--parlamentares_filepath"),
    type = "character",
    default = here::here("data/parlamentares.csv"),
    help = "Caminho do dataframe de parlamentares. [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("-o", "--out"),
    type = "character",
    default = here::here("data/"),
    help = "Caminho do diretório que terá o arquivo de saída [default= %default]",
    metavar = "character"
  )
)

opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

parlamentares_filepath <- opt$parlamentares_filepath
export_path <- opt$out

## Install local repository R package version
devtools::install(upgrade = "never")

if (!str_detect(export_path, "\\/$")) {
  export_path <- paste0(export_path, "/")
}

entidades <- .process_entidades(parlamentares_filepath)

write_csv(entidades, paste0(export_path, "entidades.csv"))
futile.logger::flog.info('Termino do processamento de Entidades')
