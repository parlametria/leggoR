#!/usr/bin/env Rscript
library(tidyverse)

source(here::here("scripts/parlamentares/process_parlamentares.R"))

if (!require(optparse)) {
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly = TRUE)

message("Use --help para mais informações\n")
option_list = list(
  make_option(
    c("-p", "--parlamentares"),
    type = "character",
    help = "Caminho do diretório onde está o arquivo csv de parlamentares",
    metavar = "character"
  )
)

opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

parlamentares_folderpath <- opt$parlamentares

if (!str_detect(parlamentares_folderpath, "\\/$")) {
  parlamentares_folderpath <- paste0(parlamentares_folderpath, "/")
}

parlamentares_filepath <-
  paste0(parlamentares_folderpath, "parlamentares.csv")

print("Atualizando informações dos parlamentares das últimas legislaturas...")

parlamentares <- .update_parlamentares(parlamentares_filepath)
write_csv(parlamentares, parlamentares_filepath)

print("Feito!")
