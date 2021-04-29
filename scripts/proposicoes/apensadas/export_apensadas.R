library(tidyverse)
library(futile.logger)

source(here::here("scripts/proposicoes/apensadas/process_apensadas.R"))

if (!require(optparse)) {
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly = TRUE)

option_list = list(
  make_option(
    c("-p", "--proposicoes_filepath"),
    type = "character",
    default = here::here("data/proposicoes.csv"),
    help = "Caminho do dataframe de proposições. [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("-i", "--interesses_filepath"),
    type = "character",
    default = here::here("data/interesses.csv"),
    help = "Caminho do dataframe de interesses processados. [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("-o", "--out"),
    type = "character",
    default = here::here("data/"),
    help = "Caminho do diretório que terá o arquivo de saída: props_apensadas.csv [default= %default]",
    metavar = "character"
  )
)

opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

proposicoes_filepath <- opt$proposicoes_filepath
interesses_filepath <- opt$interesses_filepath
export_path <- opt$out

## Install local repository R package version
devtools::install(upgrade = "never")

if (!str_detect(export_path, "\\/$")) {
  export_path <- paste0(export_path, "/")
}

flog.info("Processando dados de proposições apensadas")
list_props_apensadas <- process_apensadas(proposicoes_filepath, interesses_filepath, export_path)

props_apensadas <- list_props_apensadas[[1]]
props_apensadas_nao_monitoradas <- list_props_apensadas[[2]]

flog.info(str_glue("Salvando dados de proposições apensadas em {export_path}props_apensadas.csv"))
write_csv(props_apensadas, paste0(export_path, "props_apensadas.csv"))

flog.info(str_glue("Salvando dados de proposições apensadas não monitoradas em {export_path}props_apensadas.csv"))
write_csv(props_apensadas_nao_monitoradas, paste0(export_path, "props_apensadas_nao_monitoradas.csv"))

flog.info("Concluído")
