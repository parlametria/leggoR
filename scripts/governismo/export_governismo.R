library(tidyverse)
library(futile.logger)
library(here)
source(here::here("scripts/governismo/process_governismo.R"))

.HELP <- "
Usage:
Rscript export_governismo -v <votos_filepath> -e <export_filepath>
votos_filepath: Caminho para o csv de votos usados no cálculo do governismo
export_filepath: Caminho para exportação dos dados de governismo
"

#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly=TRUE)

  option_list = list(
    optparse::make_option(c("-v", "--votos_filepath"),
                          type="character",
                          default=here::here("inst/extdata/votos.csv"),
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-p", "--votacoes_filepath"),
                          type="character",
                          default=here::here("inst/extdata/votacoes.csv"),
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-i", "--data_inicio"),
                          type="character",
                          default="2019-02-01",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-f", "--data_final"),
                          type="character",
                          default="2022-12-31",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--export_filepath"),
                          type="character",
                          default=here::here("inst/extdata/governismo.csv"),
                          help=.HELP,
                          metavar="character")
  );

  opt_parser <- optparse::OptionParser(option_list = option_list)
  opt <- optparse::parse_args(opt_parser)
  return(opt);
}

args <- get_args()
print(args)

votos <- args$votos_filepath
votacoes <- args$votacoes_filepath
data_inicio <- args$data_inicio
data_final <- args$data_final
saida <- args$export_filepath

flog.info("Processando Governismo...")
governismo <- processa_governismo(votos, votacoes, data_inicio, data_final)

flog.info("Salvando dados de Governismo...")
readr::write_csv(governismo, saida)
flog.info("Salvo!")
