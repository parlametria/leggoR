library(tidyverse)
library(futile.logger)
library(here)
source(here::here("scripts/disciplina/process_disciplina.R"))

.HELP <- "
Usage:
Rscript export_disciplina -v <votos_filepath> -o <orientacoes_datapath> -e <export_filepath>
votos_filepath: Caminho para o csv de votos usados no cálculo da disciplina
orientacoes_datapath: Caminho para o csv de oreintações usadas no cálculo da disciplina
export_filepath: Caminho para exportação dos dados de disciplina
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
    optparse::make_option(c("-o", "--orientacoes_filepath"),
                          type="character",
                          default=here::here("inst/extdata/orientacoes.csv"),
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--export_filepath"),
                          type="character",
                          default=here::here("inst/extdata/disciplina.csv"),
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
orientacoes <- args$orientacoes_filepath
saida <- args$export_filepath

flog.info("Processando disciplina...")
disciplina <- processa_disciplina(votos, orientacoes)

flog.info("Salvando dados de disciplina...")
readr::write_csv(disciplina, saida)
flog.info("Salvo!")
