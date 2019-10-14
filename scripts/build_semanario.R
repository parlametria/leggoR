#!/usr/bin/env Rscript
library(magrittr)

.HELP <- "
Usage:
Rscript build_semanario.R -p <pls_ids_filepath>
"

#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly=TRUE)
  
  option_list = list(
    optparse::make_option(c("-p", "--pls_ids_filepath"), 
                          type="character", 
                          default="../data/semanario/tabela_geral_ids_casa.csv",
                          help=.HELP, 
                          metavar="character")
  );
  
  opt_parser <- optparse::OptionParser(option_list = option_list) 
  opt <- optparse::parse_args(opt_parser)
  return(opt);
}

## Process args
args <- get_args()
print(args)

pls_ids_filepath <- args$pls_ids_filepath

#' @title Exporta dados de Proposições
#' @description Captura e escreve todos os dados referentes a proposiçoes
#' @param pls_ids_filepath Tabela com os ids das proposições
#' @export
build_semanario <- function(pls_ids_filepath) {
  rmarkdown::render("../reports/semanario/semanario_16-09-2019.Rmd", params = list(
    temperaturas = pls_ids_filepath
  ))
}

build_semanario(pls_ids_filepath)
