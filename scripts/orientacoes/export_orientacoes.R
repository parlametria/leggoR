library(here)
print(dr_here())
source(here::here("scripts/orientacoes/analyzer_orientacoes.R"))

.HELP <- "
Usage:
Rscript export_mapeamento_interesses.R -v <votacoes_filepath> -u <orientacoes_filepath>
votacoes_filepath: Caminho para o csv de votacoes
orientacoes_filepath: Caminho para o csv de orientacoes
"

#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly=TRUE)
  
  option_list = list(
    optparse::make_option(c("-v", "--votacoes_filepath"),
                          type="character",
                          default=here::here("leggo_data/votacoes.csv"),
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-u", "--orientacoes_filepath"),
                          type="character",
                          default=here::here("leggo_data/orientacoes.csv"),
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

votacoes_datapath <- args$votacoes_filepath
orientacoes_datapath <- args$orientacoes_filepath

print("Processando dados de orientações...")
processa_orientacoes(c(2019, 2020, 2021), orientacoes_datapath)

print("Salvo")
