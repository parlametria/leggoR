library(here)
dr_here()
source(here::here("scripts/orientacoes/analyzer_orientacoes.R"))

.HELP <- "
Usage:
Rscript export_orientacoes.R -v <votacoes_filepath> -u <votos_filepath> -o <orientacoes_filepath>
votacoes_filepath: Caminho para o csv de votacoes
votos_filepath: Caminho para o csv de votos
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
    optparse::make_option(c("-u", "--votos_filepath"),
                          type="character",
                          default=here::here("leggo_data/votos.csv"),
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-o", "--orientacoes_filepath"),
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
votos_datapath <- args$votos_filepath
orientacoes_datapath <- args$orientacoes_filepath

print("Processando dados de orientações...")
process_orientacoes(c(2021), votacoes_datapath, votos_datapath, orientacoes_datapath)

print("Salvo")
