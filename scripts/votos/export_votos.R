library(here)
print(dr_here())
source(here::here("scripts/votos/analyzer_votos.R"))

.HELP <- "
Usage:
Rscript export_mapeamento_interesses.R -v <votacoes_filepath> -u <votos_filepath>
votacoes_filepath: Caminho para o csv de votações
votos_filepath: Caminho para o csv de votos
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
    optparse::make_option(c("-p", "--proposicoes_filepath"),
                          type="character",
                          default=here::here("leggo_data/proposicoes.csv"),
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--entidades_filepath"),
                          type="character",
                          default=here::here("leggo_data/entidades.csv"),
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
proposicoes_datapath <- args$proposicoes_filepath
entidades_datapath <- args$entidades_filepath

print("Processando dados de votações e votos...")
processa_votos(c(2021), votacoes_datapath, votos_datapath, proposicoes_datapath, entidades_datapath)

print("Salvo")
