library(here)
print(dr_here())
source(here::here("scripts/interesses/process_lista_pls_interesse.R"))

.HELP <- "
Usage:
Rscript export_mapeamento_interesses.R -u <url_interesses> -p <proposicoes_filepath> -e <export_filepath>
url_interesses: Link para o csv com todas as URL's de interesse
proposicoes_filepath: Caminho para o csv de proposições processadas pelo Leggo
export_filepath: Caminho para exportação das do mapeamento das PL's e Interesses
"

#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly=TRUE)

  option_list = list(
    optparse::make_option(c("-u", "--url"),
                          type="character",
                          default="",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-p", "--proposicoes_filepath"),
                          type="character",
                          default="",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--export_filepath"),
                          type="character",
                          default="../../data/interesses.csv",
                          help=.HELP,
                          metavar="character")
  );

  opt_parser <- optparse::OptionParser(option_list = option_list)
  opt <- optparse::parse_args(opt_parser)
  return(opt);
}

## Process args
futile.logger::flog.info('Início do processamento de Interesses')
args <- get_args()
print(args)

url_interesses <- args$url
proposicoes <- args$proposicoes_filepath
saida <- args$export_filepath

print("Processando interesses...")
interesses <- processa_interesses_leggo(url_interesses, proposicoes)

print("Salvando interesses...")
readr::write_csv(interesses, saida)
futile.logger::flog.info('Termino do processamento de Interesses')
