library(here)
print(dr_here())
source(here::here("scripts/anotacoes/process_anotacoes.R"))

.HELP <- "
Usage:
Rscript export_anotacoes.R -u <url_lista_anotacoes> -i <pls_interesses_filepath> -p <proposicoes_filepath> -e <export_path>
url_lista_anotacoes: Link para o csv com todas as URL's contendo as anotações para todos os interesses
pls_interesses_filepath: Caminho para o csv com o mapeamento de pls a interesses
proposicoes_filepath: Caminho para o csv de proposições processadas pelo Leggo
export_path: Caminho do diretório destino para exportação das anotações gerais e específicas
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
    optparse::make_option(c("-i", "--pls_interesses_filepath"),
                          type="character",
                          default="",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-p", "--proposicoes_filepath"),
                          type="character",
                          default="",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--export_path"),
                          type="character",
                          default="../../data/",
                          help=.HELP,
                          metavar="character")
  );
  
  opt_parser <- optparse::OptionParser(option_list = option_list)
  opt <- optparse::parse_args(opt_parser)
  return(opt);
}

## Process args
futile.logger::flog.info('Início do processamento dos dados de Anotações')
args <- get_args()
print(args)

url_lista_anotacoes <- args$url
pls_interesses <- args$pls_interesses_filepath
proposicoes <- args$proposicoes_filepath
export_path <- args$export_path

print("Baixando e processando os dados de anotações...")
anotacoes <- processa_anotacoes(url_lista_anotacoes, pls_interesses, proposicoes)

print("Salvando anotacoes...")
readr::write_csv(anotacoes$anotacoes_gerais, paste0(export_path, "/anotacoes_gerais.csv"))
readr::write_csv(anotacoes$anotacoes_especificas, paste0(export_path, "/anotacoes_especificas.csv"))
futile.logger::flog.info('Termino do processamento dos dados de Anotações')
