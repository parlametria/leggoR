library(here)
print(dr_here())
source(here::here("scripts/interesses/process_lista_pls_interesse.R"))

.HELP <- "
Usage:
Rscript export_pls_leggo.R -u <url_interesses> -e <export_filepath>
url_interesses: Link para o csv com todas as URL's de interesse
export_filepath: Caminho para exportação das PL's a serem analisadas
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
    optparse::make_option(c("-e", "--export_filepath"),
                          type="character",
                          default="../../data/pls_interesses.csv",
                          help=.HELP,
                          metavar="character")
  );

  opt_parser <- optparse::OptionParser(option_list = option_list)
  opt <- optparse::parse_args(opt_parser)
  return(opt);
}

## Process args
print('===============================')
time_init <- Sys.time()
futile.logger::flog.info('Início do processamento de Interesses')
print('===============================')
args <- get_args()
print(args)

url_interesses <- args$url
saida <- args$export_filepath

print("Juntando lista de pls para os interesses...")
pls <- processa_lista_pls_interesses(url_interesses) %>%
  dplyr::distinct(id_camara, id_senado, .keep_all = T)

print("Salvando pls de interesse...")
readr::write_csv(pls, saida)
futile.logger::flog.info('Termino do processamento de Interesses: %g segundos', difftime(Sys.time(), time_init, units = 'secs'))