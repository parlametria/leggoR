library(here)

source(here::here("scripts/proposicoes/destaques/process_destaques.R"))

.HELP <- "
Usage:
Rscript export_destaques.R -p <proposicoes_filepath> -t <progressos_filepath> -r <tramitacoes_filepath> -i <interesses_filepath> -s <pressao_filepath> -e <export_filepath>
proposicoes_filepath: Caminho para o arquivo de proposições
progressos_filepath: Caminho para o arquivo de progressos das proposições
tramitacoes_filepath: Caminho para o arquivo de tramitações das proposições
interesses_filepath: Caminho para o arquivo de interesses
pressao_filepath: Caminho para o arquivo de pressão das proposições
export_filepath: Caminho para o arquivo de destino.
"

#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly=TRUE)

  option_list = list(
    optparse::make_option(c("-p", "--proposicoes_filepath"),
                          type="character",
                          default="../../../data/proposicoes.csv",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-t", "--progressos_filepath"),
                          type="character",
                          default="../../../data/progressos.csv",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-r", "--tramitacoes_filepath"),
                          type="character",
                          default="../../../data/trams.csv",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-i", "--interesses_filepath"),
                          type="character",
                          default="../../../data/interesses.csv",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-s", "--pressao_filepath"),
                          type="character",
                          default="../../../data/pressao.csv",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--export_filepath"),
                          type="character",
                          default="../../../data/proposicoes_destaques.csv",
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
futile.logger::flog.info('Início do processamento das Proposições de Destaque')
print('===============================')
args <- get_args()
print(args)

proposicoes_filepath <- args$proposicoes_filepath
progressos_filepath <- args$progressos_filepath
tramitacoes_filepath <- args$tramitacoes_filepath
interesses_filepath <- args$interesses_filepath
pressao_filepath <- args$pressao_filepath
saida <- args$export_filepath

print("Processando proposições destaques")
proposicoes_destaques <- process_proposicoes_destaques_limpo(proposicoes_filepath, progressos_filepath, tramitacoes_filepath, interesses_filepath, pressao_filepath)

print("Salvando proposições destaques...")
readr::write_csv(proposicoes_destaques, saida)
futile.logger::flog.info('Termino do processamento das Proposições de Destaque: %g segundos', difftime(Sys.time(), time_init, units = 'secs'))
