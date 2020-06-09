library(here)
print(dr_here())
source(here::here("scripts/proposicoes/process_proposicao.R"))

.HELP <- "
Usage:
Rscript export_proposicoes_interesse_congresso_remoto.R -u <url_lista_novas_proposicoes> -p <url_lista_proposicoes_atuais> -f <flag_filter_by_regime_tramitacao> -e <export_filepath>
url_lista_novas_proposicoes: Link para o csv com as proposições enviadas pela Pulso
url_lista_proposicoes_atuais: Link para o csv com as proposições já existentes do Congresso Remoto
flag_filter_by_regime_tramitacao: Flag indicando se as proposições devem ser filtradas pelo regime de tramitação urgente
export_filepath: Caminho para exportação das anotação
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
      optparse::make_option(c("-p", "--proposicoes_existentes_url"),
                            type="character",
                            default="",
                            help=.HELP,
                            metavar="character"),
    optparse::make_option(c("-f", "--filter_by_regime_tramitacao"),
                          type="character",
                          default="",
                          help=.HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--export_filepath"),
                          type="character",
                          default="",
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

url_proposicoes_novas <- args$url
url_proposicoes_atuais <- args$proposicoes_existentes_url
filter_by_regime_tramitacao <- args$filter_by_regime_tramitacao
saida <- args$export_filepath

print("Baixando, processando e filtrando proposições...")
flag_filter_by_regime_tramitacao <- if_else(filter_by_regime_tramitacao == 1, 
                                            TRUE, 
                                            FALSE)

proposicoes <- processa_planilha_proposicoes(url_proposicoes_novas, 
                                             url_proposicoes_atuais,
                                             flag_filter_by_regime_tramitacao)

print("Salvando proposições...")
readr::write_csv(proposicoes, saida)
print("Salvo")
