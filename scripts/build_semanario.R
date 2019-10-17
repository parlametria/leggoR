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
data_inicio <- "2019-09-30"
data_fim <- "2019-10-04"
temperaturas <- readr::read_csv("data/hists_temperatura.csv")

semana_alvo <- lubridate::floor_date(as.Date(data_inicio), unit = "weeks", week_start = 1)
semana_anterior <- lubridate::floor_date(semana_alvo - 1, unit = "weeks", week_start = 1)

variacoes_temperatura <- temperaturas %>%
  dplyr::mutate(variacao_absoluta = dplyr::if_else(dplyr::lag(id_leggo) == id_leggo, temperatura_recente - dplyr::lag(temperatura_recente), 0)) %>%
  dplyr::mutate(variacao_percentual = dplyr::if_else((dplyr::lag(id_leggo) == id_leggo & temperatura_recente > 0), ((temperatura_recente - dplyr::lag(temperatura_recente))/temperatura_recente) * 100, 0)) %>%
  dplyr::mutate(variacao_percentual = dplyr::if_else(is.na(variacao_percentual), 0, variacao_percentual)) %>%
  dplyr::filter(periodo == semana_alvo) %>%
  dplyr::filter(variacao_percentual > 0)

# Read current data csvs
camara_docs <- agoradigital::read_current_docs_camara(paste0(input_path, "/camara/documentos.csv"))
camara_autores <- agoradigital::read_current_autores_camara(paste0(input_path, "/camara/autores.csv"))
senado_docs <- agoradigital::read_current_docs_senado(paste0(input_path, "/senado/documentos.csv"))
senado_autores <- agoradigital::read_current_autores_senado(paste0(input_path, "/senado/autores.csv"))

print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))

atores_camara <-
  agoradigital::create_tabela_atores_camara(camara_docs, camara_autores, data_inicio, data_fim)
atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores, data_inicio, data_fim)

atores_df <- dplyr::bind_rows(atores_camara, atores_senado)


#' @title Exporta dados de Proposições
#' @description Captura e escreve todos os dados referentes a proposiçoes
#' @param pls_ids_filepath Tabela com os ids das proposições
#' @export
build_semanario <- function(pls_ids_filepath) {
  rmarkdown::render("../reports/semanario/semanario_16-09-2019.Rmd", params = list(
    atores = atores_df,
    proposicoes = pls_ids_filepath,
    date_init = data_inicio,
    date_end = data_fim
  ))
}

build_semanario(pls_ids_filepath)
