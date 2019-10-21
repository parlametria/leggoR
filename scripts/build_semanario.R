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


pls_ids <- readr::read_csv(args$pls_ids_filepath)

proposicoes <- readr::read_csv("data/proposicoes.csv")
data_inicio <- "2019-09-30" #TODO transformar emm parâmetros
data_fim <- "2019-10-04"
temperaturas <- readr::read_csv("data/hists_temperatura.csv")
leggo_ids <- readr::read_csv("data/leggo_ids.csv")
input_path <- "data"

semana_alvo <- lubridate::floor_date(as.Date(data_inicio), unit = "weeks", week_start = 1)
semana_anterior <- lubridate::floor_date(semana_alvo - 1, unit = "weeks", week_start = 1)


#Calcula variacao das temperaturas
#TODO transformar em função
variacoes_temperatura <- temperaturas %>%
  dplyr::mutate(variacao_absoluta = dplyr::if_else(dplyr::lag(id_leggo) == id_leggo, temperatura_recente - dplyr::lag(temperatura_recente), 0)) %>%
  dplyr::mutate(variacao_percentual = dplyr::if_else((dplyr::lag(id_leggo) == id_leggo & temperatura_recente > 0), ((temperatura_recente - dplyr::lag(temperatura_recente))/temperatura_recente) * 100, 0)) %>%
  dplyr::mutate(variacao_percentual = dplyr::if_else(is.na(variacao_percentual), 0, variacao_percentual)) %>%
  dplyr::filter(periodo == semana_alvo) %>%
  dplyr::mutate(z_score = (variacao_absoluta - mean(variacao_absoluta))/sd(variacao_absoluta))

#Filtra pls de interesse de acordo com a variação da temperatura
#TODO tranformar em função, parametrizar valor mínimo do z-score
pls_de_interesse <- variacoes_temperatura %>%
  dplyr::filter(z_score > 1.5) %>%
  dplyr::left_join(leggo_ids)

#Temperaturas filtradas
temperatura_pls_filtradas <- temperaturas %>%
  dplyr::filter(temperaturas$id_leggo %in% pls_de_interesse$id_leggo)

#Pressões filtradas
pressao_pls_filtradas <- purrr::map_df(pls_de_interesse$id_ext, ~ readr::read_csv(paste0("../leggo-backend/data/pops/pop_", .x, ".csv")))

#Documentos e autores
camara_docs <- agoradigital::read_current_docs_camara(paste0(input_path, "/camara/documentos.csv"))
camara_autores <- agoradigital::read_current_autores_camara(paste0(input_path, "/camara/autores.csv"))
senado_docs <- agoradigital::read_current_docs_senado(paste0(input_path, "/senado/documentos.csv"))
senado_autores <- agoradigital::read_current_autores_senado(paste0(input_path, "/senado/autores.csv"))

print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))

#Gerando atores para semana de interesse (ou faixa de tempo de interesse)
atores_camara <-
  agoradigital::create_tabela_atores_camara(camara_docs, camara_autores, data_inicio, data_fim)
atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores, data_inicio, data_fim)

atores_df <- dplyr::bind_rows(atores_camara, atores_senado)


#' @title Exporta dados de Proposições
#' @description Captura e escreve todos os dados referentes a proposiçoes
#' @param pls_ids_filepath Tabela com os ids das proposições
#' @export
build_semanario <- function(proposicoes, temperatura_pls_filtradas, pressao_pls_filtradas, atores_df, data_inicio, data_fim) {
  rmarkdown::render("reports/semanario/semanario_16-09-2019.Rmd", params = list(
    proposicoes = proposicoes,
    temperaturas = temperatura_pls_filtradas,
    pressoes = pressao_pls_filtradas,
    atores = atores_df,
    date_init = data_inicio,
    date_end = data_fim
  ))
}

build_semanario(proposicoes, temperatura_pls_filtradas, pressao_pls_filtradas, atores_df, data_inicio, data_fim)
