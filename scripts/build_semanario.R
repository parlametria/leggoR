#!/usr/bin/env Rscript
library(magrittr)

.help <- "
Usage:
Rscript build_semanario.R <data_inicio> <data_fim> <input_base_folderpath> <pops_base_folderpath>
"

#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
# get_args <- function() {
#   args = commandArgs(trailingOnly=TRUE)
# 
#   option_list = list(
#     optparse::make_option(c("-p", "--pls_ids_filepath"),
#                           type="character",
#                           default="../data/semanario/tabela_geral_ids_casa.csv",
#                           help=.HELP,
#                           metavar="character")
#   );
# 
#   opt_parser <- optparse::OptionParser(option_list = option_list)
#   opt <- optparse::parse_args(opt_parser)
#   return(opt);
# }

is_package_installed <- function(pkg_name, rep_url)
{
  if (!require(pkg_name,character.only = TRUE))
  {
    install.packages(pkg_name,dep=TRUE, repos = rep_url)
    if(!require(pkg_name,character.only = TRUE)) stop("Package not found")
  }
}

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 4
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", .help, sep = "\n"))
}
data_inicio <- args[1]
data_fim <- args[2]
input_base_folderpath <- args[3]
pops_base_folderpath <- args[4]


#devtools::install()

is_package_installed(pkg_name = "ggchicklet", rep_url = "https://cinc.rud.is")



num_semanas_passadas <<- 3

#Lê dados básicos sobre as proposições e temperatura
proposicoes <- readr::read_csv(paste0(input_base_folderpath,'/','proposicoes.csv'))
temperaturas <- readr::read_csv(paste0(input_base_folderpath,'/','hists_temperatura.csv'))
leggo_ids <- readr::read_csv(paste0(input_base_folderpath,'/','leggo_ids.csv'))

#Define semanas a serem analisadas
semana_alvo <- lubridate::floor_date(as.Date(data_inicio), unit = "weeks", week_start = 1)
semana_anterior <- semana_alvo - lubridate::weeks(x=1)


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
  dplyr::left_join(leggo_ids, by="id_leggo")

#Temperaturas filtradas
temperatura_pls_filtradas <- temperaturas %>%
  dplyr::filter(id_leggo %in% pls_de_interesse$id_leggo) %>% 
  dplyr::filter(periodo <= semana_alvo) %>% 
  dplyr::select(-temperatura_periodo) %>% 
  dplyr::rename(temperatura = temperatura_recente)

#Pressões filtradas
pressao_pls_filtradas <- purrr::map_df(pls_de_interesse$id_ext, ~ readr::read_csv(paste0(pops_base_folderpath,"/pop_", .x, ".csv"))) %>%
  dplyr::select(-dplyr::starts_with("X")) %>% 
  dplyr::left_join(leggo_ids, by="id_ext") %>% 
  dplyr::group_by(id_leggo, id_ext, casa, date) %>% 
  dplyr::summarise(pressao = max(maximo_geral)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(id_leggo, periodo = date, pressao)

#Documentos e autores
camara_docs <- agoradigital::read_current_docs_camara(paste0(input_base_folderpath, "/camara/documentos.csv"))
camara_autores <- agoradigital::read_current_autores_camara(paste0(input_base_folderpath, "/camara/autores.csv"))
senado_docs <- agoradigital::read_current_docs_senado(paste0(input_base_folderpath, "/senado/documentos.csv"))
senado_autores <- agoradigital::read_current_autores_senado(paste0(input_base_folderpath, "/senado/autores.csv"))

print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))

#Gerando atores para semana de interesse (ou faixa de tempo de interesse)
atores_camara <-
  agoradigital::create_tabela_atores_camara(camara_docs, camara_autores, data_inicio, data_fim)
atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores, data_inicio, data_fim)

atores_df <- dplyr::bind_rows(atores_camara, atores_senado) %>% 
  dplyr::left_join(leggo_ids, by="id_ext")


leggo_ids_selecionados <- dplyr::bind_rows(dplyr::select(temperatura_pls_filtradas, id_leggo),
                                     dplyr::select(pressao_pls_filtradas, id_leggo),
                                     dplyr::select(atores_df, id_leggo)) %>% 
  dplyr::distinct()

proposicoes_filtradas <- proposicoes %>% dplyr::filter(id_leggo %in% leggo_ids_selecionados$id_leggo)
  

#' @title Exporta dados de Proposições
#' @description Captura e escreve todos os dados referentes a proposiçoes
#' @param output_filepath
#' @param proposicoes
#' @param temperatura_pls_filtradas
#' @param pressao_pls_filtradas
#' @param atores_df
#' @param data_inicio
#' @param data_fim
#' @export
build_semanario <- function(template_filepath,output_filepath, proposicoes, temperatura_pls_filtradas, 
                            pressao_pls_filtradas, atores_df, data_inicio, data_fim, num_semanas_passadas) {
  rmarkdown::render(input = template_filepath, 
                    output_file = output_filepath,
                    params = list(
                              proposicoes = proposicoes,
                              temperaturas = temperatura_pls_filtradas,
                              pressoes = pressao_pls_filtradas,
                              atores = atores_df,
                              date_init = data_inicio,
                              date_end = data_fim,
                              num_semanas_passadas = num_semanas_passadas
                             ))
}

template_filepath <- "reports/semanario/semanario_template.Rmd"
report_filepath <- paste0("semanario_",semana_alvo,".html")

build_semanario(template_filepath, report_filepath, proposicoes_filtradas, temperatura_pls_filtradas, 
                pressao_pls_filtradas, atores_df, data_inicio, data_fim, num_semanas_passadas)
