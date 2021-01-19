#!/usr/bin/env Rscript
library(tidyverse)
library(magrittr)
source(here::here("scripts/documentos/export_atuacao.R"))
source(here::here("scripts/documentos/export_coautorias.R"))
source(here::here("scripts/documentos/export_emendas.R"))

.HELP <- "
Rscript process_leggo_data.R -i <input_path> -o <output_path> -d <data_inicial_documentos> -p <peso_minimo_arestas> -f <flag>
"

.FLAG_HELP <- "
\t   flag = 1 Atualiza tudo (Atuacao, nodes e edges, emendas)
\t   flag = 2 Atualiza tudo referente aos atuacao
\t   flag = 3 Atualiza tudo sobre nodes e edges
\t   flag = 4 Atualiza tudo sobre emendas
"

.DATE_HELP <- "
\t   Ex: \"2019-01-01\"
"

.PESO_HELP <- "
\t   Ex: 0.1
"

.INPUT_HELP <- "
\t   Ex: \"../inst/extdata/\"
"

.OUTPUT_HELP <- "
\t   Ex: \"../inst/extdata/\"
"


#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly=TRUE)

  option_list = list(
    optparse::make_option(c("-f", "--flag"),
                          type="integer",
                          default=1,
                          help=.FLAG_HELP,
                          metavar="integer"),
    optparse::make_option(c("-d", "--data_inicial_documentos"),
                          type="character",
                          default="2019-01-01",
                          help=.DATE_HELP,
                          metavar="character"),
    optparse::make_option(c("-p", "--peso_minimo_arestas"),
                          type="character",
                          default="0.1",
                          help=.PESO_HELP,
                          metavar="character"),
    optparse::make_option(c("-i", "--input_path"),
                          type="character",
                          default="../inst/extdata/",
                          help=.INPUT_HELP,
                          metavar="character"),
    optparse::make_option(c("-o", "--output_path"),
                          type="character",
                          default="../inst/extdata/",
                          help=.OUTPUT_HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--entidades_path"),
                          type="character",
                          default="../inst/extdata/entidades.csv",
                          help=.OUTPUT_HELP,
                          metavar="character")
  );

  opt_parser <- optparse::OptionParser(option_list = option_list, usage = .HELP)
  opt <- optparse::parse_args(opt_parser)
  return(opt);
}

## Process args
## Process args
args <- get_args()
print(args)

input_path <- args$input_path
output_path <- args$output_path
data_inicial <- args$data_inicial_documentos
peso_minimo <- as.numeric(args$peso_minimo_arestas)
flag <- args$flag
entidades_path <- args$entidades_path

.PARTIDOS_OPOSICAO <-
  c("PT", "PSOL", "PSB", "PCdoB", "PDT", "REDE")

#' @title Chama as funções corretas
#' @description Recebe uma flag e chama as funções correspondetes
#' @param flag Inteiro que representa qual função o usuário desejar chamar
process_leggo_data <- function(flag) {
  if (!(flag %in% c(1, 2, 3,4))) {
    stop(paste("Wrong flag!", .HELP, sep = "\n"))
  } else {
    ## Install local repository R package version
    devtools::install(upgrade = "never")
    
    props_leggo_id <-
      agoradigital::read_props(paste0(input_path, "/proposicoes.csv")) %>%
      dplyr::select(id_leggo, id_principal = id_ext, casa)
    
    # Read current data csvs
    camara_docs <- agoradigital::read_current_docs_camara(paste0(input_path, "/camara/documentos.csv")) %>%
      dplyr::mutate(casa = as.character(casa)) %>% 
      dplyr::inner_join(props_leggo_id, by = c("id_principal", "casa"))
    
    camara_autores <-
      agoradigital::read_current_autores_camara(paste0(input_path, "/camara/autores.csv")) %>%
      dplyr::mutate(id_documento = as.numeric(id_documento),
                    id_principal = as.numeric(id_principal)) %>%
      dplyr::inner_join(props_leggo_id, by = c("id_principal", "casa")) %>% 
      rowwise() %>%
      dplyr::mutate(nome = agoradigital::formata_nome_deputados(nome, tipo_autor)) %>%
      ungroup()

    senado_docs <-
      agoradigital::read_current_docs_senado(paste0(input_path, "/senado/documentos.csv")) %>%
      dplyr::mutate(id_documento = as.numeric(id_documento),
                    id_principal = as.numeric(id_principal),
                    casa = as.character(casa)) %>% 
      dplyr::inner_join(props_leggo_id, by = c("id_principal", "casa"))
    
    senado_autores <-
      agoradigital::read_current_autores_senado(paste0(input_path, "/senado/autores.csv")) %>%
      dplyr::inner_join(props_leggo_id, by = c("id_principal", "casa")) %>% 
      rowwise() %>%
      dplyr::mutate(nome_autor =
                      agoradigital::formata_nome_senadores(nome_autor, tipo_autor)) %>%
      ungroup()
    
    entidades <- readr::read_csv(entidades_path)

    if (flag == 1) {
      print("Atualizando tudo!")
      export_atuacao(camara_docs, camara_autores, senado_docs, senado_autores, output_path, data_inicial, peso_minimo, props_leggo_id, entidades)
      export_nodes_edges(input_path, camara_docs, data_inicial, senado_docs, camara_autores, peso_minimo, senado_autores, props_leggo_id, output_path)
      novas_emendas = export_emendas(camara_docs, camara_autores, senado_docs, senado_autores, output_path)
      export_avulsos_iniciais(camara_docs, senado_docs, novas_emendas, output_path)
    } else if (flag == 2) {
      print("Atualizando os atuação!")
      export_atuacao(camara_docs, camara_autores, senado_docs, senado_autores, output_path, data_inicial, peso_minimo, props_leggo_id, entidades)
    } else if (flag == 3) {
      print("Atualizando nodes e edges!")
      export_nodes_edges(input_path, camara_docs, data_inicial, senado_docs, camara_autores, peso_minimo, senado_autores, props_leggo_id, output_path)
    } else if (flag == 4) {
      print("Atualizando dados de emendas e avulsos iniciais")
      novas_emendas = export_emendas(camara_docs, camara_autores, senado_docs, senado_autores, output_path)
      export_avulsos_iniciais(camara_docs, senado_docs, novas_emendas, output_path)
    } else {
      print(paste("Flag inexistente:",flag))
      print(.HELP)
      quit()
    }
  }
}

process_leggo_data(flag)
