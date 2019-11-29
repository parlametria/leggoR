#!/usr/bin/env Rscript
library(magrittr)

.HELP <- "
Usage:
Rscript fetch_updated_bills_data.R -p <pls_ids_filepath> -e <export_path> -f <flag>
flag = 1 Atualiza tudo (Proposições, emendas e comissões)
flag = 2 Atualiza tudo referente as proposições
flag = 3 Atualiza tudo sobre emendas
flag = 4 Atualiza tudo sobre comissões
"

#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly=TRUE)
  
  option_list = list(
    optparse::make_option(c("-f", "--flag"), 
                          type="integer", 
                          default=1,
                          help=.HELP, 
                          metavar="character"),
    optparse::make_option(c("-p", "--pls_ids_filepath"), 
                          type="character", 
                          default="../data/tabela_geral_ids_casa.csv",
                          help=.HELP, 
                          metavar="character"),
    optparse::make_option(c("-e", "--exporth_path"), 
                          type="character", 
                          default="../../leggo-backend/data/",
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
export_path <- args$exporth_path
flag <- args$flag

#' @title Exporta dados de Proposições
#' @description Captura e escreve todos os dados referentes a proposiçoes
#' @param pls_ids_filepath Tabela com os ids das proposições
#' @param export_path pasta para onde exportar dados.
#' @export
export_props <- function(pls_ids_filepath, export_path) {
  readr::read_csv(pls_ids_filepath) %>%
    dplyr::mutate(row_num = 1:nrow(.)) %>%
    dplyr::select(row_num,id_camara,id_senado,apelido,tema) %>%
    agoradigital::fetch_props(export_path) 
}

#' @title Exporta dados de Emendas
#' @description Captura e escreve as emendas
#' @param pls_ids_filepath Tabela com os ids das proposições
#' @param export_path pasta para onde exportar dados.
#' @export
export_emendas <- function(pls_ids_filepath, export_path) {
  readr::read_csv(pls_ids_filepath) %>%
    agoradigital::fetch_emendas(export_path)
}

#' @title Exporta dados de Comissões
#' @description Captura e escreve as comissões
#' @param export_path pasta para onde exportar dados.
#' @export
export_comissoes <- function(export_path) {
  comissoes <-
    agoradigital::fetch_all_composicao_comissao() %>% 
    dplyr::rename(id_parlamentar = id)
  readr::write_csv(comissoes, paste0(export_path, "/comissoes.csv"))
}

#' @title Chama as funções corretas
#' @description Recebe uma flag e chama as funções correspondetes
#' @param flag Inteiro que representa qual função o usuário desejar chamar
export_dados<- function(flag) {
  if (!(flag %in% c(1, 2, 3, 4))) {
    stop(paste("Wrong flag!", .HELP, sep = "\n"))
  }else {
    ## Install local repository R package version
    devtools::install()
    if (flag == 1) {
      print("Atualizando tudo!")
      export_props(pls_ids_filepath, export_path)
      export_emendas(pls_ids_filepath, export_path)
      export_comissoes(export_path)
    } else if (flag == 2) {
      print("Atualizando as proposições!")
      export_props(pls_ids_filepath, export_path)
    } else if (flag == 3) {
      print("Atualizando emendas!")
      export_emendas(pls_ids_filepath, export_path)
    } else {
      print("Atualizando comissões!")
      export_comissoes(export_path)
    } 
  } 
}

export_dados(flag)


