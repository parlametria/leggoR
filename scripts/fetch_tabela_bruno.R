#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript fetch_tabela_bruno.R <pls_ids_bruno> <export_file_path>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 2
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

ids <- args[1]
export_file_path <- args[2]

auxilia_criacao_tabela_a_partir_sigla <- function(sigla_completa, sim, nao, total) {
  array = strsplit(sigla_completa, " ")
  sigla = array[[1]][[1]]
  numero = strsplit(array[[1]][[2]], "/")[[1]][[1]]
  ano = strsplit(array[[1]][[2]], "/")[[1]][[2]]
  print(paste0(sigla, " ", numero, " ", ano))
  rcongresso::fetch_proposicao_senado_sigla(sigla, numero, ano) %>% 
    dplyr::mutate(sim = as.character(sim), nao = as.character(nao), total = as.character(total))
}

bruno_pls <- readr::read_csv(ids)
bruno_processado <-
  purrr::pmap_df(list(bruno_pls$Proposição, bruno_pls$Sim, bruno_pls$Não, bruno_pls$Total), function(a, b, c, d) auxilia_criacao_tabela_a_partir_sigla(a, b, c, d))
readr::write_csv(bruno_processado, export_file_path)
