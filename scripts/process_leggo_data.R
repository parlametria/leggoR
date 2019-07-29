#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript process_leggo_data.R <input_path> <output_path>
"

#Functions

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 2
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
input_path <- args[1]
output_path <- args[2]

## Install local repository R package version
devtools::install()
library(magrittr)

# Read current data csvs
camara_docs <- agoradigital::read_current_docs_camara(paste0(input_path, "/camara/documentos.csv"))
camara_autores <- agoradigital::read_current_autores_camara(paste0(input_path, "/camara/autores.csv"))
senado_docs <- agoradigital::read_current_docs_senado(paste0(input_path, "/senado/documentos.csv"))
senado_autores <- agoradigital::read_current_autores_senado(paste0(input_path, "/senado/autores.csv"))
senado_docs_scrap <- agoradigital::read_current_docs_senado_scrap(paste0(input_path, "/senado/documentos_scrap.csv"))
senado_autores_scrap <- agoradigital::read_current_autores_senado_scrap(paste0(input_path, "/senado/autores_scrap.csv"))


print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))

atores_camara <- agoradigital::create_tabela_atores_camara(camara_docs, camara_autores)
atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores)
atores_senado_scrap <- agoradigital::create_tabela_atores_senado_scrap(senado_docs_scrap, senado_autores_scrap)

atores_df <- dplyr::bind_rows(atores_camara, atores_senado)

readr::write_csv(atores_df, paste0(output_path, '/atores.csv'), na = "")
