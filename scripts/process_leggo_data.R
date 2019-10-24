#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript process_leggo_data.R <input_path> <output_path>
"

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

# Read current data csvs
camara_docs <- agoradigital::read_current_docs_camara(paste0(input_path, "/camara/documentos.csv"))
camara_autores <- agoradigital::read_current_autores_camara(paste0(input_path, "/camara/autores.csv"))
senado_docs <- agoradigital::read_current_docs_senado(paste0(input_path, "/senado/documentos.csv"))
senado_autores <- agoradigital::read_current_autores_senado(paste0(input_path, "/senado/autores.csv"))

print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))

atores_camara <-
  agoradigital::create_tabela_atores_camara(camara_docs, camara_autores)
atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores)

atores_df <- dplyr::bind_rows(atores_camara, atores_senado)

readr::write_csv(atores_df, paste0(output_path, '/atores.csv'), na = "")

print("Gerando tabela de nodes e edges...")

camara_docs <- 
  camara_docs %>% 
  dplyr::mutate(data = as.Date(format(status_proposicao_data_hora, "%Y-%m-%d")))

# Gerando dado de autorias de documentos para ambas as casas
coautorias_camara <- agoradigital::get_coautorias(camara_docs, camara_autores, "camara")
coautorias_senado <- agoradigital::get_coautorias(senado_docs, senado_autores, "senado")

coautorias <- 
  rbind(coautorias_camara, coautorias_senado) %>% 
  dplyr::filter(nome.x != nome.y) %>% 
  dplyr::mutate(partido.x = dplyr::if_else(is.na(partido.x), "", partido.x),
         partido.y = dplyr::if_else(is.na(partido.y), "", partido.y))

if (nrow(coautorias) != 0) {
  nodes_edges <- agoradigital::generate_nodes_and_edges(coautorias)
  readr::write_csv(nodes_edges[[1]], paste0(output_path, '/nodes.csv'), na = "")
  readr::write_csv(nodes_edges[[2]], paste0(output_path, '/edges.csv'), na = "")
} 