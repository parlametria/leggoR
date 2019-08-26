#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript update_leggo_data.R <pls_ids_filepath> <export_path> <casa>
"

#Functions

# Analyzes fetched data and returns a list with ids of docs
# whose fetch operation was successful (complete) and not successful (incomplete)
get_fetch_status <- function(docs_ids, docs_data, authors_data) {

  if (nrow(docs_data) == 0 | nrow(authors_data) == 0) {
    return(list(complete_docs = tibble::tibble(), incomplete_docs = docs_ids))
  }

  fetched_data_docs <- docs_data %>%
    dplyr::select(id_documento, casa) %>%
    dplyr::mutate(id_documento = as.numeric(id_documento)) %>%
    dplyr::distinct()

  fetched_autor_docs <- authors_data %>%
    dplyr::select(id_documento, casa) %>%
    dplyr::mutate(id_documento = as.numeric(id_documento)) %>%
    dplyr::distinct()

  complete_docs_df <- dplyr::inner_join(docs_ids,
                                     dplyr::inner_join(fetched_data_docs,fetched_autor_docs,
                                                       by=c("id_documento","casa")),
                                     by=c("id_documento","casa"))

  incomplete_docs_df <- dplyr::anti_join(docs_ids, complete_docs_df,
                                         by=c("id_documento","id_principal","casa"))

  return(list(complete_docs = complete_docs_df, incomplete_docs = incomplete_docs_df))
}

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 3
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
pls_ids_filepath <- args[1]
export_path <- args[2]
casa <- args[3]

## Install local repository R package version
devtools::install()

current_docs <- tibble::tibble()
current_autores <- tibble::tibble()
deputados <- tibble::tibble()
senadores <- tibble::tibble()

# Read current data csvs
print("Lendo csvs com dados atuais...")
pls_ids <- agoradigital::read_pls_ids(pls_ids_filepath)

docs_filepath <- paste0(export_path, '/', casa, '/documentos.csv')
autores_filepath <- paste0(export_path, '/', casa, '/autores.csv')
deputados_filepath <- paste0(export_path, '/camara/parlamentares.csv')
senadores_filepath <- paste0(export_path, '/senado/parlamentares.csv')

deputados <- agoradigital::read_deputados(deputados_filepath)

if (casa == 'camara') {
  current_docs <- agoradigital::read_current_docs_camara(docs_filepath)
  current_autores <- agoradigital::read_current_autores_camara(autores_filepath)
}

if (casa == 'senado') {
  senadores <- agoradigital::read_senadores(senadores_filepath)
  current_docs <- agoradigital::read_current_docs_senado(docs_filepath)
  current_autores <- agoradigital::read_current_autores_senado(autores_filepath)
}

# Check for new data
all_pls_ids <- agoradigital::get_all_leggo_props_ids(pls_ids)

current_docs_ids <- current_docs %>%
  dplyr::select(id_documento,
                id_principal,
                casa)



print(paste("Verificando se há novos documentos..."))

new_docs_ids <- agoradigital::find_new_documentos(all_pls_ids, current_docs, casa)

print(paste("Foram encontrados",nrow(new_docs_ids), "novos documentos."))

if (nrow(new_docs_ids) > 0) {
  new_docs_data <- tibble::tibble()
  new_autores_data <- tibble::tibble()

  print("Buscando dados sobre os novos documentos...")
  new_docs_data <- agoradigital::fetch_documentos_data(new_docs_ids) %>%
    dplyr::mutate_all(~ as.character(.))

  if (nrow(new_docs_data) > 0) {
    print("Buscando os autores dos novos documentos...")
    new_autores_data <- agoradigital::fetch_autores_documentos(new_docs_data) %>%
      dplyr::mutate_all(~ as.character(.))
  }

  fetch_status <- get_fetch_status(new_docs_ids, new_docs_data, new_autores_data)
  complete_docs <- fetch_status$complete_docs
  incomplete_docs <- fetch_status$incomplete_docs

  if (nrow(incomplete_docs) > 0) {
    print("Não foi possível baixar dados completos (proposição e autores) para os seguintes documentos:")
    print(incomplete_docs)
  }

  if (nrow(complete_docs) == 0) {
    print("Não foi possível baixar dados completos (proposição e autores) para nenhum dos novos documentos =(")
    quit(save = "no", status=1)
  }

  print(paste("Adicionando ",nrow(new_docs_data)," novos documentos."))
  updated_docs <- rbind(current_docs, new_docs_data %>% dplyr::filter(id_documento %in% complete_docs$id_documento))
  readr::write_csv(updated_docs, docs_filepath)
  print("Buscando a tramitação dos documentos")
  new_tramitacao_data <- agoradigital::fetch_tramitacao_data(paste0(export_path, "/tramitacoes"), updated_docs) 

  print(paste("Adicionando ",nrow(new_autores_data)," autores de novos documentos."))
  if (casa == 'camara') {
    new_autores_data <- merge(new_autores_data, deputados, by.x = "id_autor", by.y = "id") %>%
      dplyr::select(id_autor,nome,tipo_autor,uri_autor,id_documento,casa,partido,uf,cod_tipo_autor)
  }
  updated_autores_docs <- rbind(current_autores, new_autores_data %>% dplyr::filter(id_documento %in% complete_docs$id_documento))
  readr::write_csv(updated_autores_docs, autores_filepath)
}

if (casa == 'senado') {
  print("Realizando raspagem dos dados no site do senado...")
  pls_senado <- all_pls_ids %>%  dplyr::filter(casa == 'senado')
  senado_docs_scrap <- agoradigital::fetch_documentos_relacionados_senado(pls_senado)
  senado_autores_scrap <- agoradigital::fetch_autores_relacionadas_senado(senado_docs_scrap)
  
  senado_autores_scrap_com_id_autor <- agoradigital::match_autores_senado_scrap_to_parlamentares(senado_autores_scrap, senadores, deputados)
  
  readr::write_csv(senado_docs_scrap, paste0(export_path, "/senado/documentos_scrap.csv"))
  readr::write_csv(senado_autores_scrap_com_id_autor, paste0(export_path, "/senado/autores_scrap.csv"))
}

