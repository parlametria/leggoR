#!/usr/bin/env Rscript
library(magrittr)

.HELP <- "
Usage:
Rscript update_leggo_data.R -f 1 -p <pls_ids_filepath> -e <export_path> -c <casa>
flag = 1 Para rodar do zero
flag = 2 Para atualizar os dados
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
    optparse::make_option(c("-c", "--casa"), 
                          type="character", 
                          default="camara",
                          help=.HELP, 
                          metavar="character"),
    optparse::make_option(c("-e", "--exporth_path"), 
                          type="character", 
                          default="../data/",
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
casa <- tolower(args$casa)

if (!(casa %in% c("camara", "senado"))) {
  stop("Casa deve ser ou camara ou senado!")
}

## Install local repository R package version
devtools::install()

deputados <- tibble::tibble()
senadores <- tibble::tibble()

# Read current data csvs
print("Lendo csvs com dados atuais...")
pls_ids <- agoradigital::read_pls_ids(pls_ids_filepath)

docs_filepath <- paste0(export_path, '/', casa, '/documentos.csv')
autores_filepath <- paste0(export_path, '/', casa, '/autores.csv')
senadores <- agoradigital::read_senadores(paste0(export_path, '/senado/parlamentares.csv'))
deputados <- agoradigital::read_deputados(paste0(export_path, '/camara/parlamentares.csv'))

# Check for new data
all_pls_ids <- agoradigital::get_all_leggo_props_ids(pls_ids)

if (casa == 'senado') {
  print("Realizando atualização dos dados do Senado")
  
  pls_senado <- all_pls_ids %>%  dplyr::filter(casa == 'senado')
  senado_docs <- agoradigital::fetch_documentos_relacionados_senado(pls_senado)
  senado_autores <- agoradigital::fetch_autores_relacionadas_senado(senado_docs)
  
  senado_docs <- 
    senado_docs %>% 
    dplyr::rename(
      id_documento = codigo_texto,
      id_principal = codigo_materia
    )
  
  senado_autores <- 
    senado_autores %>% 
    dplyr::rename(
      id_documento = codigo_texto,
      id_principal = codigo_materia
    )
  
  senado_autores_com_id_autor <- agoradigital::match_autores_senado_to_parlamentares(senado_autores, senadores, deputados)
  readr::write_csv(senado_docs, docs_filepath)
  readr::write_csv(senado_autores_com_id_autor, autores_filepath)
  
}else {
  all_pls_ids <- all_pls_ids %>%  dplyr::filter(casa == 'camara')
  
  if (flag == 1) {
    print(paste("Verificando os documentos..."))
    
    new_docs_ids <- agoradigital::find_new_documentos(all_pls_ids = all_pls_ids, casa_prop = casa)
    
    print(paste("Foram encontrados",nrow(new_docs_ids), "documentos."))
    
  }else {
    current_docs <- agoradigital::read_current_docs_camara(docs_filepath)
    current_autores <- agoradigital::read_current_autores_camara(autores_filepath)
    
    print(paste("Verificando se há novos documentos..."))
    
    new_docs_ids <- agoradigital::find_new_documentos(all_pls_ids, current_docs, casa)
    
    print(paste("Foram encontrados",nrow(new_docs_ids), "novos documentos."))
  }
  
  if (nrow(new_docs_ids) > 0) {
    new_docs_data <- tibble::tibble()
    new_autores_data <- tibble::tibble()
    
    print("Buscando dados sobre os novos documentos...")
    new_docs_data <- 
      agoradigital::fetch_documentos_data(new_docs_ids) %>%
      dplyr::mutate_all(~ as.character(.))
    
    if (nrow(new_docs_data) > 0) {
      print("Buscando os autores dos novos documentos...")
      new_autores_data <- 
        agoradigital::fetch_autores_documentos(new_docs_data) %>%
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
    
    new_autores_data <-
      merge(new_autores_data, deputados, by.x = "id_autor", by.y = "id") %>%
      dplyr::select(id_autor,nome,tipo_autor,uri_autor,id_documento,casa,partido,uf,cod_tipo_autor)
    new_docs_data <- 
      new_docs_data %>% dplyr::filter(id_documento %in% complete_docs$id_documento)
    
    if (flag == 1) {
      readr::write_csv(new_docs_data, docs_filepath)
      readr::write_csv(new_autores_data, autores_filepath)
    }else {
      updated_docs <-
        rbind(current_docs, new_docs_data)
      readr::write_csv(updated_docs, docs_filepath)
      
      print(paste("Adicionando ",nrow(new_autores_data)," autores de novos documentos."))
      
      updated_autores_docs <- 
        rbind(current_autores, new_autores_data %>% dplyr::filter(id_documento %in% complete_docs$id_documento))
      readr::write_csv(updated_autores_docs, autores_filepath)
    }
  }
}