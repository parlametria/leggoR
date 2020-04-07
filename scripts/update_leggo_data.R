#!/usr/bin/env Rscript
library(magrittr)

.HELP <- "
Usage:
Rscript update_leggo_data.R <pls_ids_filepath> <export_path> <casa>
"

.FILEPATH_HELP <- "
\t   Ex: \"../inst/extdata/tabela_geral_ids_casa.csv\"
\t   PS: Deve conter a pasta referente a casa a ser atualizada no mesmo nível
\t   desse arquivo. Ex: \"../inst/extdata/camara\"
"

.EXPORT_PATH_HELP <- "
\t   Ex: \"../inst/extdata/\"
"

.CASA_HELP <- "
\t   Ex: \"camara\"
"


#' @title Get arguments from command line option parsing
#' @description Get arguments from command line option parsing
get_args <- function() {
  args = commandArgs(trailingOnly=TRUE)

  option_list = list(
    optparse::make_option(c("-p", "--pls_ids_filepath"),
                          type="character",
                          default="../inst/extdata/tabela_geral_ids_casa.csv",
                          help=.FILEPATH_HELP,
                          metavar="character"),
    optparse::make_option(c("-e", "--export_path"),
                          type="character",
                          default="../inst/extdata/",
                          help=.EXPORT_PATH_HELP,
                          metavar="character"),
    optparse::make_option(c("-c", "--casa"),
                          type="character",
                          default="camara",
                          help=.CASA_HELP,
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
min_num_args <- 3
if (length(args) < min_num_args) {
    stop(paste("Wrong number of arguments!", help, sep = "\n"))
}
pls_ids_filepath <- args$pls_ids_filepath
export_path <- args$export_path
casa <- tolower(args$casa)

if (!(casa %in% c("camara", "senado"))) {
  stop("Casa deve ser ou camara ou senado!")
}

## Install local repository R package version
devtools::install()

#current_docs <- tibble::tibble()
current_autores <- tibble::tibble()
deputados <- tibble::tibble()
senadores <- tibble::tibble()

# Read current data csvs
print("Lendo csvs com dados atuais...")
pls_ids <- agoradigital::read_pls_ids(pls_ids_filepath)

dir.create(paste0(export_path, '/', casa), showWarnings = FALSE)

docs_filepath <- paste0(export_path, '/', casa, '/documentos.csv')
autores_filepath <- paste0(export_path, '/', casa, '/autores.csv')
senadores <- agoradigital::read_senadores(paste0(export_path, '/senado/parlamentares.csv'))
deputados <- agoradigital::read_deputados(paste0(export_path, '/camara/parlamentares.csv'))

# Check for new data
all_pls_ids <- agoradigital::get_all_leggo_props_ids(pls_ids)

if (casa == 'senado') {
  print("Realizando atualização dos dados do Senado")

  pls_senado <- all_pls_ids %>%  dplyr::filter(casa == 'senado')

  if (nrow(pls_senado) == 0) {
    warning("Não existe ID para a proposição no Senado")
    quit(save='no')
  }

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
  current_docs <- agoradigital::read_current_docs_camara(docs_filepath)
  current_autores <- agoradigital::read_current_autores_camara(autores_filepath)

  current_docs_ids <-
    current_docs %>%
    dplyr::select(id_documento,
                  id_principal,
                  casa) %>%
    dplyr::mutate(id_documento = as.numeric(id_documento),
                  id_principal = as.numeric(id_principal),
                  casa = as.character(casa))

  print(paste("Verificando se há novos documentos..."))

  new_docs_ids <- agoradigital::find_new_documentos(all_pls_ids, current_docs_ids, casa)

  print(paste("Foram encontrados",nrow(new_docs_ids), "novos documentos."))

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
      quit(save = "no", status=0)
    }

    print(paste("Adicionando ",nrow(new_docs_data)," novos documentos."))

    updated_docs <-
      plyr::rbind.fill(current_docs, new_docs_data %>% dplyr::filter(id_documento %in% complete_docs$id_documento))
    readr::write_csv(updated_docs, docs_filepath)

    print(paste("Adicionando ",nrow(new_autores_data)," autores de novos documentos."))

    simpleCap <- function(x) {
       s <- strsplit(tolower(x), " ")[[1]]
       paste(toupper(substring(s, 1,1)), substring(s, 2),
                     sep="", collapse=" ")
    }
    matched_autores_data <-
      merge(new_autores_data, deputados, by.x = "id_autor", by.y = "id") %>%
      dplyr::mutate(nome = purrr::map_chr(ultimo_status_nome_eleitoral, ~ simpleCap(.x))) %>% 
      dplyr::select(id_autor, nome, tipo_autor,uri_autor,id_documento,casa,partido,uf,cod_tipo_autor)
    updated_autores_docs <-
      plyr::rbind.fill(current_autores, matched_autores_data %>% dplyr::filter(id_documento %in% complete_docs$id_documento))
    readr::write_csv(updated_autores_docs, autores_filepath)
  } else {
    print("Não há documentos novos para essa proposição na Câmara.")
  }
}
