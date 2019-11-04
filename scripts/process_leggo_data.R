#!/usr/bin/env Rscript
library(magrittr)

.HELP <- "
Usage:
Rscript process_leggo_data.R <input_path> <output_path> <data_inicial_documentos> <peso_minimo_arestas> <flag>
flag = 1 Atualiza tudo (Atores, nodes e edges)
flag = 2 Atualiza tudo referente aos atores
flag = 3 Atualiza tudo sobre nodes e edges
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
                          metavar="integer"),
    optparse::make_option(c("-d", "--data_inicial_documentos"), 
                          type="character", 
                          default="2019-01-01",
                          help=.HELP, 
                          metavar="character"),
    optparse::make_option(c("-p", "--peso_minimo_arestas"), 
                          type="character", 
                          default="0.1",
                          help=.HELP, 
                          metavar="character"),
    optparse::make_option(c("-i", "--input_path"), 
                          type="character", 
                          default="../data/",
                          help=.HELP, 
                          metavar="character"),
    optparse::make_option(c("-o", "--output_path"), 
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
## Process args
args <- get_args()
print(args)

input_path <- args$input_path
output_path <- args$output_path
data_inicial <- args$data_inicial_documentos
peso_minimo <- args$peso_minimo_arestas
flag <- args$flag

#' @title Exporta dados atores
#' @description Processa e escreve os dados de atores
#' @param camara_docs documentos da camara
#' @param senados_docs documentos do senado
#' @param camara_autores autores da camara
#' @param senado_autores autores do senado
#' @param output_path pasta para onde exportar os dados
export_atores <- function(camara_docs, camara_autores, senado_docs, senado_autores, output_path) {
  print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))
  
  atores_camara <-
    agoradigital::create_tabela_atores_camara(camara_docs, camara_autores)
  atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores)
  
  atores_df <- dplyr::bind_rows(atores_camara, atores_senado)
  
  readr::write_csv(atores_df, paste0(output_path, '/atores.csv'), na = "")
}

#' @title Exporta dados de nodes e edges
#' @description Processa e escreve os dados de nodes e edges
#' @param input_path pasta para onde importar os dados.
#' @param camara_docs documentos da camara
#' @param data_inicial data a partir da qual se considerará os documentos
#' @param senados_docs documentos do senado
#' @param camara_autores autores da camara
#' @param peso_minimo limiar para peso das arestas
#' @param senado_autores autores do senado
#' @param output_path pasta para onde exportar os dados
export_nodes_edges <- function(input_path, camara_docs, data_inicial, senado_docs, camara_autores, peso_minimo, senado_autores, output_path) {
  print("Gerando tabela de nodes e edges...")
  
  prop <- 
    agoradigital::read_props(paste0(input_path, "/proposicoes.csv")) %>% 
    dplyr::select(id_leggo, id_principal = id_ext, casa)
  
  camara_docs <-
    camara_docs %>%
    dplyr::mutate(data = as.Date(format(status_proposicao_data_hora, "%Y-%m-%d"))) %>%
    dplyr::filter(data > data_inicial) %>%
    dplyr::left_join(prop, by = c("id_principal", "casa"))
  
  senado_docs <-
    senado_docs %>%
    dplyr::filter(data_texto > data_inicial) %>%
    dplyr::left_join(prop, by = c("id_principal", "casa"))
  
  # Gerando dado de autorias de documentos para ambas as casas
  coautorias_camara <- agoradigital::get_coautorias(camara_docs, camara_autores, "camara", as.numeric(peso_minimo))
  coautorias_senado <- agoradigital::get_coautorias(senado_docs, senado_autores, "senado", as.numeric(peso_minimo))
  
  coautorias <- 
    rbind(coautorias_camara, coautorias_senado) %>% 
    dplyr::filter(nome.x != nome.y) %>% 
    dplyr::mutate(partido.x = dplyr::if_else(is.na(partido.x), "", partido.x),
                  partido.y = dplyr::if_else(is.na(partido.y), "", partido.y))
  
  if (nrow(coautorias) != 0) {
    nodes <-
      agoradigital::get_unique_nodes(coautorias)
    
    edges <-
      coautorias %>% 
      dplyr::group_by(id_leggo) %>% 
      dplyr::group_modify(~ agoradigital::generate_edges(., graph_nodes = nodes, edges_weight = 1), keep = T) %>% 
      dplyr::distinct()
    
    nodes <-
      agoradigital::compute_nodes_size(edges, nodes)
    
    readr::write_csv(nodes , paste0(output_path, '/nodes.csv'), na = "")
    readr::write_csv(edges, paste0(output_path, '/edges.csv'), na = "")
  } 
}

#' @title Chama as funções corretas
#' @description Recebe uma flag e chama as funções correspondetes
#' @param flag Inteiro que representa qual função o usuário desejar chamar
process_leggo_data<- function(flag) {
  if (!(flag %in% c(1, 2, 3))) {
    stop(paste("Wrong flag!", .HELP, sep = "\n"))
  }else {
    ## Install local repository R package version
    devtools::install()
    
    # Read current data csvs
    camara_docs <- agoradigital::read_current_docs_camara(paste0(input_path, "/camara/documentos.csv"))
    camara_autores <-
      agoradigital::read_current_autores_camara(paste0(input_path, "/camara/autores.csv")) %>% 
      dplyr::mutate(id_documento = as.numeric(id_documento))
    senado_docs <-
      agoradigital::read_current_docs_senado(paste0(input_path, "/senado/documentos.csv")) %>% 
      dplyr::mutate(id_documento = as.numeric(id_documento),
                    id_principal = as.numeric(id_principal))
    senado_autores <- agoradigital::read_current_autores_senado(paste0(input_path, "/senado/autores.csv"))
    
    if (flag == 1) {
      print("Atualizando tudo!")
      export_atores(camara_docs, camara_autores, senado_docs, senado_autores, output_path)
      export_nodes_edges(input_path, camara_docs, data_inicial, senado_docs, camara_autores, peso_minimo, senado_autores, output_path)
    } else if (flag == 2) {
      print("Atualizando os atores!")
      export_atores(camara_docs, camara_autores, senado_docs, senado_autores, output_path)
    } else{
      print("Atualizando nodes e edges!")
      export_nodes_edges(input_path, camara_docs, data_inicial, senado_docs, camara_autores, peso_minimo, senado_autores, output_path)
    }
  } 
}

process_leggo_data(flag)