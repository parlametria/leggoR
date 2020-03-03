#!/usr/bin/env Rscript
library(magrittr)

.HELP <- "
Rscript process_leggo_data.R -i <input_path> -o <output_path> -d <data_inicial_documentos> -p <peso_minimo_arestas> -f <flag>
"

.FLAG_HELP <- "
\t   flag = 1 Atualiza tudo (Atores, nodes e edges)
\t   flag = 2 Atualiza tudo referente aos atores
\t   flag = 3 Atualiza tudo sobre nodes e edges
"

.DATE_HELP <- "
\t   Ex: \"2019-01-01\"
"

.PESO_HELP <- "
\t   Ex: 0.1
"

.INPUT_HELP <- "
\t   Ex: \"../data/\"
"

.OUTPUT_HELP <- "
\t   Ex: \"../leggo-backend/data/\"
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
                          default="../data/",
                          help=.INPUT_HELP,
                          metavar="character"),
    optparse::make_option(c("-o", "--output_path"),
                          type="character",
                          default="../../leggo-backend/data/",
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

.PARTIDOS_OPOSICAO <-
  c("PT", "PSOL", "PSB", "PCdoB", "PDT", "REDE")

#' @title Exporta dados atores
#' @description Processa e escreve os dados de atores
#' @param camara_docs documentos da camara
#' @param senados_docs documentos do senado
#' @param camara_autores autores da camara
#' @param senado_autores autores do senado
#' @param data_inicial data a partir da qual se considerará os documentos
#' @param peso_minimo limiar para peso dos documentos
#' @param output_path pasta para onde exportar os dados
export_atores <- function(camara_docs, camara_autores, senado_docs, senado_autores, output_path, data_inicio, peso_minimo, props_leggo_id) {
  print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))

  atores_camara <-
    agoradigital::create_tabela_atores_camara(camara_docs, camara_autores, data_inicio = data_inicio, limiar = peso_minimo)
  atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores, data_inicio = data_inicio, limiar = peso_minimo)

  if ((nrow(atores_camara) > 0) | (nrow(atores_senado) > 0)) {
    atores_df <-
      dplyr::bind_rows(atores_camara, atores_senado) %>%
      dplyr::mutate(bancada = dplyr::if_else(partido %in% .PARTIDOS_OPOSICAO, "oposição", "governo")) %>%
      dplyr::left_join(props_leggo_id, by = c("id_ext"="id_principal", "casa")) %>%
      dplyr::select(id_leggo, dplyr::everything())
  } else {
    atores_df <- tibble::tribble(~id_leggo, ~id_ext, ~casa, ~id_autor, ~tipo_autor, ~tipo_generico, ~sigla_local,
                                 ~peso_total_documentos, ~num_documentos, ~partido, ~uf, ~nome_autor, ~is_important, ~bancada)
  }

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
export_nodes_edges <- function(input_path, camara_docs, data_inicial, senado_docs, camara_autores, peso_minimo, senado_autores, props_leggo_id, output_path) {
  coautorias_camara <- tibble::tibble()
  autorias_camara <- tibble::tibble()
  coautorias_senado <- tibble::tibble()
  autorias_senado <- tibble::tibble()

  print("Gerando tabela de nodes e edges...")

  camara_docs <-
    camara_docs %>%
    dplyr::mutate(data = lubridate::ymd(status_proposicao_data_hora)) %>%
    dplyr::filter(data > data_inicial) %>%
    dplyr::left_join(props_leggo_id, by = c("id_principal", "casa"))

  senado_docs <-
    senado_docs %>%
    dplyr::filter(data_texto > data_inicial) %>%
    dplyr::left_join(props_leggo_id, by = c("id_principal", "casa"))

  # Gerando dado de autorias de documentos para ambas as casas
  if (nrow(camara_docs) > 0) {
    coautorias_camara_list <- agoradigital::get_coautorias(camara_docs, camara_autores, "camara", as.numeric(peso_minimo), .PARTIDOS_OPOSICAO)
    coautorias_camara <- coautorias_camara_list$coautorias
    autorias_camara <- coautorias_camara_list$autorias
  }

  if (nrow(senado_docs) > 0) {
    coautorias_senado_list <- agoradigital::get_coautorias(senado_docs, senado_autores, "senado", as.numeric(peso_minimo), .PARTIDOS_OPOSICAO)
    coautorias_senado <- coautorias_senado_list$coautorias
    autorias_senado <- coautorias_senado_list$autorias
  }
  
  if ((nrow(coautorias_camara) > 0) | (nrow(coautorias_senado) > 0)) {
    coautorias <-
      rbind(coautorias_camara, coautorias_senado) %>%
      dplyr::mutate(partido.x = dplyr::if_else(is.na(partido.x), "", partido.x),
                    partido.y = dplyr::if_else(is.na(partido.y), "", partido.y))
  
    autorias <-
      rbind(autorias_camara, autorias_senado) %>% 
      dplyr::group_by(id_documento, id_autor) %>% 
      dplyr::summarise(
                descricao_tipo_documento = dplyr::first(descricao_tipo_documento),
                data = dplyr::first(data),
                url_inteiro_teor = dplyr::first(url_inteiro_teor),
                id_leggo = dplyr::first(id_leggo),
                nome_eleitoral = dplyr::first(nome_eleitoral)) %>% 
      dplyr::mutate(autores = paste0(nome_eleitoral, collapse = ", "))

    nodes <-
      agoradigital::get_unique_nodes(coautorias)

    edges <-
      coautorias %>%
      dplyr::group_by(id_leggo) %>%
      dplyr::group_modify(~ agoradigital::generate_edges(., graph_nodes = nodes, edges_weight = 1), keep = T) %>%
      dplyr::distinct()

    nodes <-
      agoradigital::compute_nodes_size(edges, nodes)

    edges <-
      edges %>%
      dplyr::filter(source != target)
  } else {
    nodes <- tibble::tribble(~id_leggo, ~id_autor, ~nome, ~partido, ~uf, ~bancada, ~nome_eleitoral, ~node_size)
    edges <- tibble::tribble(~id_leggo, ~source, ~target, ~value)
    autorias <- tibble::tribble(~id_documento, ~id_autor, ~descricao_tipo_documento, ~data, ~url_inteiro_teor, ~id_leggo, ~nome_eleitoral, ~autores)
  }
  
  readr::write_csv(nodes , paste0(output_path, '/coautorias_nodes.csv'), na = "")
  readr::write_csv(edges, paste0(output_path, '/coautorias_edges.csv'), na = "")
  readr::write_csv(autorias, paste0(output_path, '/autorias.csv'))
}

#' @title Chama as funções corretas
#' @description Recebe uma flag e chama as funções correspondetes
#' @param flag Inteiro que representa qual função o usuário desejar chamar
process_leggo_data <- function(flag) {
  if (!(flag %in% c(1, 2, 3))) {
    stop(paste("Wrong flag!", .HELP, sep = "\n"))
  }else {
    ## Install local repository R package version
    devtools::install()

    # Read current data csvs
    camara_docs <- agoradigital::read_current_docs_camara(paste0(input_path, "/camara/documentos.csv")) %>% 
      dplyr::mutate(casa = as.character(casa))
    camara_autores <-
      agoradigital::read_current_autores_camara(paste0(input_path, "/camara/autores.csv")) %>%
      dplyr::mutate(id_documento = as.numeric(id_documento),
                    nome = agoradigital::formata_nome_deputados(nome))
    senado_docs <-
      agoradigital::read_current_docs_senado(paste0(input_path, "/senado/documentos.csv")) %>%
      dplyr::mutate(id_documento = as.numeric(id_documento),
                    id_principal = as.numeric(id_principal),
                    casa = as.character(casa))
    senado_autores <- 
      agoradigital::read_current_autores_senado(paste0(input_path, "/senado/autores.csv")) %>% 
      dplyr::mutate(nome_autor =
                      agoradigital::formata_nome_senadores(nome_autor))

    props_leggo_id <-
      agoradigital::read_props(paste0(input_path, "/proposicoes.csv")) %>%
      dplyr::select(id_leggo, id_principal = id_ext, casa)

    if (flag == 1) {
      print("Atualizando tudo!")
      export_atores(camara_docs, camara_autores, senado_docs, senado_autores, output_path, data_inicial, peso_minimo, props_leggo_id)
      export_nodes_edges(input_path, camara_docs, data_inicial, senado_docs, camara_autores, peso_minimo, senado_autores, props_leggo_id, output_path)
    } else if (flag == 2) {
      print("Atualizando os atores!")
      export_atores(camara_docs, camara_autores, senado_docs, senado_autores, output_path, data_inicial, peso_minimo)
    } else{
      print("Atualizando nodes e edges!")
      export_nodes_edges(input_path, camara_docs, data_inicial, senado_docs, camara_autores, peso_minimo, senado_autores, output_path)
    }
  }
}

process_leggo_data(flag)
