generate_nodes_and_edges <- function(coautorias) {
  library(tidygraph)
  
  graph_nodes <- dplyr::bind_rows(
    coautorias %>% dplyr::select(id_autor = id_autor.x, nome = nome.x, partido = partido.x, uf = uf.x),
    coautorias %>% dplyr::select(id_autor = id_autor.y, nome = nome.y, partido = partido.y, uf = uf.y)) %>% 
    distinct() %>% 
    dplyr::select(index = id_autor, dplyr::everything())
    
  graph_edges <- coautorias %>%
    dplyr::select(
      source = id_autor.x,
      target = id_autor.y,
      value = peso_arestas) %>% 
    dplyr::mutate(source = as.factor(source),
                  target = as.factor(target))
  
  graph <- tbl_graph(nodes = graph_nodes,
                     edges = graph_edges,
                     directed = F)
  
  final_nodes <- graph %>%
    mutate(group = as.factor(group_edge_betweenness())) %>%
    as.data.frame() %>%
    group_by(group) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    rename(old_index = index) %>%
    tibble::rowid_to_column("index") %>% 
    mutate(index = index -1) %>% 
    mutate(nome_eleitoral = paste0(nome, " (", partido, "/", uf, ")")) %>% 
    as.data.frame()
  
  final_edges <- graph_edges %>% 
    mutate(source = as.numeric(as.character(source)),
           target = as.numeric(as.character(target))) %>% 
    inner_join(final_nodes %>% select(old_index,index), by = c("source" = "old_index")) %>% 
    inner_join(final_nodes %>% select(old_index,index), by = c("target" = "old_index")) %>% 
    select(source = index.x, target = index.y, value) %>%
    mutate(source = as.factor(source), target = as.factor(target)) %>% 
    arrange(target) %>% 
    as.data.frame()
  
  return(list(final_nodes,final_edges))
}

# generate_edges <- function(coautorias, nodes) {
#   df <- 
#     coautorias %>% 
#     mutate(id_autor.x=as.character(id_autor.x),
#            id_autor.y=as.character(id_autor.y))
#   
#   graph_edges <- coautorias %>%
#     dplyr::select(
#       source = id_autor.x,
#       target = id_autor.y,
#       value = peso_arestas) %>% 
#     dplyr::mutate(source = as.factor(source),
#                   target = as.factor(target)) %>% 
#     inner_join(nodes, by = c("source" = "index")) %>% 
#     inner_join(nodes, by = c("target" = "index")) %>% 
#     select(source = index.x, target = index.y, value) %>%
#     mutate(source = as.factor(source), target = as.factor(target)) %>% 
#     arrange(target) %>% 
#     as.data.frame()
# }

# generate_nodes_and_edges <- function(coautorias) {
#   
#   coautorias <- coautorias %>% 
#     distinct()
#   
#   nodes <- generate_nodes(coautorias)
#   
#   edges <-
#     generate_edges(coautorias %>% select(id_autor.x, id_autor.y, peso_arestas), nodes)
#   
#   return(list(nodes, edges))
# }

#' @title Concateca dois elementos com um separador no meio
#' @description Recebe duas variáveis x e y e retorna a união "x:y".
#' @param x Primeira variável a ser concatenada
#' @param y Segunda variável a ser concatenada
#' @param sep Separador a ser concatenado
#' @return String concatenada com a primeira variável + separador + segunda variável
paste_cols <- function(x, y, sep = ":") {
  stopifnot(length(x) == length(y))
  return(lapply(1:length(x), function(i) {
    paste0(sort(c(x[i], y[i])), collapse = sep)
  }) %>%
    unlist())
}

remove_duplicated_edges <- function(df) {
  df %>%
    mutate(col_pairs =
             paste_cols(id_autor.x,
                        id_autor.y,
                        sep = ":")) %>%
    group_by(col_pairs) %>%
    tidyr::separate(col = col_pairs,
                    c("id_autor.x",
                      "id_autor.y"),
                    sep = ":") %>%
    group_by(id_autor.x, id_autor.y) %>%
    distinct()
}


get_coautorias <- function(peso_autorias, autorias, parlamentares) {

  peso_autorias <- 
    peso_autorias %>% 
    ungroup() %>% 
    filter(peso_arestas < 1)
  
  coautorias <- 
    autorias %>%
    full_join(autorias, by = c("id_principal", "casa", "id_documento")) %>% 
    filter(id_autor.x != id_autor.y)
  
  coautorias <- coautorias %>%
    remove_duplicated_edges() %>%
    inner_join(peso_autorias, by = c("id_principal", "id_documento")) %>% 
    group_by(id_principal, casa, id_autor.x, id_autor.y, data.x) %>% 
    summarise(peso_arestas = sum(peso_arestas),
           num_coautorias = n()) %>%
    ungroup() %>% 
    mutate(id_autor.x = as.numeric(id_autor.x),
           id_autor.y = as.numeric(id_autor.y))
  
  coautorias <- coautorias %>% 
    inner_join(parlamentares, by = c("id_autor.x" = "id_autor")) %>% 
    inner_join(parlamentares, by = c("id_autor.y" = "id_autor")) %>% 
    distinct()
  
  return(coautorias)
}

prepare_autorias_df_camara <- function(docs_camara, autores_camara) {
  autores_docs <- merge(docs_camara, autores_camara, by = c("id_documento", "casa")) %>%
        dplyr::select(id_principal,
                      casa,
                      id_documento,
                      id_autor,
                      data) %>% 
    dplyr::distinct()
}

prepare_autorias_df_senado <- function(docs_senado, autores_senado) {
  autores_docs <- merge(docs_senado, autores_senado %>% dplyr::filter(!is.na(id_autor)), 
                        by = c("id_principal", "id_documento", "casa")) %>% 
    dplyr::select(id_principal,
                  casa,
                  id_documento,
                  id_autor,
                  data = data_texto) %>% 
    dplyr::distinct()
}

compute_peso_autoria_doc <- function(autorias) {
  peso_autorias <- autorias %>% 
    group_by(id_principal, id_documento) %>% 
    summarise(peso_arestas = 1/n())
}