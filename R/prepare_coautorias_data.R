#' @title Calcula o tamanho dos nós
#' @description Recebe um dataframe com as arestas e os nós, e calcula o tamanho do nó
#' a partir do somatório dos pesos das arestas
#' @param final_edges Dataframe com as arestas
#' @param final_nodes Dataframe com os nós
#' @param smoothing Variável para suavizar o tamanho dos nós
#' @return Dataframe dos nós com a coluna node_size
#' @export
compute_nodes_size <- function(final_edges, final_nodes, smoothing = 1) {
  nodes_size_source <-
    final_edges %>%
    dplyr::group_by(node = source, id_leggo) %>%
    dplyr::summarise(node_size = sum(value)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(node = as.character(node))
  nodes_size_target <-
    final_edges %>%
    dplyr::group_by(node = target, id_leggo) %>%
    dplyr::summarise(node_size = sum(value)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(node = as.character(node))
  nodes_size <-
    dplyr:: bind_rows(nodes_size_source,nodes_size_target) %>%
    dplyr::group_by(node, id_leggo) %>%
    dplyr::summarise(node_size = sum(node_size)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(node = as.numeric(node))
  final_nodes %>%
    dplyr::left_join(nodes_size, by=c("id_autor"="node", "id_leggo")) %>%
    dplyr::mutate(node_size = node_size / smoothing)
}

#' @title Gera os dataframe de nós
#' @description Recebe um dataframe com coautorias e gera os dataframes com os nós
#' @param coautorias Dataframe com as coautorias
#' @return Dataframes de nós
#' @export
generate_nodes <- function(coautorias) {
    graph_nodes <-
      dplyr::bind_rows(
      coautorias %>% dplyr::select(id_autor = id_autor.x, nome = nome.x, partido = partido.x, uf = uf.x, bancada = bancada.x),
      coautorias %>% dplyr::select(id_autor = id_autor.y, nome = nome.y, partido = partido.y, uf = uf.y, bancada = bancada.y)) %>%
      dplyr::distinct()

  final_nodes <- graph_nodes %>%
    tibble::as_tibble() %>%
    dplyr::mutate(nome_eleitoral = agoradigital::formata_nome_eleitoral(nome, partido, uf))
  
  return(final_nodes)
}

#' @title Gera os dataframe de nós únicos
#' @description Remove os nós duplicados (eles estavam duplicados pois os nomes vinham
#' diferente)
#' @param coautorias Dataframe com as coautorias
#' @return Dataframes de nós
#' @export
get_unique_nodes <- function(coautorias) {
  nodes <-
    coautorias %>% 
    dplyr::group_by(id_leggo) %>% 
    dplyr::group_modify(~ agoradigital::generate_nodes(.), keep = T) %>% 
    dplyr::ungroup()
  
  unique_nodes <-
    nodes %>% 
    dplyr::group_by(id_leggo, id_autor) %>% 
    dplyr::summarise(nome = dplyr::first(nome),
                     partido = dplyr::first(partido),
                     uf = dplyr::first(uf),
                     bancada = dplyr::first(bancada),
                     nome_eleitoral = dplyr::first(nome_eleitoral))
  
  nodes %>% 
    dplyr::inner_join(unique_nodes, by = c("id_leggo", "id_autor", "nome", "partido", "uf", "bancada", "nome_eleitoral"))
}

#' @title Gera os dataframe de arestas
#' @description Recebe um dataframe com coautorias e gera os dataframes com as arestas
#' @param coautorias Dataframe com as coautorias
#' @param graph_nodes Dataframe com os nós
#' @param edges_weight Variável para multiplicar com os pesos das arestas
#' @return Dataframes de arestas
#' @export
generate_edges <- function(coautorias, graph_nodes, edges_weight = 1) {
  coautorias_index <- 
    coautorias %>%
    dplyr::left_join(graph_nodes %>%  dplyr::select(id_autor), by=c("id_autor.x"="id_autor")) %>%
    dplyr::left_join(graph_nodes %>%  dplyr::select(id_autor), by=c("id_autor.y"="id_autor"))
  
  graph_edges <- coautorias_index %>%
    dplyr::select(
      source = id_autor.x,
      target = id_autor.y,
      value = peso_arestas) 
  
  final_edges <- graph_edges %>%
    tibble::as_tibble() %>%
    dplyr::mutate(value = value*edges_weight) %>% 
    dplyr::distinct()
}

#' @title Remove arestas duplicadas
#' @description Recebe um dataframe com autorias e remove as arestas
#' duplicadas
#' @param df Dataframe com as arestas duplicadas
#' @return Dataframe sem as duplicadas
remove_duplicated_edges <- function(df) {
  df %>%
    dplyr::mutate(col_pairs =
             paste_cols_sorted(id_autor.x,
                               id_autor.y,
                               sep = ":")) %>%
    dplyr::distinct(id_leggo, id_principal, casa, id_documento, data, col_pairs) %>%
    tidyr::separate(col = col_pairs,
                    c("id_autor.x",
                      "id_autor.y"),
                    sep = ":")
}

#' @title Cria o dataframe de coautorias sem os dados de parlamentares
#' @description  Recebe o dataframe de autorias, pesos e o limiar e retorna o dataframe 
#' de coautorias raw
#' @param autorias Dataframe com as autorias
#' @param peso_autorias Dataframe com o peso das autorias
#' @param limiar Peso mínimo das arestas
#' @return Dataframe
get_coautorias_raw <- function(autorias, peso_autorias, limiar) {
  num_autorias_por_pl <- autorias %>% 
    dplyr::group_by(id_leggo, id_documento, casa) %>% 
    dplyr::summarise(num_autores = dplyr::n()) %>% 
    dplyr::ungroup()
  
  peso_autorias <-
    peso_autorias %>%
    dplyr::ungroup() %>%
    dplyr::filter(peso_arestas >= limiar)
  
  coautorias_raw <- autorias %>%
    dplyr::full_join(autorias, by = c("id_leggo", "id_documento")) %>% 
    dplyr::select(id_leggo, id_principal = id_principal.x, casa = casa.x, id_documento, data = data.x, 
                  id_autor.x, id_autor.y)
  
  coautorias_simples <- coautorias_raw %>% 
    dplyr::inner_join(num_autorias_por_pl %>% dplyr::filter(num_autores == 1),
                      by=c("id_documento", "id_leggo", "casa"))
  
  coautorias_multiplas <- coautorias_raw %>% 
    dplyr::inner_join(num_autorias_por_pl %>% dplyr::filter(num_autores > 1),
                      by=c("id_documento", "id_leggo", "casa")) %>% 
    dplyr::filter(id_autor.x != id_autor.y)
  
  coautorias <- dplyr::bind_rows(coautorias_simples, coautorias_multiplas) %>% 
    dplyr::select(-num_autores)
  
  coautorias %>%
    remove_duplicated_edges() %>%
    dplyr::inner_join(peso_autorias, by = c("id_principal", "id_documento")) %>%
    dplyr::group_by(id_leggo, id_principal, casa, id_autor.x, id_autor.y) %>%
    dplyr::summarise(peso_arestas = sum(peso_arestas),
                     num_coautorias = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id_autor.x = as.numeric(id_autor.x),
                  id_autor.y = as.numeric(id_autor.y))
}

#' @title Cria o dataframe de coautorias
#' @description  Recebe o dataframe de documentos, autores a casa e o limiar
#' e retorna o dataframe de coautorias
#' @param docs Dataframe com os documentos
#' @param autores Dataframe com autores dos documentos
#' @param casa camara ou senado
#' @param limiar Peso mínimo das arestas
#' @return Dataframe
#' @export
get_coautorias <- function(docs, autores, casa, limiar = 0.1) {
  
  if (casa == 'camara') {
    autorias <- agoradigital::prepare_autorias_df_camara(docs, autores)
    peso_autorias <- agoradigital::compute_peso_autoria_doc(autorias)
    parlamentares <- autores %>% dplyr::select(id_autor, nome, partido, uf)
  } else {
    autorias <- agoradigital::prepare_autorias_df_senado(docs, autores)
    peso_autorias <- agoradigital::compute_peso_autoria_doc(autorias)
    parlamentares <- autores %>% dplyr::select(id_autor, nome = nome_autor, partido, uf)
  }
  
  coautorias <- get_coautorias_raw(autorias, peso_autorias, limiar)
  
  autorias <-
    autorias %>% 
    dplyr::left_join(parlamentares, by = "id_autor") %>% 
    dplyr::distinct() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(nome_eleitoral = formata_nome_eleitoral(nome, partido, uf)) %>% 
    dplyr::select(-c(nome, partido, uf, id_principal, casa))
  
  parlamentares <-
    parlamentares %>% 
    dplyr::mutate(bancada = dplyr::if_else(partido %in% .PARTIDOS_OPOSICAO, "oposição", "governo"))

  coautorias <- 
    coautorias %>%
    dplyr::inner_join(parlamentares, by = c("id_autor.x" = "id_autor")) %>%
    dplyr::inner_join(parlamentares, by = c("id_autor.y" = "id_autor")) %>%
    dplyr::distinct() 

  return(list(coautorias = coautorias, autorias = autorias))
}

#' @title Cria o dataframe de autorias da camara
#' @description Faz um merge do df de documentos com autores
#' @param docs_camara Dataframe com documentos da camara
#' @param autores_camara Dataframe com autores da camara
#' @return Dataframe
#' @export
prepare_autorias_df_camara <- function(docs_camara, autores_camara) {
  autores_docs <-
    merge(docs_camara, autores_camara, by = c("id_documento", "casa")) %>%
        dplyr::select(id_principal,
                      casa,
                      id_documento,
                      descricao_tipo_documento,
                      id_autor,
                      data,
                      url_inteiro_teor,
                      id_leggo) %>%
    dplyr::distinct()
}

#' @title Cria o dataframe de autorias da senado
#' @description Faz um merge do df de documentos com autores
#' @param docs_senado Dataframe com documentos da senado
#' @param autores_senado Dataframe com autores da senado
#' @return Dataframe
#' @export
prepare_autorias_df_senado <- function(docs_senado, autores_senado) {
  autores_docs <- merge(docs_senado, autores_senado %>% dplyr::filter(!is.na(id_autor)),
                        by = c("id_principal", "id_documento", "casa")) %>%
    dplyr::select(id_principal,
                  casa,
                  id_documento,
                  descricao_tipo_documento = descricao_texto,
                  id_autor,
                  url_inteiro_teor = url_texto,
                  data = data_texto,
                  id_leggo) %>%
    dplyr::distinct()
}

#' @title Cria o dataframe com os pesos das autorias
#' @description Calcula o peso de cada autoria e o peso é menor quanto
#' maior o número de autores
#' @param autorias Dataframe com as autorias
#' @return Dataframe
#' @export
compute_peso_autoria_doc <- function(autorias) {
  peso_autorias <- autorias %>%
    dplyr::group_by(id_principal, id_documento) %>%
    dplyr::summarise(peso_arestas = 1/dplyr::n())
}
