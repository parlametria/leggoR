#' @title Calcula o tamanho dos nós
#' @description Recebe um dataframe com as arestas e os nós, e calcula o tamanho do nó
#' a partir do somatório dos pesos das arestas
#' @param final_edges Dataframe com as arestas
#' @param final_nodes Dataframe com os nós
#' @param smoothing Variável para suavizar o tamanho dos nós
#' @return Dataframe dos nós com a coluna node_size
set_nodes_size <- function(final_edges, final_nodes, smoothing) {
  nodes_size_source <-
    final_edges %>%
    dplyr::group_by(node = source) %>%
    dplyr::summarise(node_size = sum(value)) %>%
    dplyr::mutate(node = as.character(node))
  nodes_size_target <-
    final_edges %>%
    dplyr::group_by(node = target) %>%
    dplyr::summarise(node_size = sum(value)) %>%
    dplyr::mutate(node = as.character(node))
  nodes_size <-
    dplyr:: bind_rows(nodes_size_source,nodes_size_target) %>%
    dplyr::group_by(node) %>%
    dplyr::summarise(node_size = sum(node_size)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(node = as.numeric(node))
  final_nodes %>%
    dplyr::left_join(nodes_size, by=c("index"="node")) %>%
    dplyr::mutate(node_size = node_size / smoothing)
}

#' @title Gera os dataframe de nós e arestas
#' @description Recebe um dataframe com coautorias e gera os dataframes com os nós e
#' com as arestas
#' @param coautorias Dataframe com as coautorias
#' @param edges_weight Variável para multiplicar com os pesos das arestas
#' @param smoothing Variável para suavizar o tamanho dos nós
#' @return Lista com os dataframes de arestas e dos nós
#' @export
generate_nodes_and_edges <- function(coautorias, edges_weight = 100, smoothing = 1) {
  graph_nodes <- dplyr::bind_rows(
    coautorias %>% dplyr::select(id_leggo, id_autor = id_autor.x, nome = nome.x, partido = partido.x, uf = uf.x, bancada = bancada.x),
    coautorias %>% dplyr::select(id_leggo, id_autor = id_autor.y, nome = nome.y, partido = partido.y, uf = uf.y, bancada = bancada.y)) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column("index") %>%
    dplyr::mutate(index = index -1)

  coautorias_index <- coautorias %>%
    dplyr::left_join(graph_nodes %>%  dplyr::select(id_leggo, index.x = index, id_autor), by=c("id_autor.x"="id_autor")) %>%
    dplyr::left_join(graph_nodes %>%  dplyr::select(id_leggo, index.y = index, id_autor), by=c("id_autor.y"="id_autor"))

  graph_edges <- coautorias_index %>%
    dplyr::select(
      source = index.x,
      target = index.y,
      id_leggo, 
      value = peso_arestas) %>%
    dplyr::mutate(source = as.factor(source),
                  target = as.factor(target))

  final_nodes <- graph_nodes %>%
    as.data.frame() %>%
    dplyr::mutate(nome_eleitoral = paste0(nome, " (", partido, "/", uf, ")"))

  final_edges <- graph_edges %>%
    as.data.frame() %>%
    dplyr::mutate(value = value*edges_weight)

  final_nodes <-
    set_nodes_size(final_edges, final_nodes, smoothing)

  return(list(final_nodes,final_edges))
}

#' @title Concateca dois elementos com um separador no meio
#' @description Recebe duas variáveis x e y e retorna a união "x:y".
#' @param x Primeira variável a ser concatenada
#' @param y Segunda variável a ser concatenada
#' @param sep Separador a ser concatenado
#' @return String concatenada com a primeira variável + separador + segunda variável
paste_cols_sorted <- function(x, y, sep = ":") {
  stopifnot(length(x) == length(y))
  return(lapply(1:length(x), function(i) {
    paste0(sort(c(x[i], y[i])), collapse = sep)
  }) %>%
    unlist())
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
    dplyr::distinct(id_principal, casa, id_documento, data = data.x, col_pairs, id_leggo) %>%
    tidyr::separate(col = col_pairs,
                    c("id_autor.x",
                      "id_autor.y"),
                    sep = ":")
}

#' @title Cria o dataframe de coautorias
#' @description  Recebe o dataframe de pesos, autorias e parlamentares e
#' cria o dataframe de coautorias
#' @param peso_autorias Dataframe com as pesos das autorias
#' @param autorias Dataframe com autorias
#' @param parlamentares Dataframe com todos os dados dos parlamentars
#' @return Dataframe
#' @export
get_coautorias <- function(docs, autores, casa) {
  
  if (casa == 'camara') {
    autorias <- agoradigital::prepare_autorias_df_camara(docs, autores)
    peso_autorias <- agoradigital::compute_peso_autoria_doc(autorias)
    parlamentares <- autores %>% dplyr::select(id_autor, nome, partido, uf)
  } else {
    autorias <- agoradigital::prepare_autorias_df_senado(docs, autores)
    peso_autorias <- agoradigital::compute_peso_autoria_doc(autorias)
    parlamentares <- autores %>% dplyr::select(id_autor, nome = nome_autor, partido, uf)
  }
  
  parlamentares <-
    parlamentares %>% 
    dplyr::mutate(bancada = dplyr::if_else(partido %in% .PARTIDOS_OPOSICAO, "oposição", "governo"))

  peso_autorias <-
    peso_autorias %>%
    dplyr::ungroup() %>%
    dplyr::filter(peso_arestas < 1)

  coautorias <-
    autorias %>%
    dplyr::full_join(autorias, by = c("id_principal", "casa", "id_documento")) %>%
    dplyr::filter(id_autor.x != id_autor.y) %>% 
    dplyr::rename(id_leggo = id_leggo.x) %>% 
    dplyr::select(-id_leggo.y)

  coautorias <- coautorias %>%
    remove_duplicated_edges() %>%
    dplyr::inner_join(peso_autorias, by = c("id_principal", "id_documento")) %>%
    dplyr::group_by(id_leggo, id_principal, casa, id_autor.x, id_autor.y, data) %>%
    dplyr::summarise(peso_arestas = sum(peso_arestas),
           num_coautorias = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id_autor.x = as.numeric(id_autor.x),
           id_autor.y = as.numeric(id_autor.y))

  coautorias <- coautorias %>%
    dplyr::inner_join(parlamentares, by = c("id_autor.x" = "id_autor")) %>%
    dplyr::inner_join(parlamentares, by = c("id_autor.y" = "id_autor")) %>%
    dplyr::distinct() 

  return(coautorias)
}

#' @title Cria o dataframe de autorias da camara
#' @description  Faz um merge do df de documentos com autores
#' @param docs_camara Dataframe com documentos da camara
#' @param autores_camara Dataframe com autores da camara
#' @return Dataframe
#' @export
prepare_autorias_df_camara <- function(docs_camara, autores_camara) {
  autores_docs <- merge(docs_camara, autores_camara, by = c("id_documento", "casa")) %>%
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
#' @description  Faz um merge do df de documentos com autores
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

#' @title Cria o dataframe com os documentos e seus autores
#' @description Cria um documento que contém todos os autores e documentos da Camara e do Senado
#' @param autorias_camara Dataframe com as autorias da Camara
#' @param autorias_senado Dataframe com as autorias do Senado
#' @param  parlamentares_camara Dataframe com os deputados
#' @param parlamentares_senado Dataframe com os senadores
#' @return Dataframe
get_autorias_geral <- function(autorias_camara, autorias_senado, parlamentares_camara, parlamentares_senado) {

  get_autorias_completas <- function(autorias, parlamentares) {
    autorias %>%
      dplyr::left_join(parlamentares, by = "id_autor") %>%
      dplyr::distinct() %>%
      dplyr::mutate(nome_com_partido = paste0(nome, " (", partido, "/", uf, ")")) %>%
      dplyr::select(-c(nome, partido, uf))
  }

  autorias_camara_completo <- get_autorias_completas(autorias_camara, parlamentares_camara)

  autorias_senado_completo <-
    get_autorias_completas(autorias_senado, parlamentares_senado) %>%
    dplyr::mutate(id_principal = as.numeric(id_principal),
                  id_documento = as.numeric(id_documento))

  autorias_geral <-
    dplyr:: bind_rows(autorias_camara_completo, autorias_senado_completo)
}

#' @title Cria o dataframe com os pesos das autorias
#' @description  Calcula o peso de cada autoria, onde o peso é menor quanto
#' maior o número de autores
#' @param autorias Dataframe com as autorias
#' @return Dataframe
#' @export
compute_peso_autoria_doc <- function(autorias) {
  peso_autorias <- autorias %>%
    dplyr::group_by(id_principal, id_documento) %>%
    dplyr::summarise(peso_arestas = 1/dplyr::n())
}
