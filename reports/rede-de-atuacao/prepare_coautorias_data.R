#' @title Calcula o tamanho dos nós
#' @description Recebe um dataframe com as arestas e os nós, e calcula o tamanho do nó
#' a partir do somatório dos pesos das arestas
#' @param final_edges Dataframe com as arestas
#' @param final_nodes Dataframe com os nós
#' @param smoothing Variável para suavizar o tamanho dos nós
#' @return Dataframe dos nós com a coluna node_size
set_nodes_size <- function(final_edges, final_nodes, smoothing) {
  nodes_size_src <-
    final_edges %>%
    group_by(node = source) %>%
    summarise(node_size = sum(value)) %>%
    mutate(node = as.character(node))
  nodes_size_tgt <-
    final_edges %>%
    group_by(node = target) %>%
    summarise(node_size = sum(value)) %>%
    mutate(node = as.character(node))
  nodes_size <-
    bind_rows(nodes_size_src,nodes_size_tgt) %>%
    group_by(node) %>%
    summarise(node_size = sum(node_size)) %>%
    ungroup() %>%
    mutate(node = as.numeric(node))
  final_nodes %>%
    left_join(nodes_size, by=c("index"="node")) %>%
    mutate(node_size = node_size / smoothing)
}

#' @title Gera os dataframe de nós e arestas
#' @description Recebe um dataframe com coautorias e gera os dataframes com os nós e
#' com as arestas
#' @param coautorias Dataframe com as coautorias
#' @param edges_weight Variável para multiplicar com os pesos das arestas
#' @param smoothing Variável para suavizar o tamanho dos nós
#' @return Lista com os dataframes de arestas e dos nós
generate_nodes_and_edges <- function(coautorias, edges_weight = 100, smoothing = 1) {
  graph_nodes <- dplyr::bind_rows(
    coautorias %>% dplyr::select(id_autor = id_autor.x, nome = nome.x, partido = partido.x, uf = uf.x),
    coautorias %>% dplyr::select(id_autor = id_autor.y, nome = nome.y, partido = partido.y, uf = uf.y)) %>%
    distinct() %>%
    tibble::rowid_to_column("index") %>%
    mutate(index = index -1)

  coautorias_index <- coautorias %>%
    left_join(graph_nodes %>% select(index.x = index, id_autor), by=c("id_autor.x"="id_autor")) %>%
    left_join(graph_nodes %>% select(index.y = index, id_autor), by=c("id_autor.y"="id_autor"))

  graph_edges <- coautorias_index %>%
    dplyr::select(
      source = index.x,
      target = index.y,
      value = peso_arestas) %>%
    dplyr::mutate(source = as.factor(source),
                  target = as.factor(target))

  final_nodes <- graph_nodes %>%
    as.data.frame() %>%
    mutate(nome_eleitoral = paste0(nome, " (", partido, "/", uf, ")")) %>%
    as.data.frame()

  final_edges <- graph_edges %>%
    as.data.frame() %>%
    mutate(value = value*edges_weight)

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
    mutate(col_pairs =
             paste_cols_sorted(id_autor.x,
                               id_autor.y,
                               sep = ":")) %>%
    distinct(id_principal, casa, id_documento, data = data.x, col_pairs) %>%
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
    group_by(id_principal, casa, id_autor.x, id_autor.y, data) %>%
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

#' @title Cria o dataframe de autorias da camara
#' @description  Faz um merge do df de documentos com autores
#' @param docs_camara Dataframe com documentos da camara
#' @param autores_camara Dataframe com autores da camara
#' @return Dataframe
prepare_autorias_df_camara <- function(docs_camara, autores_camara) {
  autores_docs <- merge(docs_camara, autores_camara, by = c("id_documento", "casa")) %>%
        dplyr::select(id_principal,
                      casa,
                      id_documento,
                      descricao_tipo_documento,
                      id_autor,
                      data,
                      url_inteiro_teor) %>%
    dplyr::distinct()
}

#' @title Cria o dataframe de autorias da senado
#' @description  Faz um merge do df de documentos com autores
#' @param docs_senado Dataframe com documentos da senado
#' @param autores_senado Dataframe com autores da senado
#' @return Dataframe
prepare_autorias_df_senado <- function(docs_senado, autores_senado) {
  autores_docs <- merge(docs_senado, autores_senado %>% dplyr::filter(!is.na(id_autor)),
                        by = c("id_principal", "id_documento", "casa")) %>%
    dplyr::select(id_principal,
                  casa,
                  id_documento,
                  descricao_tipo_documento = descricao_texto,
                  id_autor,
                  url_inteiro_teor = url_texto,
                  data = data_texto) %>%
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
      left_join(parlamentares, by = "id_autor") %>%
      distinct() %>%
      mutate(nome_com_partido = paste0(nome, " (", partido, "/", uf, ")")) %>%
      select(-c(nome, partido, uf))
  }

  autorias_camara_completo <- get_autorias_completas(autorias_camara, parlamentares_camara)

  autorias_senado_completo <-
    get_autorias_completas(autorias_senado, parlamentares_senado) %>%
    dplyr::mutate(id_principal = as.numeric(id_principal),
                  id_documento = as.numeric(id_documento))

  autorias_geral <-
    bind_rows(autorias_camara_completo, autorias_senado_completo)
}

#' @title Cria o dataframe com os pesos das autorias
#' @description  Calcula o peso de cada autoria, onde o peso é menor quanto
#' maior o número de autores
#' @param autorias Dataframe com as autorias
#' @return Dataframe
compute_peso_autoria_doc <- function(autorias) {
  peso_autorias <- autorias %>%
    group_by(id_principal, id_documento) %>%
    summarise(peso_arestas = 1/n())
}
