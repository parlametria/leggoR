#' @title Exporta dados de autorias e coautorias na Câmara
#' @description Processa e escreve os dados de autorias e coautorias na Câmara
#' @param camara_docs documentos da camara
#' @param data_inicial data a partir da qual se considerará os documentos
#' @param camara_autores autores da camara
#' @param peso_minimo limiar para peso das arestas
#' @param props_leggo_id proposições a serem filtradas
#' @param output_path caminho para onde os arquivos serão exportados
.generate_coautorias_camara <- function(camara_docs, data_inicial, camara_autores, peso_minimo, props_leggo_id, output_path) {
  coautorias_camara <- tibble::tibble()
  autorias_camara <- tibble::tibble()
  
  print("Gerando tabela de autorias e coautorias para a Câmara...")
  
  camara_documentos <-
    camara_docs %>%
    dplyr::mutate(data = lubridate::floor_date(data_apresentacao, unit='day')) %>%
    dplyr::filter(data > data_inicial) %>%
    dplyr::left_join(props_leggo_id, by = c("id_principal", "casa"))
  
  # Gerando dado de autorias de documentos
  if (nrow(camara_documentos) > 0) {
    coautorias_camara_list <- agoradigital::get_coautorias(camara_documentos, camara_autores, "camara", as.numeric(peso_minimo), .PARTIDOS_OPOSICAO)
    coautorias_camara <- coautorias_camara_list$coautorias
    autorias_camara <- coautorias_camara_list$autorias
  }
  
  if (nrow(coautorias_camara) > 0) {
    coautorias <-
      coautorias_camara %>%
      dplyr::mutate(partido.x = dplyr::if_else(is.na(partido.x), "", partido.x),
                    partido.y = dplyr::if_else(is.na(partido.y), "", partido.y))
    
    autorias <-
      autorias_camara %>% 
      dplyr::mutate(autores = purrr::map_chr(nome_eleitoral, ~ paste(.x, collapse = ", "))) %>% 
      dplyr::distinct()
    
  } else {
    coautorias <- tibble::tibble(~id_leggo, ~id_principal, ~casa, ~id_autor.x, ~id_autor.y, ~peso_arestas, ~num_coautorias, 
                                 ~nome.x, ~partido.x, ~uf.x, ~casa_autor.x, ~bancada.x, ~nome.y, ~partido.y, ~uf.y, ~casa_autor.y,
                                 ~bancada.y)
    autorias <- tibble::tribble(~id_principal, ~casa, ~id_documento, ~descricao_tipo_documento, ~id_autor, ~data, 
                                ~url_inteiro_teor, ~id_leggo, ~casa_autor, ~nome_eleitoral, ~autores)
  }
  
  readr::write_csv(coautorias, paste0(output_path, '/camara/coautorias.csv'))
  readr::write_csv(autorias, paste0(output_path, '/camara/autorias.csv'))
}

#' @title Exporta dados de autorias e coautorias no Senado
#' @description Processa e escreve os dados de autorias e coautorias no Senado
#' @param senados_docs documentos do senado
#' @param data_inicial data a partir da qual se considerará os documentos
#' @param senado_autores autores do senado
#' @param peso_minimo limiar para peso das arestas
#' @param props_leggo_id proposições a serem filtradas
#' @param output_path pasta para onde exportar os dados
.generate_coautorias_senado <- function(senado_docs, data_inicial, senado_autores, peso_minimo, props_leggo_id, output_path) {
  coautorias_senado <- tibble::tibble()
  autorias_senado <- tibble::tibble()
  
  print("Gerando tabela de autorias e coautorias no Senado...")
  
  senado_documentos <-
    senado_docs %>%
    dplyr::filter(data_texto > data_inicial) %>%
    dplyr::left_join(props_leggo_id, by = c("id_principal", "casa"))
  
  # Gerando dado de autorias de documentos
  if (nrow(senado_documentos) > 0) {
    coautorias_senado_list <- agoradigital::get_coautorias(senado_documentos, senado_autores, "senado", as.numeric(peso_minimo), .PARTIDOS_OPOSICAO)
    coautorias_senado <- coautorias_senado_list$coautorias
    autorias_senado <- coautorias_senado_list$autorias
  }
  
  if (nrow(coautorias_senado) > 0) {
    coautorias <-
      coautorias_senado %>%
      dplyr::mutate(partido.x = dplyr::if_else(is.na(partido.x), "", partido.x),
                    partido.y = dplyr::if_else(is.na(partido.y), "", partido.y))
    
    autorias <-
      autorias_senado %>% 
      dplyr::mutate(autores = purrr::map_chr(nome_eleitoral, ~ paste(.x, collapse = ", "))) %>% 
      dplyr::distinct()
    
  } else {
    coautorias <- tibble::tibble(~id_leggo, ~id_principal, ~casa, ~id_autor.x, ~id_autor.y, ~peso_arestas, ~num_coautorias, 
                                 ~nome.x, ~partido.x, ~uf.x, ~casa_autor.x, ~bancada.x, ~nome.y, ~partido.y, ~uf.y, 
                                 ~casa_autor.y, ~bancada.y)
    autorias <- tibble::tribble(~id_principal, ~casa, ~id_documento, ~descricao_tipo_documento, ~id_autor, ~data, 
                                ~url_inteiro_teor, ~id_leggo, ~casa_autor, ~nome_eleitoral, ~autores)
  }
  
  readr::write_csv(coautorias, paste0(output_path, '/senado/coautorias.csv'))
  readr::write_csv(autorias, paste0(output_path, '/senado/autorias.csv'))
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
  
  .generate_coautorias_camara(camara_docs, data_inicial, camara_autores, peso_minimo, props_leggo_id, output_path)
  .generate_coautorias_senado(senado_docs, data_inicial, senado_autores, peso_minimo, props_leggo_id, output_path)
  
  coautorias_camara <- readr::read_csv(paste0(output_path, '/camara/coautorias.csv'))
  autorias_camara <- readr::read_csv(paste0(output_path, '/camara/autorias.csv'))
  
  coautorias_senado <- readr::read_csv(paste0(output_path, '/senado/coautorias.csv'))
  autorias_senado <- readr::read_csv(paste0(output_path, '/senado/autorias.csv'))
  
  if ((nrow(coautorias_camara) > 0) | (nrow(coautorias_senado) > 0)) {
    
    names(autorias_senado)
    names(autorias_camara)
    coautorias <-
      dplyr::bind_rows(coautorias_camara, coautorias_senado) %>%
      dplyr::mutate(partido.x = dplyr::if_else(is.na(partido.x), "", partido.x),
                    partido.y = dplyr::if_else(is.na(partido.y), "", partido.y))
    
    autorias <-
      dplyr::bind_rows(autorias_camara %>% 
                         mutate(data = as.Date(data, format = "%Y-%m-%d")), 
                       autorias_senado %>% 
                         mutate(data = as.Date(data, format = "%Y-%m-%d"))) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(id_autor_parlametria = paste0(dplyr::if_else(casa_autor == "camara", 1, 2),
                                                  id_autor))
    
    nodes <-
      agoradigital::get_unique_nodes(coautorias)
    
    edges <-
      coautorias %>%
      dplyr::group_by(id_leggo) %>%
      dplyr::group_modify(~ agoradigital::generate_edges(., graph_nodes = nodes, edges_weight = 1), keep = T) %>%
      dplyr::distinct()
    
    nodes <-
      agoradigital::compute_nodes_size(edges, nodes) %>% 
      dplyr::mutate(id_autor_parlametria = paste0(dplyr::if_else(casa_autor == "camara", 1, 2),
                                                  id_autor))
    
    edges <-
      edges %>%
      dplyr::filter(source != target)
  } else {
    nodes <- tibble::tribble(~id_leggo, ~id_autor, ~nome, ~partido, ~uf, ~bancada, ~casa_autor, ~nome_eleitoral, 
                             ~node_size, ~id_autor_parlametria)
    edges <- tibble::tribble(~id_leggo, ~source, ~target, ~value)
    autorias <- tibble::tribble(~id_principal, ~casa, ~id_documento, ~descricao_tipo_documento, ~id_autor,
                                ~data, ~url_inteiro_teor, ~id_leggo, ~casa_autor, ~nome_eleitoral, ~autores, 
                                ~id_autor_parlametria)
  }
  
  readr::write_csv(nodes , paste0(output_path, '/coautorias_nodes.csv'), na = "")
  readr::write_csv(edges, paste0(output_path, '/coautorias_edges.csv'), na = "")
  readr::write_csv(autorias, paste0(output_path, '/autorias.csv'))
}

