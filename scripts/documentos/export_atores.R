#' @title Exporta dados atores
#' @description Processa e escreve os dados de atores
#' @param camara_docs documentos da camara
#' @param senados_docs documentos do senado
#' @param camara_autores autores da camara
#' @param senado_autores autores do senado
#' @param data_inicio data a partir da qual se considerará os documentos
#' @param peso_minimo limiar para peso dos documentos
#' @param output_path pasta para onde exportar os dados
export_atores <- function(camara_docs, camara_autores, senado_docs, senado_autores, output_path, data_inicio, peso_minimo, props_leggo_id) {
  print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))
  
  atores_camara <- agoradigital::create_tabela_atores_camara(camara_docs, camara_autores, data_inicio = data_inicio, limiar = peso_minimo) %>% 
    select(id_ext, casa, id_autor, tipo_autor, tipo_generico, sigla_local, peso_total_documentos, num_documentos, partido, uf, nome_autor, is_important)
  
  atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores, data_inicio = data_inicio, limiar = peso_minimo) %>% 
    select(id_ext, casa, id_autor, tipo_autor, tipo_generico, sigla_local, peso_total_documentos, num_documentos, partido, uf, nome_autor, is_important)
  
  .PARTIDOS_OPOSICAO <-
    c("PT", "PSOL", "PSB", "PCdoB", "PDT", "REDE")
  
  if ((nrow(atores_camara) > 0) | (nrow(atores_senado) > 0)) {
    atores_df <-
      dplyr::bind_rows(atores_camara, atores_senado) %>%
      dplyr::mutate(bancada = dplyr::if_else(partido %in% .PARTIDOS_OPOSICAO, "oposição", "governo")) %>%
      dplyr::left_join(props_leggo_id, by = c("id_ext"="id_principal", "casa")) %>%
      dplyr::mutate(casa_autor = dplyr::if_else(tolower(tipo_autor) == "deputado", 
                                                "camara",
                                                "senado")) %>% 
      dplyr::mutate(id_autor_parlametria = paste0(dplyr::if_else(casa_autor == "camara", 1, 2),
                                           id_autor)) %>% 
      dplyr::select(id_leggo, id_autor_parlametria, dplyr::everything())
  } else {
    atores_df <- tibble::tribble(~id_leggo, ~id_autor_parlametria, ~id_ext, ~casa, ~id_autor, ~tipo_autor, ~tipo_generico, ~sigla_local,
                                 ~peso_total_documentos, ~num_documentos, ~partido, ~uf, ~nome_autor, ~is_important, ~bancada, ~casa_autor)
  }
  
  readr::write_csv(atores_df, paste0(output_path, '/atores.csv'), na = "")
}