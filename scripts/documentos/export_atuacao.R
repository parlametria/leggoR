#' @title Exporta dados atuação
#' @description Processa e escreve os dados de atuação
#' @param camara_docs documentos da camara
#' @param senados_docs documentos do senado
#' @param camara_autores autores da camara
#' @param senado_autores autores do senado
#' @param data_inicio data a partir da qual se considerará os documentos
#' @param peso_minimo limiar para peso dos documentos
#' @param output_path pasta para onde exportar os dados
export_atuacao <- function(camara_docs, camara_autores, senado_docs, senado_autores, output_path, data_inicio, peso_minimo, props_leggo_id) {
  print(paste("Gerando tabela de atuação a partir de dados atualizados de documentos e autores..."))

  atuacao_camara <- agoradigital::create_tabela_atuacao_camara(camara_docs, camara_autores, data_inicio = data_inicio, limiar = peso_minimo) %>%
    select(id_ext, casa, id_autor, tipo_autor, tipo_generico, sigla_local, peso_total_documentos, num_documentos, partido, uf, nome_autor, is_important) %>%
    agoradigital::remove_atuacao_camara_comissao_mista()

  atuacao_senado <- agoradigital::create_tabela_atuacao_senado(senado_docs, senado_autores, data_inicio = data_inicio, limiar = peso_minimo) %>%
    select(id_ext, casa, id_autor, tipo_autor, tipo_generico, sigla_local, peso_total_documentos, num_documentos, partido, uf, nome_autor, is_important)

  .PARTIDOS_OPOSICAO <-
    c("PT", "PSOL", "PSB", "PCdoB", "PDT", "REDE")

  if ((nrow(atuacao_camara) > 0) | (nrow(atuacao_senado) > 0)) {
    atuacao_df <-
      dplyr::bind_rows(atuacao_camara, atuacao_senado) %>%
      dplyr::mutate(bancada = dplyr::if_else(partido %in% .PARTIDOS_OPOSICAO, "oposição", "governo")) %>%
      dplyr::left_join(props_leggo_id, by = c("id_ext"="id_principal", "casa")) %>%
      dplyr::mutate(casa_autor = dplyr::if_else(tolower(tipo_autor) == "deputado",
                                                "camara",
                                                "senado")) %>%
      dplyr::mutate(id_autor_parlametria = paste0(dplyr::if_else(casa_autor == "camara", 1, 2),
                                           id_autor)) %>%
      dplyr::select(id_leggo, id_autor_parlametria, id_ext, casa, id_autor, tipo_autor, casa_autor, tipo_generico, sigla_local,
                    peso_total_documentos, num_documentos, is_important, bancada)
  } else {
    atuacao_df <- tibble::tribble(~id_leggo, ~id_autor_parlametria, ~id_ext, ~casa, ~id_autor, ~tipo_autor, ~casa_autor, ~tipo_generico,
                                 ~sigla_local, ~peso_total_documentos, ~num_documentos, ~is_important, ~bancada)
  }

  readr::write_csv(atuacao_df, paste0(output_path, '/atuacao.csv'), na = "")
}
