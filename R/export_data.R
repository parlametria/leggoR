process_etapa <- function(id, casa) {
    prop <- agoradigital::fetch_proposicao(id, casa)
    tram <- agoradigital::fetch_tramitacao(id, casa, TRUE)
    proc_tram <-
        agoradigital::process_proposicao(prop, tram, casa) %>%
        dplyr::mutate(data_hora = as.POSIXct(data_hora))
    status <- agoradigital::extract_status_tramitacao(tram)
    historico_energia <-
        agoradigital::get_historico_energia_recente(proc_tram) %>%
        dplyr::mutate(id_ext = prop$prop_id, casa = prop$casa) %>%
        dplyr::select(id_ext, casa, periodo, energia_periodo, energia_recente)
    energia_value <-
        historico_energia %>%
        dplyr::slice(n()) %>%
        .$energia_recente
    extended_prop <-
        merge(prop, status, by = "prop_id") %>%
        dplyr::mutate(energia = energia_value)

    list(
        proposicao = extended_prop,
        fases_eventos = proc_tram,
        hist_energia = historico_energia)
}

process_pl <- function(id_camara, id_senado, apelido, tema) {
    print(id_camara)
    print(apelido)
    cat(paste(
        "\n--- Processando:", apelido, "\ncamara:", id_camara,
        "\nsenado", id_senado, "\n"))

    etapas <- list()
    if (id_camara) {
        etapas %<>%
            append(process_etapa(id_camara, "camara"))
    }
    if (id_senado) {
        etapas %<>%
            append(process_etapa(id_senado, "senado"))
    }

    print(etapas)
    ## progresso = progresso_pl,
    ## progresso_pl <- agoradigital::get_progresso(id_camara, id_senado)
    etapas %>%
        purrr::map_df( ~ data_frame(x = .x))
        ## dplyr::mutate(apelido = apelido, tema = tema)
}

#' @title Exporta dados de proposições
#' @description Exporta para uma pasta CSVs com dados sobre uma lista de proposições.
#' @param pls dataframe com proposições.
#' @param export_path pasta para onde exportar dados.
#' @export
export_data <- function(pls, export_path) {
  ## res <-
  ##   purrr::pmap(
  ##     list(pls$id_camara, pls$id_senado, pls$apelido, pls$tema),
  ##     function(x, y, z, w, df) process_pl(x, y, z, w))

  res <- pls %>% purrr::pmap(process_pl)
    ## %>% dplyr::bind_rows()

  proposicoes <-
    purrr::map_df(res, ~ .$proposicao) %>%
    dplyr::select(-ano)
  tramitacoes <- purrr::map_df(res, ~ .$fases_eventos)
  hists_energia <- purrr::map_df(res, ~ .$hist_energia)
  progressos <-
    purrr::map_df(res, ~ .$progresso) %>%
    dplyr::select(
      prop_id, casa, local_casa, fase_global, local, data_inicio, data_fim)

  ## rename columns
  names(proposicoes) <- c(
    "id_ext", "casa", "sigla_tipo", "numero", "data_apresentacao", "ementa",
    "palavras_chave", "casa_origem", "autor_nome", "tema", "apelido",
    "regime_tramitacao", "forma_apreciacao", "em_pauta", "energia")
  names(tramitacoes) <- c(
    "id_ext", "casa", "data", "sequencia", "texto_tramitacao", "sigla_local",
    "id_situacao", "descricao_situacao", "fase", "situacao_descricao_situacao",
    "evento", "data_audiencia", "local", "global")
  names(progressos) <- c(
      "id_ext", "casa", "local_casa", "fase_global", "local", "data_inicio",
      "data_fim")

  ## export data to CSVs
  readr::write_csv(proposicoes, paste0(export_path, "/proposicoes.csv"))
  readr::write_csv(tramitacoes, paste0(export_path, "/trams.csv"))
  readr::write_csv(
      hists_energia, paste0(export_path, "/hists_energia.csv"))
  readr::write_csv(progressos, paste0(export_path, "/progressos.csv"))
}
