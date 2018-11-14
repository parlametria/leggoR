process_etapa <- function(id, casa, agenda) {
    prop <- agoradigital::fetch_proposicao(id, casa)
    tram <- agoradigital::fetch_tramitacao(id, casa, TRUE)
    proc_tram <-
        agoradigital::process_proposicao(prop, tram, casa) %>%
        dplyr::mutate(data_hora = as.POSIXct(data_hora))
    status <- agoradigital::extract_status_tramitacao(tram, agenda)
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
        dplyr::mutate(energia = energia_value) %>%
        dplyr::select(-casa_origem)

    list(
        proposicao = extended_prop,
        fases_eventos = proc_tram,
        hist_energia = historico_energia)
}

process_pl <- function(id_camara, id_senado, apelido, tema_pl, agenda) {
    cat(paste(
        "\n--- Processando:", apelido, "\ncamara:", id_camara,
        "\nsenado", id_senado, "\n"))
    etapas <- list()
    if (!is.na(id_camara)) {
        etapas %<>% append(list(process_etapa(id_camara, "camara", agenda)))
    }
    if (!is.na(id_senado)) {
        etapas %<>% append(list(process_etapa(id_senado, "senado", agenda)))
    }
    etapas %<>% purrr::pmap(dplyr::bind_rows)
    etapas[["progresso"]] <-
        agoradigital::get_progresso(etapas$proposicao, etapas$fases_eventos)
    etapas$proposicao %<>%
        dplyr::mutate(apelido_materia = apelido, tema = tema_pl)
    etapas
}

#' @title Exporta dados de proposições
#' @description Exporta para uma pasta CSVs com dados sobre uma lista de proposições.
#' @param pls dataframe com proposições.
#' @param export_path pasta para onde exportar dados.
#' @export
export_data <- function(pls, export_path) {
  agenda <- fetch_agenda_geral(as.Date(cut(Sys.Date(), "week")), as.Date(cut(Sys.Date(), "week")) + 4)
  res <- pls %>% purrr::pmap(process_pl, agenda)

  proposicoes <-
    purrr::map_df(res, ~ .$proposicao) %>%
    dplyr::select(-ano) %>%
    dplyr::rename(id_ext = prop_id, sigla_tipo = tipo_materia, apelido = apelido_materia)
  tramitacoes <-
    purrr::map_df(res, ~ .$fases_eventos) %>%
    dplyr::rename(id_ext = prop_id, data = data_hora)
  hists_energia <- purrr::map_df(res, ~ .$hist_energia)
  progressos <-
    purrr::map_df(res, ~ .$progresso) %>%
    dplyr::rename(id_ext = prop_id)

  ## export data to CSVs
  readr::write_csv(proposicoes, paste0(export_path, "/proposicoes.csv"))
  readr::write_csv(tramitacoes, paste0(export_path, "/trams.csv"))
  readr::write_csv(
      hists_energia, paste0(export_path, "/hists_energia.csv"))
  readr::write_csv(progressos, paste0(export_path, "/progressos.csv"))
}
