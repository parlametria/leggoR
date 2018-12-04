process_etapa <- function(id, casa, agenda) {
    prop <- agoradigital::fetch_proposicao(id, casa)
    tram <- agoradigital::fetch_tramitacao(id, casa, TRUE)
    proc_tram <-
        agoradigital::process_proposicao(prop, tram, casa) %>%
        dplyr::mutate(data_hora = as.POSIXct(data_hora))
    status <- agoradigital::extract_status_tramitacao(id, casa)
    historico_temperatura <-
        agoradigital::get_historico_temperatura_recente(proc_tram) %>%
        dplyr::mutate(id_ext = prop$prop_id, casa = prop$casa) %>%
        dplyr::select(id_ext, casa, periodo, temperatura_periodo, temperatura_recente)
    temperatura_value <-
        historico_temperatura %>%
        dplyr::slice(n()) %>%
        .$temperatura_recente
    extended_prop <-
        merge(prop, status, by = "prop_id") %>%
        dplyr::mutate(temperatura = temperatura_value) %>%
        dplyr::select(-casa_origem)

    list(
        proposicao = extended_prop,
        fases_eventos = proc_tram,
        hist_temperatura = historico_temperatura)
}

adiciona_coluna_pulou <- function(progresso_df) {
  progresso_df$prox_local_casa = dplyr::lead(progresso_df$local_casa)
  
  progresso_df %>%
    dplyr::mutate(
      pulou = dplyr::if_else((is.na(local_casa) & !is.na(prox_local_casa)), T, F)) %>%
    dplyr::select(-prox_local_casa)
}

adiciona_locais_faltantes_progresso <- function(progresso_df) {
  progresso_df %>%
    dplyr::mutate(local_casa = dplyr::case_when(
      local %in% c('Presidência da República', 'Congresso') ~
        tolower(local),
      (fase_global == "Revisão I" & is.na(local_casa)) ~ ifelse(.$casa[[1]] == 'senado', 'camara', 'senado'),
      fase_global == "Revisão II" ~ .$casa[[1]],
      is.na(local_casa) ~ casa,
      TRUE ~ local_casa))
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
        agoradigital::get_progresso(etapas$proposicao, etapas$fases_eventos) %>%
      adiciona_coluna_pulou() %>%
      adiciona_locais_faltantes_progresso()
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
  hists_temperatura <- purrr::map_df(res, ~ .$hist_temperatura)
  progressos <-
    purrr::map_df(res, ~ .$progresso) %>%
    dplyr::rename(id_ext = prop_id)

  ## export data to CSVs
  readr::write_csv(proposicoes, paste0(export_path, "/proposicoes.csv"))
  readr::write_csv(tramitacoes, paste0(export_path, "/trams.csv"))
  readr::write_csv(
      hists_temperatura, paste0(export_path, "/hists_temperatura.csv"))
  readr::write_csv(progressos, paste0(export_path, "/progressos.csv"))
}
