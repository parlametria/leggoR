#' @title Adiciona o local para o vistime
#' @description Adiciona o label local com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' fetch_tramitacao(91341, 'senado', T) %>% format_local()
format_local <- function(df) {
  local_df <-
    df %>%
    dplyr::mutate(z = cumsum(local != dplyr::lag(local, default='NULL')), 
           end_data = dplyr::lead(data_hora)) %>%
    dplyr::group_by(local, sequence = data.table::rleid(z)) %>%
    dplyr::summarize(start = min(data_hora),
              end = dplyr::if_else(is.na(max(end_data)),max(data_hora),max(end_data)),
              time_interval = end - start) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(sequence) %>%
    dplyr::select(-sequence) %>%
    dplyr::filter(time_interval > 0) %>%
    dplyr::rename(label=local) %>%
    dplyr::mutate(group = "Comissão")

  local_df %>%
    dplyr::mutate(color = dplyr::case_when((grepl('^S', label) | 
                               label == 'ATA-PLEN') ~ "#e2e3d9",
                               label == 'PLEN' ~ "#5496cf",
                               label == 'CCJ' ~ "#ffffcc",
                               label == 'CAE' ~ "#a1dab4",
                               label == 'CDR' ~ "#225ea8"))
}

#' @title Adiciona a fase para o vistime
#' @description Adiciona o label fase com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' fetch_tramitacao(91341, 'senado', T) %>% format_fase()
format_fase <- function(df) {
  df <- 
    # Improve the phases names and convert data_tramitacao to Date
    df %>%
    dplyr::mutate(
      data_hora = as.Date(data_hora),
      fase = as.character(fase)
    )
  
  df <- df %>%
    dplyr::mutate(z = cumsum(fase != dplyr::lag(fase, default='NULL')), 
           end_data = dplyr::lead(data_hora)) %>%
    dplyr::group_by(fase, sequence = data.table::rleid(z)) %>%
    dplyr::summarize(start = min(data_hora),
              end = dplyr::if_else(is.na(max(end_data)),max(data_hora),max(end_data)),
              time_interval = end - start) %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(sequence) %>% 
    dplyr::select(-sequence) %>%
    #filter(time_interval > 0) %>%
    dplyr::rename(label=fase) %>%
    dplyr::mutate(group = "Situação na comissão")
  
  df %>% 
    dplyr::mutate(color = dplyr::case_when(label == "Recebimento" ~ "#d7191c",
                           label == "Análise do relator" ~ "#fdae61",
                           label == "Discussão e votação" ~ "#ffffbf",
                           label == "Encaminhamento" ~ "#abd9e9"))
}

#' @title Adiciona os eventos para o vistime
#' @description Adiciona o label evento com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' fetch_tramitacao(91341, 'senado', T) %>% format_eventos()
format_eventos <- function(df) {
  df <-
    # Improve the phases names and convert data_hora to Date
    df %>%
    dplyr::mutate(
      data_hora = dplyr::if_else(is.na(data_audiencia), as.Date(data_hora), as.Date(data_audiencia)),
      evento = as.character(evento)
    )
  
  df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::mutate(start = data_hora,
           end = start,
           time_interval = end - start) %>%
    dplyr::rename(label=evento) %>%
    unique() %>%
    dplyr::mutate(group = "Evento", color = "#a9a9a9") %>%
    dplyr::select(label, start, end, time_interval, group, color)
  
}

#' @title Adiciona a fase global para o vistime
#' @description Adiciona o label global com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' fetch_tramitacao(91341, 'senado', T) %>% format_fase_global()
format_fase_global <- function(df) {
  
  df %>%
    dplyr::mutate(global = if_else(casa == "Mesa - Senado", paste0("Apresentação ", global), paste0(local, " ", global))) %>%
    dplyr::mutate(z = cumsum(global != dplyr::lag(global, default='NULL')),
           end_data = dplyr::lead(data_hora)) %>%
    dplyr::group_by(global, sequence = data.table::rleid(z)) %>%
    dplyr::summarize(start = min(data_hora),
              end = dplyr::if_else(is.na(max(end_data)),max(data_hora),max(end_data)),
              time_interval = end - start) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(sequence) %>%
    dplyr::select(-sequence) %>%
    dplyr::filter(!is.na(global)) %>%
    dplyr::rename(label=global) %>%
    dplyr::mutate(group = "Global",
           color = dplyr::case_when(stringr::str_detect(label, "Plenário") ~ "#5496cf",
                             stringr::str_detect(label, "CCJ") ~ "#ffffcc",
                             stringr::str_detect(label, "CAE") ~ "#a1dab4",
                             stringr::str_detect(label, "CDR") ~ "#225ea8",
                             stringr::str_detect(label, "CDR") ~ "#568245",
                             stringr::str_detect(label, "CI") ~ "#454682",
                             stringr::str_detect(label, "Apresentação") ~ "#d6952a"))
  
}

#' @title Formata tabela para o vistime
#' @description Formata a tabela final que será usado para fazer a visualização
#' usando o vistime
#' @param tram_senado_df dataframe da tramitação do PL no Senado
#' @examples
#' build_vis_csv_senado(fetch_tramitacao(91341, 'senado', T))
build_vis_csv_senado <- function(tram_senado_df, output_folder=NULL) {
  bill_id <- tram_senado_df[1, "prop_id"]
  tram_senado_filtered <- 
    tram_senado_df %>%
    dplyr::select(data_hora, local, evento, casa, global, data_audiencia)
  
  vis_df <- 
    rbind(format_local(tram_senado_filtered),
          format_fase_global(tram_senado_filtered),
          #format_fase(tram_senado_filtered),
          format_eventos(tram_senado_filtered)) %>%
    dplyr::filter(time_interval != 0 | group == "Evento") %>%
    dplyr::filter(group != 'Comissão')
  
  vis_df %>%
    readr::write_csv(paste0(output_folder,'/vis/tramitacao/',bill_id,"-data-senado.csv"))
}

