congresso_env <- jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))

#' @title Verfica se um elemento está contido em um vetor
#' @description Verfica se um elemento está contido em um vetor
#' @param element Elemento que pode estar contido
#' @param set Vetor de elementos
#' @return Valor booleano que indica se o elemento está contido no vetor.
#' @export
detect_fase <- function(element, set) {
  element %in% set
}

#' @title Extrai as casas globais de um PL
#' @description Extrai as casas globais de um PL
#' @param full_proposicao_df Dataframe da proposição do PL.
#' @param full_tramitacao_df Dataframe da tramitação do PL.
#' @return Dataframe com uma nova coluna chamada fase_global
extract_casas <- function(full_proposicao_df, full_tramitacao_df){
  eventos_fases <- congresso_env$eventos_fases
  
  full_ordered_tram <- full_tramitacao_df %>% dplyr::arrange(data_hora)
  
  #number delimiting events
  full_ordered_tram <- full_ordered_tram %>%
    dplyr::group_by(evento) %>%
    dplyr::mutate(evento_num = dplyr::if_else(evento %in% c('apresentacao_pl','virada_de_casa','remetida_a_sancao'),
                                              paste0(evento,dplyr::row_number()),
                                              '')) %>%
    dplyr::ungroup()
  
  #label first event when happened before the presentation
  if (full_ordered_tram[1,"evento_num"] != 'apresentacao_pl1')
    full_ordered_tram[1,"evento_num"] <- 'primeiro_evento'
  
  delimiting_events <- full_ordered_tram %>% dplyr::filter(evento_num != '') %>%
    dplyr::left_join(eventos_fases, by = 'evento_num')
  
  camara_ordered_tram <- tibble::tibble()
  senado_ordered_tram <- tibble::tibble()
  
  if ('camara' %in% full_ordered_tram$casa) {
    camara_ordered_tram <- full_ordered_tram %>%
      dplyr::filter(casa == 'camara') %>%
      dplyr::left_join(eventos_fases, by = 'evento_num') %>%
      tidyr::fill(fase_global) %>% 
      dplyr::bind_rows(delimiting_events) %>%
      dplyr::arrange(data_hora) %>%
      dplyr::distinct() %>%
      tidyr::fill(fase_global, .direction = 'up') %>%
      dplyr::filter(casa == 'camara') %>%
      extract_local_global_in_camara() %>%
      dplyr::group_by(fase_global) %>%
      tidyr::fill(local) %>%
      dplyr::ungroup()  
  }
  
  
  if ('senado' %in% full_ordered_tram$casa) {
    senado_ordered_tram <- full_ordered_tram %>%
      dplyr::filter(casa == 'senado') %>%
      dplyr::left_join(eventos_fases, by = 'evento_num') %>%
      tidyr::fill(fase_global) %>%
      dplyr::bind_rows(delimiting_events) %>%
      dplyr::arrange(data_hora) %>%
      dplyr::distinct() %>%
      tidyr::fill(fase_global, .direction = 'up') %>%
      dplyr::filter(casa == 'senado') %>%
      extract_local_global_in_senado() %>%
      dplyr::group_by(fase_global) %>%
      tidyr::fill(local) %>%
      dplyr::ungroup()
  }
  
  full_ordered_tram_fases <- dplyr::bind_rows(camara_ordered_tram,senado_ordered_tram) %>%
    dplyr::distinct() %>%
    dplyr::arrange(data_hora) %>%
    dplyr::mutate(local_casa = dplyr::if_else(fase_global == 'Sanção/Veto','presidência da república',
                                              dplyr::if_else(fase_global == 'Avaliação dos Vetos','congresso',casa)))
}

#' @title Recupera o progresso de um PL
#' @description Retorna um dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @param df Dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @return Dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @examples
#'  generate_progresso_df(fetch_tramitacao(2121442, 'camara', T))
generate_progresso_df <- function(tramitacao_df){
  df <-
    tramitacao_df %>%
    dplyr::arrange(data_hora, fase_global)  %>%
    dplyr::filter((!is.na(fase_global)) & (!is.na(local))) %>% 
    dplyr::mutate(end_data = dplyr::lead(data_hora)) %>%
    dplyr::group_by(
      casa, prop_id, fase_global, local, sequence = data.table::rleid(fase_global)) %>%
    dplyr::summarise(
      data_inicio = min(data_hora, na.rm = T),
      data_fim = max(end_data, na.rm = T)) %>%
    dplyr::select(-sequence) %>%
    dplyr::group_by(fase_global) %>%
    dplyr::mutate(data_fim_anterior = dplyr::lag(data_fim)) %>%
    dplyr::filter(is.na(data_fim_anterior) | data_fim > data_fim_anterior) %>%
    dplyr::select(-data_fim_anterior) %>%
    dplyr::arrange(data_inicio)

  if (nrow(df %>% dplyr::group_by(fase_global, local) %>% dplyr::filter(dplyr::n() > 1)) > 0) {
    df %<>%
      dplyr::group_by(prop_id, casa, fase_global, local) %>%
      dplyr::summarise(data_inicio = min(data_inicio),
                       data_fim = max(data_fim)) %>% 
      dplyr::arrange(data_inicio)
  } 
  
  df$data_fim[nrow(df)] <- NA
  
  df %<>%
    dplyr::right_join(congresso_env$fases_global, by = c("local", "fase_global")) %>% 
    dplyr::ungroup()
  
  if (sum(is.na(df$casa)) == nrow(df)) {
    tramitacao_df <- 
      tramitacao_df %>%
      dplyr::select(prop_id, casa) %>% 
      head(1)
    df <-
      df %>%
      dplyr::mutate(casa = tramitacao_df$casa) %>%
      dplyr::mutate(prop_id = tramitacao_df$prop_id)
  }
  
  #Adding correct casa column value for phases: Sanção/Veto and Avaliação dos Vetos.
  df <- df %>%
    dplyr::mutate(local_casa = dplyr::if_else(fase_global %in% c('Sanção/Veto','Avaliação dos Vetos'),
                                             tolower(local),
                                             casa))
  
  return(df)
}

#' @title Retorna um progresso de uma mpv
#' @description Retorna um dataframe contendo os dados sobre o progresso de uma mpv
#' @param tramitacao_df Dataframe processessado da tramitação
#' @return Dataframe contendo o progresso
#' @export
generate_progresso_df_mpv <- function(tramitacao_df) {
  tramitacao_df <-
    tramitacao_df %>% 
    dplyr::arrange(data_hora) %>% 
    dplyr::mutate(fase_global = 
                    dplyr::case_when(
                      destino_tramitacao_local_nome_casa_local == "Câmara dos Deputados" ~ destino_tramitacao_local_nome_casa_local,
                      stringr::str_detect(tolower(texto_tramitacao), "encaminhada ao senado federal") ~ "Senado Federal",
                      stringr::str_detect(tolower(texto_tramitacao), "sancionada") ~ 
                        dplyr::if_else(stringr::str_detect(tolower(texto_tramitacao), "vetada"), "Transformada em Lei com vetos","Transformada em Lei"),
                      dplyr::row_number() == 1 ~ "Comissão Mista")) %>% 
    tidyr::fill(fase_global)
  
  df <-
    tramitacao_df %>%
    dplyr::mutate(end_data = dplyr::lead(data_hora)) %>%
    dplyr::group_by(
      casa, prop_id, fase_global, sequence = data.table::rleid(fase_global)) %>%
    dplyr::summarise(
      data_inicio = min(data_hora, na.rm = T),
      data_fim = max(end_data, na.rm = T)) %>%
    dplyr::select(-sequence) %>%
    dplyr::group_by(fase_global) %>%
    dplyr::mutate(data_fim_anterior = dplyr::lag(data_fim)) %>%
    dplyr::select(-data_fim_anterior) %>%
    dplyr::arrange(data_inicio)
  
  if (nrow(df) == 1) {
    df <- 
      df %>% 
      dplyr::mutate(data_fim = NA)
  }
  
  df <-
    df %>% 
    dplyr::right_join(congresso_env$fases_global_mpv, by = c("fase_global")) %>% 
    dplyr::ungroup()
  
  df %>% 
    tidyr::fill(casa, prop_id) %>% 
    unique() %>% 
    dplyr::mutate(data_fim =
                    dplyr::if_else(fase_global %in% c("Transformada em Lei", "Transformada em Lei com vetos"),
                                   data_inicio,
                                   data_fim)) %>% 
    dplyr::arrange(data_inicio) 
}

#' @title Recupera o número de linha em que houve virada_de_casa
#' @description Recupera o número da linha em que houve evento virada_de_casa
#' @param df Dataframe da tramitação processada da proposiçao
#' @return número da ultima linha cujo evento é virada_de_casa
#' @examples
#'  get_linha_virada_de_casa(fetch_tramitacao(2121442, 'camara', T) %>% extract_events_in_camara())
get_linha_virada_de_casa <- function(proc_tram_df) {
  linha_virada_de_casa = which(proc_tram_df$evento == 'virada_de_casa')
  return(ifelse(length(linha_virada_de_casa) == 0, nrow(proc_tram_df), linha_virada_de_casa))
}

#' @title Recupera o número de linha em que houve evento vetada_totalmente ou transformada_lei
#' @description Recupera o número da linha em que houve evento vetada_totalmente ou transformada_lei
#' @param df Dataframe da tramitação processada da proposiçao
#' @return número da ultima linha cujo evento é vetada_totalmente ou transformada_lei
#' @examples
#'  get_linha_finalizacao_tramitacao(fetch_tramitacao(2121442, 'camara', T) %>% extract_events_in_camara())
get_linha_finalizacao_tramitacao <- function(proc_tram_df) {
  linha_vetada = which(proc_tram_df$evento == 'vetada_totalmente')
  linha_lei = which(proc_tram_df$evento == 'transformada_lei')
  return(ifelse(length(linha_vetada) == 0, ifelse(length(linha_lei) == 0, nrow(proc_tram_df), linha_lei), linha_vetada))
}
