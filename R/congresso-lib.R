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
#' @param proposicao_df Dataframe da tramitação do PL.
#' @param tramitacao_df Dataframe da proposição do PL.
#' @param casa Casa (Senado ou Câmara)
#' @return Dataframe com uma nova coluna chamada fase_global
extract_casas <- function(tramitacao_df, proposicao_df){
  ## Prepara tabela que mapeia casa -> label
  labels <- list("Construção", "Revisão I", "Revisão II", "Sanção/Veto")
  casa_label <- tramitacao_df %>%
    dplyr::mutate(data = lubridate::floor_date(data_hora, unit="day")) %>%
    dplyr::arrange(data) %>%
    
    dplyr::group_by(
      casa, sequence = data.table::rleid(casa)) %>%
    dplyr::summarise(
      data_inicio = min(data_hora, na.rm = T),
      data_fim = max(data_hora, na.rm = T)) %>%
    dplyr::arrange(sequence) 
  
  if(nrow(casa_label) > 1) {  
    casa_label <-
      casa_label %>%
      dplyr::filter(data_inicio < data_fim)
  }
  
  sequencias <- casa_label$sequence
  casa_label <-
    casa_label %>%
    dplyr::select(casa) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(label = head(labels, nrow(casa_label)))

  ## Roda função específica para cada casa
  extract_casas_subgroups <- function(tram, row_num) {
      casa <- tolower(tram[1, ]$casa)
      prop_id <- tram[1, ]$prop_id
      label <- casa_label[["label"]][[row_num[[1]]]]
      if (casa == congress_constants$camara_label) {
        df <- extract_casas_in_camara(tram, label)
      } else if (casa == congress_constants$senado_label) {
        df <- extract_casas_in_senado(tram, label)
      }
      df %>% dplyr::mutate(local_casa = casa)
  }

  tramitacao_df %>%
    dplyr::arrange(data_hora) %>%
    dplyr::mutate(sequence = data.table::rleid(casa)) %>%
    dplyr::filter((sequence %in% sequencias)) %>%
    dplyr::group_by(
      casa, sequence_2 = data.table::rleid(casa)) %>%
    dplyr::do(extract_casas_subgroups(., .$sequence_2)) %>%
    dplyr::mutate(fase_global = dplyr::if_else(global == paste0('- ', labels[[4]]), labels[[4]], fase_global)) %>% 
    dplyr::ungroup() %>%
    tidyr::fill(fase_global) %>%
    dplyr::mutate(local_casa = dplyr::if_else(!is.na(global) & global == paste0('- ', labels[[4]]), 'presidencia', local_casa)) %>% 
    dplyr::select(-c(sequence_2, sequence))
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
    dplyr::arrange(data_hora, fase_global) %>%
    dplyr::filter((!is.na(fase_global) & !is.na(local)) | !is.na(evento)) %>%
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

  if (nrow(df %>% dplyr::group_by(fase_global, local) %>% dplyr::filter(n() > 1)) > 0) {
    df %<>%
      dplyr::group_by(prop_id, casa, fase_global, local) %>%
      dplyr::summarise(data_inicio = min(data_inicio),
                       data_fim = max(data_fim)) %>% 
      dplyr::arrange(data_inicio)
  } 
  
  df$data_fim[nrow(df)] <- NA
  df <-
    df %>%
    dplyr::mutate(local = ifelse(is.na(local) & nrow(.) == 1, "Comissões", local))
  
  df %<>%
    dplyr::right_join(congresso_env$fases_global, by = c("local", "fase_global")) %>% 
    dplyr::ungroup()
  
  return(df)
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
