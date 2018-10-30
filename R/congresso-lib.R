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
  labels <- list("Construção", "Revisão I")
  casa_label <-
    proposicao_df %>%
    dplyr::arrange(data_apresentacao) %>%
    dplyr::select(casa) %>%
    dplyr::mutate(label = head(labels, nrow(proposicao_df)))
  rownames(casa_label) <- casa_label$casa

  ## Roda função específica para cada casa
  extract_casas_subgroups <- function(tram) {
      casa <- tolower(tram[1, ]$casa)
      label <- casa_label[casa, "label"][[1]]
      if (casa == congress_constants$camara_label) {
        df <- agoradigital:::extract_casas_in_camara(tram, label)
      } else if (casa == congress_constants$senado_label) {
        df <- agoradigital:::extract_casas_in_senado(tram, label)
      }
      df %>% dplyr::mutate(local_casa = casa)
  }

  tramitacao_df %>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::group_by(casa) %>%
    dplyr::do(extract_casas_subgroups(.)) %>%
    dplyr::ungroup() %>%
    tidyr::fill(fase_global)
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
    dplyr::arrange(data_hora) %>%
    dplyr::filter(fase_global != "NA" & local != "NA") %>%
    dplyr::mutate(end_data = dplyr::lead(data_hora)) %>%
    dplyr::group_by(
      casa, prop_id, fase_global, local, sequence = data.table::rleid(fase_global)) %>%
    dplyr::summarise(data_inicio = min(data_hora), data_fim = max(end_data)) %>%
    dplyr::filter(data_fim - data_inicio > 0) %>%
    dplyr::select(-sequence) %>%
    dplyr::group_by(fase_global) %>%
    dplyr::mutate(data_fim_anterior = dplyr::lag(data_fim)) %>%
    dplyr::filter(is.na(data_fim_anterior) | data_fim > data_fim_anterior) %>%
    dplyr::select(-data_fim_anterior)

  if (nrow(
      df %>%
        dplyr::group_by(fase_global, local) %>%
        dplyr::filter(n() > 1))
      > 0) {
    df %<>%
      dplyr::group_by(prop_id, casa, fase_global, local) %>%
      dplyr::summarise(data_inicio = min(data_inicio),
                       data_fim = max(data_fim)) %>%
      dplyr::right_join(congress_env$fases_global, by = c("local", "fase_global"))
  } else {
    df %<>%
      dplyr::right_join(congress_env$fases_global, by = c("local", "fase_global"))
  }

  df %>% dplyr::ungroup()
}
