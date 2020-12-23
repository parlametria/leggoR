
#' @title Processa o critério de proposições com requerimento de urgência apresentado e/ou aprovado;
#' @description Processa o critério que retorna informações de proposições com requerimento de
#' urgência aprovado ou apresentado;
#' @param proposicoes_datapath Datapath de proposições
#' @param trams_datapath Datapath de tramitações
#' @return Dataframe contendo as proposições que passam no critério.

process_criterio_requerimento_urgencia <- function(trams_datapath = here::here("leggo_data/trams.csv"),
                                                   props_datapath = here::here("leggo_data/proposicoes.csv") ){

  trams <- read_csv(trams_datapath, col_types = cols(id_ext = col_character(),
                                                     origem_tramitacao_local_nome_casa_local = col_character())) %>%
    select(evento, id_ext, casa, data)

  # Filtra eventos dos últimos 4 anos
  hoje <- Sys.time()
  trams <- trams %>%
    mutate(idade = lubridate::interval(data, hoje) %>%
             as.numeric('years')) %>%
    dplyr::filter(idade <= 4) %>%
    select(-idade)

  props <- read_csv(props_datapath, col_types = cols(id_ext = col_character())) %>%
    select(id_ext, casa, id_leggo)

  proposicoes_requerimento_urgencia <- trams %>%
  left_join(props, by = c("id_ext", "casa")) %>%
  filter(str_detect(evento, "requerimento_urgencia_apresentado|requerimento_urgencia_aprovado"))

  # remove as repetições de proposições
  proposicoes_sem_repetidos <- proposicoes_requerimento_urgencia %>% arrange(id_leggo, id_ext, casa, data) %>%
    distinct(id_leggo, id_ext, casa, .keep_all = TRUE)

  proposicoes_requerimento_urgencia_final <-
    proposicoes_sem_repetidos %>%
    spread(evento, data) %>%
    group_by(id_leggo) %>%
    mutate(
      casa_req_urgencia_apresentacao = if_else(
        is.na(requerimento_urgencia_apresentado),
        as.character(NA),
        casa
      ),
      casa_req_urgencia_aprovacao = if_else(
        is.na(requerimento_urgencia_aprovado),
        as.character(NA),
        casa
      )
    ) %>%
    fill(requerimento_urgencia_apresentado, .direction = "downup") %>%
    fill(requerimento_urgencia_aprovado, .direction = "downup") %>%
    fill(casa_req_urgencia_apresentacao, .direction = "downup") %>%
    fill(casa_req_urgencia_aprovacao, .direction = "downup") %>%
    mutate(
      requerimento_urgencia_apresentado = max(requerimento_urgencia_apresentado),
      requerimento_urgencia_aprovado = max(requerimento_urgencia_aprovado)
    ) %>%
    ungroup() %>%
    select(
      id_leggo,
      data_req_urgencia_apresentacao = requerimento_urgencia_apresentado,
      data_req_urgencia_aprovacao = requerimento_urgencia_aprovado,
      casa_req_urgencia_apresentacao,
      casa_req_urgencia_aprovacao
    ) %>%
    distinct()

  return(proposicoes_requerimento_urgencia_final)

}
