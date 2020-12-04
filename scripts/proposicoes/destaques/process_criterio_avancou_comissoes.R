library(tidyverse)
library(here)
library(lubridate)

#' @title Verifica se proposições passaram pela CCJ.
#' @description Processa verificação das proposições que passaram ou não na CCJ.
#' @param proposicoes_df Dataframe de proposições.
#' @param tramitacoes_df Dataframe de tramitações.
#' @return Dataframe de proposicoes_df com coluna a mais indicando se a proposição passou ou não pela CCJ.
#' @examples
#' .verifica_local_ccj(proposicoes_df, tramitacoes_df)
.verifica_local_ccj <- function(proposicoes_df, tramitacoes_df) {
  tram_ccj <- tramitacoes_df %>%
    dplyr::filter(sigla_local %in% c("CCJ", "CCJC")) %>%
    dplyr::filter(!is.na(evento)) %>%

    group_by(id_ext, casa, sigla_local) %>%
    summarise(eventos = paste(evento, collapse = ";")) %>%
    ungroup() %>%

    dplyr::filter(!str_detect(eventos, "parecer_pela_rejeicao"))

  proposicoes_ccj <- proposicoes_df %>%
    left_join(tram_ccj, by = c("id_ext", "casa")) %>%
    mutate(ccj = !is.na(sigla_local)) %>%
    select(id_ext, casa, id_leggo, ccj)

  return(proposicoes_ccj)
}

#' @title Verifica se proposições tiveram parecer aprovado em alguma comissão.
#' @description Processa verificação das proposições que tiveram ou não parecer aprovado em alguma comissão.
#' @param proposicoes_df Dataframe de proposições.
#' @param tramitacoes_df Dataframe de tramitações.
#' @return Dataframe de proposicoes_df com coluna a mais indicando se a proposição teve parecer aprovado em alguma
#' comissão ou não.
#' @examples
#' .verifica_aprovacao_parecer(proposicoes_df, tramitacoes_df)
.verifica_aprovacao_parecer <- function(proposicoes_df, tramitacoes_df) {
  ## Lista de locais que não são comissões
  locais_filtro <- c("mesa", "ata-plen", "plen", "seadi")

  eventos_agrupados <- tramitacoes_df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::filter(!tolower(sigla_local) %in% locais_filtro) %>%
    group_by(id_ext, casa, sigla_local) %>%
    summarise(eventos = paste(evento, collapse = ";")) %>%
    ungroup()

  eventos_parecer_aprovado <- eventos_agrupados %>%
    dplyr::filter(str_detect(eventos, "aprovacao_parecer"),
                  !str_detect(eventos, "parecer_pela_rejeicao"))

  proposicoes_aprovadas <- eventos_parecer_aprovado %>%
    group_by(id_ext, casa) %>%
    summarise(comissoes_aprovadas = paste(sigla_local, collapse = ";")) %>%
    ungroup()

  proposicoes_parecer_aprovado <- proposicoes_df %>%
    left_join(proposicoes_aprovadas, by = c("id_ext", "casa")) %>%
    mutate(parecer_aprovado = !is.na(comissoes_aprovadas)) %>%
    select(id_ext, casa, id_leggo, parecer_aprovado)

  return(proposicoes_parecer_aprovado)
}

#' @title Processa critério de parecer aprovado em comissão.
#' @description Processa o critério que retorna informações de proposições que tiveram algum parecer
#' aprovado em uma comissão.
#' @param tramitacoes_datapath Caminho para o CSV de tramitações.
#' @return Dataframe com informações de proposições com parecer aprovado em comissões.
#' @examples
#' process_criterio_avancou_comissoes()
process_criterio_avancou_comissoes <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  tramitacoes_datapath = here::here("leggo_data/trams.csv")) {

  tramitacoes <- read_csv(tramitacoes_datapath,
                          col_types = cols(data_audiencia = col_datetime(),
                                           data = col_datetime(),
                                           sequencia = col_integer(),
                                           .default = col_character()))

  ano_atual <- lubridate::year(Sys.time())
  ## Filtra eventos dos últimos 4 anos
  tramitacoes <- tramitacoes %>%
    dplyr::filter(lubridate::year(data) > (ano_atual - 4))

  proposicoes <- read_csv(proposicoes_datapath,
                          col_types = cols(id_ext = col_character())) %>%
    select(id_ext, casa, id_leggo)

  proposicoes_destaque_camara <- proposicoes %>%
    filter(casa == "camara") %>%
    .verifica_local_ccj(tramitacoes)

  proposicoes_destaque_senado <- proposicoes %>%
    filter(casa == "senado") %>%
    .verifica_aprovacao_parecer(tramitacoes)

  proposicoes_merge <- proposicoes_destaque_camara %>%
    bind_rows(proposicoes_destaque_senado) %>%
    select(id_leggo, id_ext, casa, ccj_camara = ccj, parecer_aprovado_comissao = parecer_aprovado) %>%
    distinct(id_leggo, id_ext, casa, .keep_all = T)

  proposicoes_alt <- proposicoes_merge %>%
    mutate_at(.funs = list(~replace_na(., FALSE)),
              .vars = vars(ccj_camara, parecer_aprovado_comissao)) %>%
    group_by(id_leggo) %>%
    summarise(ccj_camara = sum(ccj_camara),
              parecer_aprovado_comissao = max(parecer_aprovado_comissao)) %>%
    ungroup() %>%
    distinct(id_leggo, .keep_all = T) %>%
    mutate(ccj_camara = as.logical(ccj_camara),
           parecer_aprovado_comissao = as.logical(parecer_aprovado_comissao))

  return(proposicoes_alt)
}
