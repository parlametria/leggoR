library(tidyverse)

#' @title Define critérios de destaque para proposições
#' @description A partir de vários critérios das proposições, define atributos que as tornam destaque
#' @param proposicoes_datapath Caminho para o arquivo de proposições
#' @param progressos_datapath Caminho para o arquivo de progressos das proposições
#' @param tramitacoes_datapath Caminho para o arquivo de tramitações das proposições
#' @param interesses_datapath Caminho para o arquivo de interesses
#' @param pressao_datapath: Caminho para o arquivo de pressão das proposições
#' @return Um só dataframe contendo todas as proposições com seus atributos de destaque.
process_proposicoes_destaques <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  progressos_datapath = here::here("leggo_data/progressos.csv"),
  tramitacoes_datapath = here::here("leggo_data/trams.csv"),
  interesses_datapath = here::here("leggo_data/interesses.csv"),
  pressao_datapath = here::here("leggo_data/pressao.csv")) {
  library(lubridate)

  source(here::here("scripts/proposicoes/destaques/process_criterio_aprovada_em_uma_casa.R"))
  source(here::here("scripts/proposicoes/destaques/process_criterio_avancou_comissoes.R"))
  source(here::here("scripts/proposicoes/destaques/process_criterio_pressao_alta.R"))
  source(here::here("scripts/proposicoes/destaques/process_criterio_requerimento_urgencia.R"))
  source(here::here("scripts/proposicoes/destaques/fetcher_criterio_mais_comentadas_twitter.R"))

  interesses <- read_csv(interesses_datapath)

  interesses_agendas <- interesses %>%
    distinct(interesse) %>%
    pull()

  interesses <- interesses %>%
    group_by(id_leggo) %>%
    summarise(agendas = paste(interesse, collapse = ";")) %>%
    ungroup()

  proposicoes <- read_csv(proposicoes_datapath,
                          col_types = cols(id_ext = col_character())) %>%
    filter(status == "Ativa") %>%
    mutate(sigla = paste0(sigla_tipo, " ", numero, "/", year(ymd_hms(data_apresentacao)))) %>%
    mutate(casa_origem = if_else(is.na(casa_origem), "camara", casa_origem),
           casa_revisora = if_else(casa_origem == "camara", "senado", "camara")) %>%
    select(id_leggo, id_ext, casa, sigla, casa_origem, casa_revisora) %>%
    distinct()

  proposicoes_criterio_aprovada_em_uma_casa <-
    process_criterio_aprovada_em_uma_casa(proposicoes_datapath,
                                          progressos_datapath) %>%
    mutate(criterio_aprovada_em_uma_casa = T) %>%
    select(id_leggo, criterio_aprovada_em_uma_casa, fase_global, local, local_casa, data_inicio, data_fim)

  proposicoes_criterio_avancou_comissoes <-
    process_criterio_avancou_comissoes(proposicoes_datapath,
                                       tramitacoes_datapath) %>%
    mutate(criterio_avancou_comissoes = ccj_camara | parecer_aprovado_comissao) %>%
    filter(criterio_avancou_comissoes) %>%
    select(id_leggo, criterio_avancou_comissoes, ccj_camara, sigla_local, parecer_aprovado_comissao, comissoes_aprovadas)

 proposicoes_criterio_requerimento_urgencia <-
    process_criterio_requerimento_urgencia(tramitacoes_datapath,
                                      proposicoes_datapath) %>%
    mutate(criterio_req_urgencia_apresentado = !is.na(data_req_urgencia_apresentacao),
           criterio_req_urgencia_aprovado = !is.na(data_req_urgencia_aprovacao)) %>%
    select(id_leggo,
           criterio_req_urgencia_apresentado,
           criterio_req_urgencia_aprovado,
           data_req_urgencia_apresentacao,
           data_req_urgencia_aprovacao,
           casa_req_urgencia_apresentacao,
           casa_req_urgencia_aprovacao)

  proposicoes_pressao_alta <-
    process_criterio_pressao_alta(pressao_datapath) %>%
    mutate(criterio_pressao_alta = T) %>%
    select(id_leggo, criterio_pressao_alta, maximo_pressao_periodo)

  proposicoes_criterio_mais_comentadas_twitter <-
    fetch_proposicoes_mais_comentadas_twitter(interesses = interesses_agendas) %>%
    mutate(criterio_mais_comentadas_twitter = T) %>%
    distinct()

  proposicoes_destaques <- proposicoes %>%
    left_join(proposicoes_criterio_aprovada_em_uma_casa,
               by = c("id_leggo")) %>%
    left_join(proposicoes_criterio_avancou_comissoes,
              by = c("id_leggo")) %>%
    left_join(proposicoes_pressao_alta,
              by = c("id_leggo")) %>%
    left_join(proposicoes_criterio_mais_comentadas_twitter,
              by = c("id_leggo")) %>%
    left_join(proposicoes_criterio_requerimento_urgencia,
              by = c("id_leggo")) %>%
    mutate(criterio_aprovada_em_uma_casa = !is.na(criterio_aprovada_em_uma_casa),
           criterio_avancou_comissoes = !is.na(criterio_avancou_comissoes),
           criterio_pressao_alta = !is.na(criterio_pressao_alta),
           criterio_req_urgencia_apresentado = if_else(is.na(criterio_req_urgencia_apresentado), FALSE, criterio_req_urgencia_apresentado),
           criterio_req_urgencia_aprovado = if_else(is.na(criterio_req_urgencia_aprovado), FALSE, criterio_req_urgencia_aprovado),
           criterio_mais_comentadas_twitter = !is.na(criterio_mais_comentadas_twitter)) %>%
    left_join(interesses, by = c("id_leggo")) %>%
    distinct(id_leggo, .keep_all = TRUE) %>%
    select(-c(id_ext, casa, sigla))

  return(proposicoes_destaques)

}

#' @title Define critérios de destaque para proposições e prepara o dataframe removendo atributos desnecessários
#' @description A partir de vários critérios das proposições, define atributos que as tornam destaque
#' @param proposicoes_datapath Caminho para o arquivo de proposições
#' @param progressos_datapath Caminho para o arquivo de progressos das proposições
#' @param tramitacoes_datapath Caminho para o arquivo de tramitações das proposições
#' @param interesses_datapath Caminho para o arquivo de interesses
#' @param pressao_datapath: Caminho para o arquivo de pressão das proposições
#' @return Um só dataframe contendo todas as proposições com seus atributos mínimos de destaque.
process_proposicoes_destaques_limpo = function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  progressos_datapath = here::here("leggo_data/progressos.csv"),
  tramitacoes_datapath = here::here("leggo_data/trams.csv"),
  interesses_datapath = here::here("leggo_data/interesses.csv"),
  pressao_datapath = here::here("leggo_data/pressao.csv")) {

  proposicoes_destaques = process_proposicoes_destaques(
    proposicoes_datapath,
    progressos_datapath,
    tramitacoes_datapath,
    interesses_datapath,
    pressao_datapath
  ) %>%
    mutate(casa_aprovacao = if_else(local_casa == "camara", "senado", "camara")) %>%
    select(id_leggo,
           casa_origem,
           casa_revisora,
           criterio_aprovada_em_uma_casa,
           casa_aprovacao,
           data_aprovacao = data_fim,
           criterio_avancou_comissoes,
           comissoes_camara = sigla_local,
           comissoes_senado = comissoes_aprovadas,
           criterio_req_urgencia_apresentado,
           casa_req_urgencia_apresentado = casa_req_urgencia_apresentacao,
           data_req_urgencia_apresentado = data_req_urgencia_apresentacao,
           criterio_req_urgencia_aprovado,
           casa_req_urgencia_aprovado = casa_req_urgencia_aprovacao,
           data_req_urgencia_aprovado = data_req_urgencia_aprovacao
           )

  return(proposicoes_destaques)
}
