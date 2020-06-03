library(tidyverse)

#' @title Filtra as proposições pelo regime de tramitação
#' @description Recebe um dataframe e o regime de tramitação de interesse e
#' retorna as linhas que possuem este regime.
#' @param df Dataframe a ser filtrado
#' @param regime Nome do regime de tramitação de interesse
#' @return Dataframe filtrado pelo regime de tramitação
.filter_proposicoes_by_regime_tramitacao <- function(df, regime) {
  return(
    df %>%
      filter(regime_tramitacao == regime)
  )
}

#' @title Mapeia o id da proposição
#' @description Se o link existir, o id será extraído dele. Caso contrário,
#' será feita uma requisição a partir do nome e casa.
#' @param link_casa Link para a proposição
#' @param nome nome formal da proposição
#' @param casa casa da proposiçãõ
#' @return Id da proposição
.fetch_id <- function(link_casa, nome, casa) {
  if (!is.na(link_casa)) {
    return(extract_id_from_link(link_casa))
  } else {
    return(as.character(fetch_id_by_nome_formal(nome, casa)))
  }
}

#' @title Processa dataframe intermediário de proposições em 
#' formato esperado pelo Leggo
#' @description Recebe um dataframe de proposições processados pela função
#' mapeia_ids_proposicoes() e processa-o no formato aceito pelo leggo
#' @param proposicoes Dataframe contendo colunas id, casa, nome e ementa
#' @return Dataframe de proposições formatados.
.formata_dataframe_proposicoes <- function(proposicoes) {
  new_df <- tribble(~ proposicao,
                    ~ id_camara,
                    ~ id_senado,
                    ~ apelido,
                    ~ tema,
                    ~ advocacy_link,
                    ~ keywords,
                    ~ tipo_agenda,
                    ~ explicacao_projeto)
  
  proposicoes <- proposicoes %>% 
    select(id, proposicao = nome, casa,
           explicacao_projeto = ementa)
  
  new_df <- new_df %>% 
    bind_rows(proposicoes) %>% 
    mutate(id_camara = if_else(casa == "camara", id, as.character(NA)),
           id_senado = if_else(casa == "senado", id, as.character(NA))) %>% 
    select(-c(id, casa))
  
  return(new_df)
  
}

#' @title Mapeia ids das proposições a partir do nome formal 
#' ou link da proposição
#' @description Recebe uma url para um csv e retorna esse mesmo csv 
#' adicionada uma coluna id correspondente
#' @param url URL do dataframe de proposições. Deve conter as colunas
#' nome, casa, link_casa
#' @return Mesmo csv de entrada contendo nova coluna id da proposição.
.mapeia_ids_proposicoes <- function(url) {
  source(here::here("scripts/proposicoes/fetcher_proposicao.R"))
  
  proposicoes <- read_csv(url) %>%
    agoradigital::rename_table_to_underscore() %>%
    select(nome,
           casa,
           link_casa = `link casa`,
           regime_tramitacao = `regime de tramitação`,
           ementa) %>% 
    .filter_proposicoes_by_regime_tramitacao("URGENTE")
  
  
  proposicoes_com_id <- proposicoes %>% 
    rowwise(.) %>% 
    mutate(id = .fetch_id(link_casa, nome, casa)) %>% 
    mutate(casa = .process_casa(casa))
  
  return(proposicoes_com_id)
  
}

#' @title Une duas planilhas a partir de suas urls
#' @description Recebe a url das proposições novas a serem adicionadas, executa o 
#' processamento e as unem às proposições já existentes a partir de sua url.
#' @param raw_proposicao_url URL do csv das proposições a serem processadas e 
#' adicionadas
#' @param processed_proposicao_url URL do csv das proposições já existentes.
#' @return Um só dataframe contendo todas as proposições, novas e existentes.
processa_planilha_proposicoes <- function(raw_proposicao_url, processed_proposicao_url) {
  old_proposicoes <-
    read_csv(processed_proposicao_url, col_types = cols(.default = "c"))
  
  new_proposicoes <-
    .mapeia_ids_proposicoes(raw_proposicao_url) %>%
    .formata_dataframe_proposicoes()
  
  merged_proposicoes <- bind_rows(old_proposicoes, new_proposicoes)
  
  return(merged_proposicoes)
}
