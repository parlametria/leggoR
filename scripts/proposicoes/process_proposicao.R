library(tidyverse)


#' @title Filtra as proposições pelo texto da coluna Urgência
#' @description Recebe um dataframe e o regime de tramitação de interesse e
#' retorna as linhas que possuem este regime.
#' @param df Dataframe a ser filtrado
#' @return Dataframe filtrado pelo regime de tramitação
.filter_proposicoes_by_texto_urgencia <- function(df) {
  urgencia_texto_regex <- "(urgência (– (despacho|requerimento)|urgentíssima)|transformad.* lei.* 14006/2020|requerimento pendente de aprovação)"
  
  return(
    df %>%
      filter(str_detect(tolower(urgencia), urgencia_texto_regex))
  )
}

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

#' @title Recupera os 3 temas mais relevantes de uma proposição
#' @description Recebe um id e casa e retorna os 3 temas mais relevantes de
#' uma proposição separados por ';'.
#' @param id id da proposição
#' @param casa camara ou senado
#' @return Temas da proposição separados por ';'.
.get_temas <- function(id, casa) {
  
  print(paste0("Extraindo temas da proposição ", id, " na casa ", casa, "..."))
  
  df <- rcongresso::fetch_temas_proposicao(id, casa)
  
  if (nrow(df) > 0) {
    temas <- df %>% arrange(desc(relevancia)) %>%
      head(3) %>%
      pull(tema)
    
    temas <- paste(temas, collapse = ";")
    
    return(temas)
  } else {
    return(NA)
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
    rowwise(.) %>% 
    mutate(tema = .get_temas(id, casa)) %>% 
    select(-c(id, casa))
  
  return(new_df)
  
}

#' @title Mapeia ids das proposições a partir do nome formal 
#' ou link da proposição
#' @description Recebe uma url para um csv e retorna esse mesmo csv 
#' adicionada uma coluna id correspondente
#' @param url URL do dataframe de proposições. Deve conter as colunas
#' nome, casa, link_casa
#' @param filter_by_regime_tramitacao Flag indicando se as proposições devem
#' ser filtradas pelo regime de tramitação ou não.
#' @return Mesmo csv de entrada contendo nova coluna id da proposição.
.mapeia_ids_proposicoes <- function(url, filter_by_regime_tramitacao = T) {
  source(here::here("scripts/proposicoes/fetcher_proposicao.R"))
  
  proposicoes <- read_csv(url) %>%
    agoradigital::rename_table_to_underscore()
  
  if (filter_by_regime_tramitacao) {
    proposicoes <- proposicoes %>%
      select(nome,
             casa,
             link_casa = `link casa`,
             regime_tramitacao = `regime de tramitação`,
             ementa) %>% 
      .filter_proposicoes_by_regime_tramitacao("URGENTE")
  } else {
    proposicoes <- proposicoes %>%
      select(nome,
             casa,
             link_casa = `link casa`,
             urgencia = `urgência`,
             ementa) %>% 
      .filter_proposicoes_by_texto_urgencia()
  }
  
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
#' @param filter_by_regime_tramitacao Flag indicando se as proposições devem
#' ser filtradas pelo regime de tramitação ou não.
#' @return Um só dataframe contendo todas as proposições, novas e existentes.
processa_planilha_proposicoes <- function(raw_proposicao_url, 
                                          processed_proposicao_url,
                                          filter_by_regime_tramitacao = T) {
  old_proposicoes <-
    read_csv(processed_proposicao_url, col_types = cols(.default = "c"))
  
  new_proposicoes <-
    .mapeia_ids_proposicoes(raw_proposicao_url, filter_by_regime_tramitacao) %>%
    .formata_dataframe_proposicoes()
  
  merged_proposicoes <- bind_rows(old_proposicoes, new_proposicoes) %>% 
    distinct()
  
  return(merged_proposicoes)
}
