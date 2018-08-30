library(data.table)
library(tidyverse)
library(lubridate)
library(here)
library(agoradigital)
source(here::here("R/camara-lib.R"))

#' @title Adiciona o local para o vistime
#' @description Adiciona o label local com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' read_csv(paste0(here::here('data/camara/'), bill_id, '-tramitacao-camara.csv')) %>% data_local()
#' @export
data_local <- function(df) {
  df <-
    df %>%
    filter((!(grepl('^S', sigla_orgao) | sigla_orgao == 'MESA')))

  df %>%
    mutate(end_data = lead(data_hora, default=Sys.time())) %>%
    group_by(local, sequence = rleid(local)) %>%
    summarise(start = min(data_hora),
              end = max(end_data)) %>%
    filter(end - start > 0) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    rename(label = local) %>%
    mutate(group = "Comissão",
           color = case_when(label == "Plenário" ~ "#5496cf",
                             label == "PL672616" ~ "#ff9c37",
                             label == "CCP" ~ "#8bca42",
                             label == "CTASP" ~ "#ea81b1"))
}

#' @title Adiciona o evento para o vistime
#' @description Adiciona o label evento com suas respectivas cores no formato
#' suportado pelo vistime
#' @param df Dataframe com a tramitacao
#' @examples
#' read_csv(paste0(here::here('data/camara/'), bill_id, '-tramitacao-camara.csv')) %>% data_evento()
#' @export
data_evento <- function(df) {
  df %>%
    filter(!is.na(evento)) %>%
    mutate(start = data_hora, end = data_hora, group = 'Evento') %>%
    group_by(evento) %>%
    rename(label = evento) %>%
    select(label, start, end, group) %>%
    mutate(color = '#a9a9a9') %>%
    unique()
}

#' @title Retorna as comissões onde a proposição ainda vai passar na Camara 
#' @description Analisa as comissões previstas e as quais as comissões já passou e 
#' retorna as que ela ainda vai passar
#' @param tramitacao Dataframe com a tramitacao
#' @examples
#' get_comissoes_futuras(tramitacao)
#' @export
get_comissoes_futuras <- function(tramitacao) {
  
  siglas_comissoes <-
    get_comissoes_camara() %>% 
    dplyr::select(siglas_comissoes, comissoes_permanentes) %>% 
    tidyr::unnest()
  
  comissao_especial <- 
    get_comissoes_in_camara(tramitacao) %>%
    dplyr::filter(proximas_comissoes == "Comissão Especial")
  
  if (nrow(comissao_especial) == 0) { 
    comissoes_previstas <-
      get_comissoes_in_camara(tramitacao) %>%
      utils::head(1) %>% 
      dplyr::select(proximas_comissoes) %>%
      tidyr::unnest() %>%
      dplyr::rename("comissoes_permanentes" = "proximas_comissoes")
    
    comissoes_previstas_siglas <- 
      fuzzyjoin::regex_left_join(comissoes_previstas, siglas_comissoes) %>%
      dplyr::select(siglas_comissoes) %>%
      dplyr::rename("local" = "siglas_comissoes") %>%
      dplyr::filter(!is.na(local))
    
    comissoes_faltantes <-
      dplyr::anti_join(comissoes_previstas_siglas, tramitacao)
    
    if (nrow(comissoes_faltantes) != 0) {
      futuro_comissoes <-
        tramitacao %>%
        utils::tail(nrow(comissoes_faltantes)) %>%
        dplyr::mutate(data_hora = as.POSIXct(Sys.Date() + 200))
      
      futuro_comissoes$local <-
        comissoes_faltantes$local
      
      return(rbind(tramitacao, futuro_comissoes))
    }
    
    return(tramitacao)
  }
  
  return(tramitacao)
}

#' @title Adiciona a fase global para o vistime
#' @description Adiciona a fase global com suas respectivas cores no formato
#' suportado pelo vistime
#' @param bill_id id da proposição
#' @param tramitacao Dataframe com a tramitacao
#' @examples
#' read_csv(paste0(here::here('data/camara/'), bill_id, '-tramitacao-camara.csv')) %>% data_fase_global(bill_id, .)
#' @export
data_fase_global <- function(bill_id, tramitacao) {
  data_prop <- extract_autor_in_camara(bill_id) %>% tail(1)
  tipo_casa <- if_else(data_prop$casa_origem == "Câmara dos Deputados", "Origem", "Revisão")

  tramitacao <- get_comissoes_futuras(tramitacao)
  
  tramitacao <- 
    tramitacao %>%
    mutate(end_data = lead(data_hora, default=Sys.time())) %>%
    group_by(local, sequence = rleid(local)) %>%
    summarise(start = min(data_hora),
              end = max(end_data)) %>%
    filter(end - start > 0) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    rename(label = local) %>%
    mutate(group = "Global",
           label = case_when(label == "CD-MESA-PLEN" ~ "Apresentação",
                             TRUE ~ label),
           color = case_when(stringr::str_detect(label, "Plenário") ~ "#5496cf",
                             stringr::str_detect(label, "Comissão Especial") ~ "#ff9c37",
                             stringr::str_detect(label, "CCP") ~ "#8bca42",
                             stringr::str_detect(label, "CTASP") ~ "#ea81b1"),
           label = paste(label, '-', tipo_casa, '(Câmara)'))
  
  if (tipo_casa == "Origem") {
    futuro_revisao <-
      tramitacao %>%
      tail(1) %>%
      mutate(label = "Comissões - Revisão (Senado)",
             start = as.POSIXct(Sys.Date() + 200),
             end = as.POSIXct(Sys.Date() + 201))
    
    tramitacao <- rbind(tramitacao, futuro_revisao)
  }
    
  tramitacao
}

data_situacao_comissao <- function(df) {
  df %>% 
  dplyr::select(data_hora, situacao_comissao) %>%
  dplyr::filter(!is.na(situacao_comissao)) %>%
  mutate(end_data = lead(data_hora, default=Sys.time())) %>%
  group_by(situacao_comissao, sequence = data.table::rleid(situacao_comissao)) %>%
  summarise(start = min(data_hora),
            end = max(end_data)) %>%
  filter(end - start > 0) %>%
  ungroup() %>%
  arrange(sequence) %>%
  select(-sequence) %>% 
  rename(label = situacao_comissao) %>% 
  mutate(group = "Situação na comissão",
         color = case_when(label == "Recebimento" ~ "#5496cf",
                           label == "Análise do relator" ~ "#ff9c37",
                           label == "Discussão e votação" ~ "#8bca42",
                           label == "Encaminhamento" ~ "#ea81b1"))
}

#' @title Formata tabela para o vistime
#' @description Formata a tabela final que será usado para fazer a visualização
#' usando o vistime
#' @param tram_camara_df dataframe da tramitação do PL na Câmara
#' @examples
#' build_vis_csv_camara(fetch_tramitacao_camara(2121442))
build_vis_csv_camara <- function(tram_camara_df, output_folder=NULL) {
  bill_id <- tram_camara_df[1, "id_prop"]
  file_path <- paste0(output_folder,'/vis/tramitacao/', bill_id, '-data-camara.csv')

  data <- 
    bind_rows(data_fase_global(bill_id, tram_camara_df), 
                     data_local(tram_camara_df),
                    #data_situacao_comissao(tram_camara_df), 
                    data_evento(tram_camara_df)) %>%
    filter(group != "Comissão")
  
  readr::write_csv(data, file_path)
}
