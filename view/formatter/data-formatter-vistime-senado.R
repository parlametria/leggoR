library(tidyverse)
library(here)

# Create data frame to display local inline
format_local <- function(df) {
  local_df <-
    df %>%
    mutate(z = cumsum(local != lag(local, default='NULL')), 
           end_data = lead(data_hora)) %>%
    group_by(local, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_hora),
              end = if_else(is.na(max(end_data)),max(data_hora),max(end_data)),
              time_interval = end - start) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    filter(time_interval > 0) %>%
    rename(label=local) %>%
    mutate(group = "Comissão")

  local_df %>%
    mutate(color = case_when((grepl('^S', label) | 
                               label == 'ATA-PLEN') ~ "#e2e3d9",
                               label == 'PLEN' ~ "#5496cf",
                               label == 'CCJ' ~ "#ffffcc",
                               label == 'CAE' ~ "#a1dab4",
                               label == 'CDR' ~ "#225ea8"))
}

# Create data frame to display phases inline
format_fase <- function(df) {
  df <- 
    # Improve the phases names and convert data_tramitacao to Date
    df %>%
    mutate(
      data_hora = as.Date(data_hora),
      fase = as.character(fase)
    )
  
  df <- df %>%
    mutate(z = cumsum(fase != lag(fase, default='NULL')), 
           end_data = lead(data_hora)) %>%
    group_by(fase, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_hora),
              end = if_else(is.na(max(end_data)),max(data_hora),max(end_data)),
              time_interval = end - start) %>%
    ungroup() %>% 
    arrange(sequence) %>% 
    select(-sequence) %>%
    #filter(time_interval > 0) %>%
    rename(label=fase) %>%
    mutate(group = "Situação na comissão")
  
  df %>% 
    mutate(color = case_when(label == "Recebimento" ~ "#d7191c",
                           label == "Análise do relator" ~ "#fdae61",
                           label == "Discussão e votação" ~ "#ffffbf",
                           label == "Encaminhamento" ~ "#abd9e9"))
}

#Create data to display events inlines
format_eventos <- function(df) {
  df <-
    # Improve the phases names and convert data_hora to Date
    df %>%
    mutate(
      data_hora = if_else(is.na(data_audiencia), as.Date(data_hora), as.Date(data_audiencia)),
      evento = as.character(evento)
    )
  
  df %>%
    filter(!is.na(evento)) %>%
    mutate(start = data_hora,
           end = start,
           time_interval = end - start) %>%
    rename(label=evento) %>%
    unique() %>%
    mutate(group = "Evento", color = "#a9a9a9") %>%
    select(label, start, end, time_interval, group, color)
  
}

#Create data to display casa inlines
format_fase_global <- function(df) {
  
  df %>%
    mutate(global = if_else(casa == "Mesa - Senado", paste0("Apresentação ", global), paste0(local, " ", global))) %>%
    mutate(z = cumsum(global != lag(global, default='NULL')),
           end_data = lead(data_hora)) %>%
    group_by(global, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_hora),
              end = if_else(is.na(max(end_data)),max(data_hora),max(end_data)),
              time_interval = end - start) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    filter(!is.na(global)) %>%
    rename(label=global) %>%
    mutate(group = "Global",
           color = case_when(stringr::str_detect(label, "Plenário") ~ "#5496cf",
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
#' build_vis_csv_senado(fetch_tramitacao_senado(91341))
build_vis_csv_senado <- function(tram_senado_df, output_folder=NULL) {
  bill_id <- tram_senado_df[1, "prop_id"]
  tram_senado_filtered <- tram_senado_df %>%
    dplyr::select(data_hora, local, evento, casa, global, data_audiencia)
  
  vis_df <- 
    rbind(format_local(tram_senado_filtered),
          format_fase_global(tram_senado_filtered),
          #format_fase(tram_senado_filtered),
          format_eventos(tram_senado_filtered)) %>%
   # filter(time_interval != 0 | group == "Evento") %>%
    filter(group != 'Comissão')
  
  vis_df %>%
    write_csv(paste0(output_folder,'/vis/tramitacao/',bill_id,"-data-senado.csv"))
}

