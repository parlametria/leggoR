library(tidyverse)
library(here)

# Create data frame to display local inline
format_local <- function(df) {
  df <-
    df %>%
    mutate(z = cumsum(local != lag(local, default='NULL')), 
           end_data = lead(data_tramitacao)) %>%
    group_by(local, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = if_else(is.na(max(end_data)),max(data_tramitacao),max(end_data)),
              time_interval = end - start) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    filter(time_interval > 0) %>%
    rename(label=local) %>%
    mutate(group = "Comissão")

  df %>%
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
      data_tramitacao = as.Date(data_tramitacao),
      fase = as.character(fase)
    )
  
  df <- df %>%
    mutate(z = cumsum(fase != lag(fase, default='NULL')), 
           end_data = lead(data_tramitacao)) %>%
    group_by(fase, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = if_else(is.na(max(end_data)),max(data_tramitacao),max(end_data)),
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
    # Improve the phases names and convert data_tramitacao to Date
    df %>%
    mutate(
      data_tramitacao = if_else(is.na(data_audiencia), as.Date(data_tramitacao), as.Date(data_audiencia)),
      evento = as.character(evento)
    )
  
  df %>%
    filter(!is.na(evento)) %>%
    mutate(start = data_tramitacao,
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
           end_data = lead(data_tramitacao)) %>%
    group_by(global, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = if_else(is.na(max(end_data)),max(data_tramitacao),max(end_data)),
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

build_vis_csv <- function(bill_id, events = TRUE) {
  data_tramitacao <- 
    read_csv(paste0(here::here("data/Senado/"), bill_id,"-visualizacao-tramitacao-senado.csv"))
  
  if (events) {
    df <- 
      rbind(format_local(data_tramitacao),
            format_fase_global(data_tramitacao),
            #format_fase(data_tramitacao),
            format_eventos(data_tramitacao)) %>%
      filter(time_interval != 0 | group == "Evento") %>%
      filter(group != 'Comissão')
  }else {
    df <- 
      rbind(format_local(data_tramitacao),
            format_fase_global(data_tramitacao)) %>%
      filter(group != 'Comissão')
  }

  df %>%
    write_csv(paste0(here::here("data/vis/tramitacao/"), bill_id, "-data-senado.csv"))
}
