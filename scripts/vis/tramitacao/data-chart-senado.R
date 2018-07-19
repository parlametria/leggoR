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
    mutate(group = "Sub-fase Comissão")
  
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
      data_tramitacao = as.Date(data_tramitacao),
      evento = as.character(evento)
    )
  
  df <- 
    df %>%
    mutate(z = cumsum(evento != lag(evento, default='NULL')),
           end_data = lead(data_tramitacao)) %>%
    group_by(evento, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = start,
              time_interval = end - start) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    filter(!is.na(evento)) %>%
    rename(label=evento) %>%
    mutate(group = "Evento", color = "#a9a9a9")
  
}

#Create data to display global phase inlines
create_fase_global <- function(data_tramitacao, bill_id) {
  data_prop <- read_csv(paste0(here::here("data/Senado/"), bill_id,"-proposicao-senado.csv"))
  casa_origem <- if_else(data_prop$nome_casa_origem == "Senado Federal", " - Casa de Origem (Senado)", " - Casa Revisora (Câmara)")
  
  virada_de_casa <- 
    data_tramitacao %>%
    filter(fase == "Virada de Casa") %>%
    arrange(data_tramitacao) %>%
    select(data_tramitacao)
  
  if(nrow(virada_de_casa) == 0) {
    data_tramitacao %>%
      mutate(casa = paste0(casa, casa_origem))
  }else {
    casa_atual <- if_else(casa_origem == " - Casa de Origem (Senado)", " - Casa Revisora (Câmara)", " - Casa de Origem (Senado)")
    data_tramitacao %>%
      mutate(casa = if_else(data_tramitacao < virada_de_casa[1, ][[1]], paste0(casa, casa_origem), paste0(casa, casa_atual)))
  }
}

#Create data to display casa inlines
format_fase_global <- function(df) {
  df <-
    # Improve the phases names and convert data_tramitacao to Date
    df %>%
    mutate(
      data_tramitacao = as.Date(data_tramitacao)
    )
  
  df %>%
    mutate(z = cumsum(casa != lag(casa, default='NULL')),
           end_data = lead(data_tramitacao)) %>%
    group_by(casa, sequence = data.table::rleid(z)) %>%
    summarize(start = min(data_tramitacao),
              end = if_else(is.na(max(end_data)),max(data_tramitacao),max(end_data)),
              time_interval = end - start) %>%
    ungroup() %>%
    arrange(sequence) %>%
    select(-sequence) %>%
    filter(!is.na(casa)) %>%
    rename(label=casa) %>%
    mutate(group = "Global",
           color = case_when(stringr::str_detect(label, "Plenário") ~ "#5496cf",
                             stringr::str_detect(label, "Comissões") ~ "#938ecc",
                             stringr::str_detect(label, "Apresentação") ~ "#d6952a"))
  
}


build_vis_csv <- function(bill_id) {
  data_tramitacao <- 
    read_csv(paste0(here::here("data/Senado/"), bill_id,"-visualizacao-tramitacao-senado.csv")) %>%
    create_fase_global(bill_id)
  df <- 
    rbind(format_fase_global(data_tramitacao),
              format_local(data_tramitacao), 
              format_fase(data_tramitacao),
              format_eventos(data_tramitacao))  %>%
    write_csv(paste0(here::here("data/vis/tramitacao/"), bill_id, "-data-senado.csv"))
}
