library(here)
library(tidyverse)
library(rcongresso)
source(here("code/senado-lib.R"))
source(here("code/camara-lib.R"))

extract_informations <- function(bill_id_camara, bill_id_senado, url) {
  nome_ementa_camara <- get_nome_ementa_Camara(bill_id_camara)
  nome_ementa_senado <- get_nome_ementa_Senado(bill_id_senado)
  
  tramitacao_camara <- read_csv(paste0("../data/camara/", "tramitacao_camara_",bill_id_camara,".csv"))
  tramitacao_senado <- read_csv(paste0("../data/Senado/", bill_id_senado, "-bill-passage-phases-senado.csv"))
  despacho_camara <- tail_descricao_despacho_Camara(tramitacao_camara)
  despacho_senado <- tail_descricao_despacho_Senado(tramitacao_senado)
  
  last_events_senado <- extract_n_last_events_Senado(tramitacao_senado, 3)
  last_events_camara <- extract_n_last_events_Camara(tramitacao_camara, 3)
  
  nome <- paste0(nome_ementa_senado$sigla_subtipo_materia, nome_ementa_senado$numero_materia, " / ", nome_ementa_camara$siglaTipo, nome_ementa_camara$numero)
  casa <- if_else(despacho_camara$data_hora > despacho_senado$data_tramitacao, "Câmara", "Senado")
  if(casa == "Câmara") {
    eventos <- as.list(last_events_camara$evento)
    despacho <- despacho_camara$descricao_tramitacao 
    relator <- extract_last_relator_Camara(tramitacao_camara)
  } else { 
    eventos <- as.list(last_events_senado$evento)
    despacho <- despacho_senado$situacao_descricao_situacao
    relator <- fetch_last_relatoria(id)$nome_parlamentar
  }
  
  proposicoes_df <- 
    frame_data(~ nome, ~ casa_atual, ~ ementa, ~ despacho_atual, ~ ultimos_eventos, ~ ultimo_relator,
               nome, casa, nome_ementa_camara$ementa, despacho, eventos, relator)
  proposicoes_df$nome <-paste0("[", proposicoes_df$nome, "](", url, ")")
  
  proposicoes_df
}

gera_tabela_proposicoes <- function(dataframe) {
  require(magrittr)
  
  propositions <- data.frame()
  
  propositions <- dataframe %>% 
    rowwise() %>%
    do(extract_informations(.$id_camara, .$id_senado, .$url)) %>%
    rbind(propositions,.)
  
  propositions %<>% mutate(ultimos_eventos = toString(ultimos_eventos))
  
  propositions
}

extract_informations_from_single_house <- function(id, casa) {
  casa <- tolower(casa)
  
  if (casa == 'camara') {
    nome_camara <- get_nome_ementa_Camara(id)
    tramitacao_camara = read_csv(paste0("../data/camara/", "tramitacao_camara_", id, ".csv"))
    despacho_camara <- tail_descricao_despacho_Camara(tramitacao_camara)
    nome <- paste0(nome_camara$siglaTipo, nome_camara$numero) 
    autor <- extract_autor_Camara(id)
    casa_origem <- autor$casa_origem
    nome_autor <- autor$autor.nome
    despacho <- despacho_camara$descricao_tramitacao 
    relator <- extract_last_relator_Camara(tramitacao_camara)
    ementa <- nome_camara$ementa
    data_apresentacao <- format(as.Date(fetch_proposicao(id)$dataApresentacao), "%d/%m/%Y")
    
  } else if (casa == 'senado') {
    tramitacao_senado <- read_csv(paste0("../data/Senado/", id, "-bill-passage-phases-senado.csv"))
    proposicao <- fetch_bill(id)
    despacho_senado <- tail_descricao_despacho_Senado(tramitacao_senado)
    nome_senado <- proposicao %>% select(ementa_materia, sigla_subtipo_materia, numero_materia) %>% unique
    nome <- paste0(nome_senado$sigla_subtipo_materia, nome_senado$numero_materia)
    casa_origem <- proposicao$nome_casa_origem
    nome_autor <- proposicao$nome_autor
    despacho <- despacho_senado$texto_tramitacao
    relatoria <- fetch_last_relatoria(id)
    relator <- relatoria$nome_parlamentar
    ementa <- proposicao$ementa_materia
    data_apresentacao <- format(as.Date(proposicao$data_apresentacao), "%d/%m/%Y")
  }
  
  proposicoes_df <- 
    frame_data(~ nome, ~autor, ~ casa_origem, ~ data_apresentacao, ~ ementa, ~ status_atual, ~ ultimo_relator,
               nome, nome_autor, casa_origem, data_apresentacao, ementa, despacho, relator)
  proposicoes_df
}

gera_tabela_apensadas <- function(bill_id_camara, bill_id_senado) {
  url_senado <- "https://www25.senado.leg.br/web/atividade/materias/-/materia/"
  url_camara <- "http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao="
  
  camara <- 
    fetch_apensadas(bill_id_camara) %>%
    mutate(casa = "Câmara", apensadas = paste0("[", apensadas, "](", paste0(url_camara, apensadas), ")"))
  
  senado <- 
    fetch_bill(bill_id_senado)
  if (!is.na(senado$proposicoes_apensadas)) {
    senado <- 
      senado  %>% 
      mutate(proposicoes_apensadas = strsplit(.$proposicoes_apensadas, " ")) %>%
      unnest() 
    
    senado <-
      senado %>%
      select(apensadas = proposicoes_apensadas) %>%
      mutate(casa = "Senado", apensadas = paste0("[", apensadas, "](", paste0(url_senado, apensadas), ")"))
    
    x <- rbind(camara, senado)
  }else {
    senado <-
      senado %>%
      select(apensadas = proposicoes_apensadas) %>%
      mutate(casa = "Senado", apensadas = paste0("[", apensadas, "](", paste0(url_senado, apensadas), ")"))
    
    x <- rbind(camara, senado[0,])
  }
  
}

