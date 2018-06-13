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
    relator <- fetch_last_relatoria(bill_id_senado)$nome_parlamentar
  }
  
  proposicoes_df <- 
    frame_data(~ nome, ~ casa_atual, ~ ementa, ~ despacho_atual, ~ ultimos_eventos, ~ ultimo_relator,
               nome, casa, nome_ementa_camara$ementa, despacho, eventos, relator)
  proposicoes_df$nome <-paste0("[", proposicoes_df$nome, "](", url, ")")
  
  proposicoes_df
}

gera_tabela_proposicoes <- function(dataframe) {
  propositions <- data.frame()
  
  propositions <- dataframe %>% 
    rowwise() %>%
    do(extract_informations(.$id_camara, .$id_senado, .$url)) %>%
    rbind(propositions,.)
  
  propositions
}
