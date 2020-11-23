library(tidyverse)

#' @title Processa o critério de aprovação em pelo menos uma casa
#' @description Realiza o processamento das proposições de destaque com base nos critérios:
#' -> Ainda está em tramitação (ou seja, não foi sancionada ou vetada ainda);
#' -> Iniciou a fase global de Revisão I ou Pré-Revisão I
#' @param proposicoes_datapath Datapath de proposições
#' @param progressos_datapath Datapath de progressos
#' @return Dataframe contendo as proposições que passam no critério, 
#' com o status do progresso atual de cada uma.
process_criterio_aprovada_em_uma_casa <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  progressos_datapath = here::here("leggo_data/progressos.csv")) {
  
  .fases_presidencia <-
    c("Promulgação/Veto",
      "Sanção/Veto",
      "Sanção Presidencial/Promulgação")
  
  .fases_revisao <- c("Revisão I", "Pré-Revisão I")
  
  progressos <- read_csv(progressos_datapath)
  
  proposicoes <- read_csv(proposicoes_datapath) %>%
    select(id_ext, casa, id_leggo, status)
  
  # Recupera as proposições que ainda não foram sancionadas ou vetadas
  proposicoes_sem_promulgacao <- progressos %>%
    filter(fase_global %in% .fases_presidencia, is.na(data_inicio)) %>%
    select(id_ext, casa) %>%
    distinct()
  
  # Recupera as proposições que já iniciaram a fase de Revisão
  progressos_em_revisao <- progressos %>%
    inner_join(proposicoes_sem_promulgacao, by = c("id_ext", "casa")) %>%
    filter(fase_global %in% .fases_revisao, !is.na(data_inicio)) %>%
    distinct(id_ext, casa)
  
  # Recupera o progresso mais atual das proposições filtradas
  progresso_atual_proposicoes_em_revisao <- progressos %>% 
    inner_join(progressos_em_revisao, by = c("id_ext", "casa")) %>% 
    filter(!is.na(data_inicio)) %>% 
    group_by(id_ext, casa) %>% 
    slice(which.max(as.Date(data_inicio, '%Y-%m-%d'))) %>% 
    ungroup()
  
  proposicoes_em_revisao <- progresso_atual_proposicoes_em_revisao %>% 
    inner_join(proposicoes, by = c("id_ext", "casa")) %>% 
    select(id_leggo, fase_global, local, local_casa, data_inicio, data_fim)
  
  return(proposicoes_em_revisao)
}
