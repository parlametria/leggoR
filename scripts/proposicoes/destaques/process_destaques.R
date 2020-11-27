library(tidyverse)

process_proposicoes_destaques <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  progressos_datapath = here::here("leggo_data/progressos.csv"),
  tramitacoes_datapath = here::here("leggo_data/trams.csv")) {
  library(lubridate)

  source(here::here("scripts/proposicoes/destaques/process_criterio_aprovada_em_uma_casa.R"))
  source(here::here("scripts/proposicoes/destaques/process_criterio_parecer_aprovado_comissao.R"))
  source(here::here("scripts/proposicoes/destaques/process_criterio_requerimento_urgencia.R"))

  proposicoes <- read_csv(proposicoes_datapath,
                          col_types = cols(id_ext = col_character())) %>%
    filter(status == "Ativa") %>%
    mutate(sigla = paste0(sigla_tipo, " ", numero, "/", year(ymd_hms(data_apresentacao)))) %>%
    select(id_leggo, id_ext, casa, sigla) %>%
    distinct()

  proposicoes_criterio_aprovada_em_uma_casa <-
    process_criterio_aprovada_em_uma_casa(proposicoes_datapath,
                                          progressos_datapath) %>%
    mutate(criterio_aprovada_em_uma_casa = T) %>%
    select(id_leggo, criterio_aprovada_em_uma_casa, fase_global, local, local_casa, data_inicio, data_fim)

  proposicoes_criterio_parecer_aprovado_comissao <-
    process_criterio_parecer_aprovado_comissao(proposicoes_datapath,
                                                 tramitacoes_datapath) %>%
    mutate(criterio_parecer_aprovado_comissao = T) %>%
    select(id_leggo, id_ext, casa, criterio_parecer_aprovado_comissao, comissoes_aprovadas)
  
  proposicoes_criterio_requerimento_urgencia <- 
    process_criterio_requerimento_urgencia(tramitacoes_datapath, 
                                      proposicoes_datapath) %>% 
    mutate(requerimento_urgencia_apresentado = ifelse(!is.na(requerimento_urgencia_apresentado), T , F), 
           requerimento_urgencia_aprovado = ifelse(!is.na(requerimento_urgencia_aprovado), T , F)) %>%
    select(id_leggo, id_ext, casa, requerimento_urgencia_apresentado, requerimento_urgencia_aprovado)

  proposicoes_destaques <- proposicoes %>%
    left_join(proposicoes_criterio_aprovada_em_uma_casa,
               by = c("id_leggo")) %>%
    left_join(proposicoes_criterio_parecer_aprovado_comissao,
              by = c("id_leggo", "id_ext", "casa")) %>%
    left_join(proposicoes_criterio_requerimento_urgencia,
              by = c("id_leggo", "id_ext", "casa")) %>% 
    mutate(criterio_aprovada_em_uma_casa = !is.na(criterio_aprovada_em_uma_casa),
           criterio_parecer_aprovado_comissao = !is.na(criterio_parecer_aprovado_comissao),
           requerimento_urgencia_apresentado = !is.na(requerimento_urgencia_apresentado, requerimento_urgencia_aprovado))

  return(proposicoes_destaques)

}
