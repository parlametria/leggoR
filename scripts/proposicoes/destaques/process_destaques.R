library(tidyverse)

process_proposicoes_destaques <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  progressos_datapath = here::here("leggo_data/progressos.csv")) {
  library(lubridate)
  
  source(here::here("scripts/proposicoes/destaques/process_criterio_aprovada_em_uma_casa.R"))
  
  proposicoes_destaques <- read_csv(proposicoes_datapath) %>%
    mutate(sigla = paste0(sigla_tipo, " ", numero, "/", year(ymd_hms(data_apresentacao)))) %>% 
    select(id_leggo, id_ext, casa, sigla) %>% 
    distinct()
  
  proposicoes_criterio_aprovada_em_uma_casa <-
    process_criterio_aprovada_em_uma_casa(proposicoes_datapath,
                                          progressos_datapath) %>% 
    mutate(criterio_aprovada_em_uma_casa = T) %>% 
    select(id_leggo, criterio_aprovada_em_uma_casa, fase_global, local, local_casa, data_inicio, data_fim)
  
  proposicoes_destaques <- proposicoes_destaques %>%
    left_join(proposicoes_criterio_aprovada_em_uma_casa,
               by = c("id_leggo")) %>% 
    mutate(criterio_aprovada_em_uma_casa = !is.na(criterio_aprovada_em_uma_casa))
  
  return(proposicoes_destaques)
  
}
