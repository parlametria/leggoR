library(tidyverse)

process_proposicoes_destaques <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  progressos_datapath = here::here("leggo_data/progressos.csv")) {
  
  source(here::here("scripts/proposicoes/destaques/process_criterio_aprovada_em_uma_casa.R"))
  
  proposicoes_destaques <- read_csv(proposicoes_datapath) %>%
    select(id_leggo, id_ext, casa)
  
  proposicoes_criterio_aprovada_em_uma_casa <-
    process_criterio_aprovada_em_uma_casa(proposicoes_datapath,
                                          progressos_datapath)
  
  proposicoes_destaques <- proposicoes_destaques %>%
    inner_join(proposicoes_criterio_aprovada_em_uma_casa,
               by = c("id_leggo", "id_ext", "casa"))
  
  return(proposicoes_destaques)
  
}