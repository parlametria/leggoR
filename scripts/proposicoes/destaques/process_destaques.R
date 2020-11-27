library(tidyverse)

process_proposicoes_destaques <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  progressos_datapath = here::here("leggo_data/progressos.csv"),
  tramitacoes_datapath = here::here("leggo_data/trams.csv"),
  interesses_datapath = here::here("leggo_data/interesses.csv")) {
  library(lubridate)

  source(here::here("scripts/proposicoes/destaques/process_criterio_aprovada_em_uma_casa.R"))
  source(here::here("scripts/proposicoes/destaques/process_criterio_parecer_aprovado_comissao.R"))
  source(here::here("scripts/proposicoes/destaques/fetcher_criterio_mais_comentadas_twitter.R"))

  interesses <- read_csv(interesses_datapath)
  
  interesses_agendas <- interesses %>% 
    distinct(interesse) %>% 
    pull()
    
  interesses <- interesses %>%
    group_by(id_leggo) %>%
    summarise(agendas = paste(interesse, collapse = ";")) %>%
    ungroup()

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
  
  proposicoes_criterio_mais_comentadas_twitter <- 
    fetch_proposicoes_mais_comentadas_twitter(interesses = interesses_agendas) %>% 
    mutate(num_tweets = as.numeric(num_tweets)) %>% 
    distinct()
  
  quartil_3 <- proposicoes_criterio_mais_comentadas_twitter %>% 
    pull(num_tweets) %>% 
    quantile() %>% 
    getElement("75%")
  
  a = proposicoes_criterio_mais_comentadas_twitter %>% 
    filter(num_tweets >= quartil_3)
    
    group_by(id_leggo) %>% 
    mutate(ranker = quantile(num_tweets))

  proposicoes_destaques <- proposicoes %>%
    left_join(proposicoes_criterio_aprovada_em_uma_casa,
               by = c("id_leggo")) %>%
    left_join(proposicoes_criterio_parecer_aprovado_comissao,
              by = c("id_leggo", "id_ext", "casa")) %>%
    mutate(criterio_aprovada_em_uma_casa = !is.na(criterio_aprovada_em_uma_casa),
           criterio_parecer_aprovado_comissao = !is.na(criterio_parecer_aprovado_comissao)) %>%
    left_join(interesses, by = c("id_leggo"))

  return(proposicoes_destaques)

}
