library(tidyverse)

mapeia_ids_proposicoes <- function(url) {
  source(here::here("scripts/proposicoes/fetcher_proposicao.R"))
  
  colunas <- c("proposicao", 
               "id_camara",
               "id_senado",
               "apelido",	
               "tema",
               "advocacy_link",
               "keywords",
               "tipo_agenda",
               "explicacao_projeto")
  
  proposicoes <- read_csv(url) %>% 
    select(nome = Nome,
           casa = Casa, 
           link_casa = `Link Casa`)
  
  proposicoes_com_id <- proposicoes %>% 
    rowwise(.) %>% 
    mutate(id = if_else(!is.na(link_casa), 
                        extract_id_from_link(link_casa),
                        as.character(fetch_id_by_nome_formal(nome, casa))))
  
}