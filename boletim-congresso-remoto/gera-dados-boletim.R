## Leitura dos dados
library(tidyverse)
library(jsonlite)

proposicoes <- read_csv(here::here("boletim-congresso-remoto/interesses.csv"))
proposicoes_info <- read_csv(here::here("boletim-congresso-remoto/proposicoes.csv"))
atuacao <- read_csv(here::here("boletim-congresso-remoto/atuacao.csv"))

## Filtra proposições do congresso Remoto

proposicoes_congresso_remoto <- proposicoes %>% 
  select(id_leggo, nome_interesse, tema) %>% 
  filter(nome_interesse == "Congresso Remoto") %>% 
  left_join(proposicoes_info %>% 
              select(id_ext, id_leggo, sigla_tipo, numero, ementa, data_apresentacao, casa),
            by = c("id_leggo"))
write_csv(proposicoes_congresso_remoto, here::here("boletim-congresso-remoto/data/proposicoes_congresso_remoto.csv"))

atuacao_congresso_remoto <- atuacao %>% 
  filter(id_ext %in% (proposicoes_congresso_remoto %>%
                        pull(id_ext))) %>% 
  select(id_leggo, id_autor_parlametria, id_ext, casa, id_autor, tipo_autor, casa_autor, tipo_generico,
         sigla_local, peso_total_documentos, num_documentos)
write_csv(atuacao_congresso_remoto, here::here("boletim-congresso-remoto/data/atuacao_congresso_remoto.csv"))

atuacao_summary <- atuacao_congresso_remoto %>% 
  group_by(id_leggo, id_ext, casa, id_autor_parlametria, id_autor) %>% 
  summarise(peso_total = sum(peso_total_documentos),
            n_documentos = sum(num_documentos)) %>% 
  ungroup() %>% 
  mutate(peso_total_normalizado = (peso_total - min(peso_total)) / (max(peso_total) - min(peso_total)))
write_csv(atuacao_summary, here::here("boletim-congresso-remoto/data/atuacao_summary.csv"))


proposicoes_temas <- proposicoes_congresso_remoto %>% 
  separate_rows(tema, sep = ";") %>% 
  distinct(id_leggo, tema)

atuacao_summary_temas <- atuacao_summary %>% 
  left_join(proposicoes_temas, by = c("id_leggo"))
write_csv(atuacao_summary_temas, here::here("boletim-congresso-remoto/data/atuacao_summary_temas.csv"))

## Peso político
url_peso_politico = "https://perfil.parlametria.org/api/perfil/lista"

peso_politico <- fromJSON(url_peso_politico) %>% 
  as.data.frame() %>% 
  select(id_autor_parlametria = idParlamentarVoz, peso_politico = pesoPolitico)
write_csv(peso_politico, here::here("boletim-congresso-remoto/data/peso_politico.csv"))


