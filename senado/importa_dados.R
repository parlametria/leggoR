install.packages("congressbr")
library(congressbr)
library(dplyr)

bill_id <- 129808
url_base <- "http://legis.senado.leg.br/dadosabertos/materia/votacoes/"

tramitacao <- sen_bills_passage(bill_id = bill_id)
tramitacao <- tramitacao %>% filter(!is.na(bill_passage_text))

url <- paste(url_base, bill_id, sep = "")
j <- jsonlite::fromJSON(url, flatten = T)
df_identificacao_materia <- j$VotacaoMateria[4]$Materia$IdentificacaoMateria %>% purrr::map_df(~ .) 
df_votacoes_geral <- j$VotacaoMateria[4]$Materia$Votacoes %>% purrr::map_df(~ .)
df_votacoes_parlamentares <- tidyr::unnest(df_votacoes_geral)
