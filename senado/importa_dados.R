install.packages("congressbr")
library(congressbr)
library(dplyr)

#CongressBr
#bill_id <- 91341
#tramitacao_91341 <- sen_bills_passage(bill_id = bill_id) %>% filter(!is.na(bill_passage_text))


url_base_voting <- "http://legis.senado.leg.br/dadosabertos/materia/votacoes/"
url_base_passage <- "http://legis.senado.leg.br/dadosabertos/materia/movimentacoes/"
bill_id <- 91341

#Voting data
url <- paste(url_base_voting, bill_id, sep = "")
json_voting <- jsonlite::fromJSON(url, flatten = T)
list_bill_voting_data <- json_voting$VotacaoMateria[4]$Materia
df_bill_voting_identification <- list_bill_voting_data$IdentificacaoMateria %>% purrr::map_df(~ .) 
df_voting_general <- list_bill_voting_data$Votacoes %>% purrr::map_df(~ .)
df_voting_parliamentarians <- tidyr::unnest(df_voting_general)

#Passage Data
url <- paste(url_base_passage, bill_id, sep = "")
json_passage <- jsonlite::fromJSON(url, flatten = T)
list_bill_passage_data <- json_passage$MovimentacaoMateria[4]$Materia
df_bill_passage_identification <- list_bill_passage_data$IdentificacaoMateria %>% purrr::map_df(~ .)
df_bill_actual_situation <- list_bill_passage_data$SituacaoAtual$Autuacoes$Autuacao$Situacao %>% purrr::map_df(~ .)
df_bill_passages <- list_bill_passage_data$Tramitacoes %>% purrr::map_df(~ .)

