library(tidyverse)
library(here)
library(jsonlite)
#install.packages("congressbr")
# library(congressbr)
source(here("code/senado-lib.R"))

#tramitacao_91341 <- sen_bills_passage(bill_id = bill_id) %>% filter(!is.na(bill_passage_text))


bill_id <- 91341

#Voting data
voting <- fetch_voting(bill_id)
voting %>% 
    write_csv(here(paste0("data/", bill_id, "-votacoes-senado.csv")))

#Passage Data
url_base_passage <- "http://legis.senado.leg.br/dadosabertos/materia/movimentacoes/"
url <- paste(url_base_passage, bill_id, sep = "")
json_passage <- jsonlite::fromJSON(url, flatten = T)
list_bill_passage_data <- json_passage$MovimentacaoMateria[4]$Materia
df_bill_passage_identification <- list_bill_passage_data$IdentificacaoMateria %>% purrr::map_df(~ .)
bill_type <- df_bill_passage_identification$SiglaSubtipoMateria
bill_number <- df_bill_passage_identification$NumeroMateria
df_bill_actual_situation <- list_bill_passage_data$SituacaoAtual$Autuacoes$Autuacao$Situacao %>% purrr::map_df(~ .)
df_bill_passages <- list_bill_passage_data$Tramitacoes %>% purrr::map_df(~ .)
df_bill_passages_csv <- df_bill_passages %>% 
                                              select(-c(IdentificacaoTramitacao.OrigemTramitacao.Local.NomeCasaLocal,
                                                        IdentificacaoTramitacao.OrigemTramitacao.Local.NomeLocal,
                                                        IdentificacaoTramitacao.DestinoTramitacao.Local.NomeCasaLocal,
                                                        IdentificacaoTramitacao.DestinoTramitacao.Local.NomeLocal,
                                                        IdentificacaoTramitacao.Situacao.SiglaSituacao,
                                                        Textos.Texto,
                                                        Publicacoes.Publicacao)) %>%
                                              mutate(CodigoMateria = bill_id, 
                                                     SiglaSubtipoMateria = bill_type,
                                                     NumeroMateria = bill_number)
write.csv(df_bill_passages_csv, paste(bill_id, "_passage.csv", sep = "_"))

