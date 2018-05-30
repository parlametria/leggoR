library(tidyverse)
library(here)
library(jsonlite)
source(here("code/senado-lib.R"))

to_underscore <- function(x) {
  x2 <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x3 <- gsub(".", "_", x2, fixed = TRUE)
  x4 <- gsub("([a-z])([A-Z])", "\\1_\\2", x3)
  x5 <- tolower(x4)
  x5
}

bill_id <- 91341

#Voting data
voting <- fetch_voting(bill_id)
new_names = names(voting) %>% 
  to_underscore() %>% 
  str_replace("sessao_plenaria_|tramitacao_identificacao_tramitacao_|identificacao_parlamentar_", "")

names(voting) <- new_names

voting %>% 
    write_csv(here(paste0("data/", bill_id, "-votacoes-senado.csv")))

#Passage Data
passage <- fetch_passage(bill_id)
new_names = names(passage) %>% 
  to_underscore() %>% 
  str_replace("identificacao_tramitacao_|
                   identificacao_tramitacao_origem_tramitacao_local_|
                   identificacao_tramitacao_destino_tramitacao_local_|
                   identificacao_tramitacao_situacao_", "")

names(passage) <- new_names

passage %>%
    write_csv(here(paste0("data/", bill_id, "-passage-senado.csv")))
