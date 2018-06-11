library(tidyverse)
library(here)
library(jsonlite)
source(here("code/senado-lib.R"))

bill_id <- 91341

#Voting data
voting <- fetch_voting(bill_id)
voting %>% 
    write_csv(here(paste0("data/", bill_id, "-votacoes-senado.csv")))

#Passage Data
passage <- fetch_passage(bill_id)
passage %>%
    write_csv(here(paste0("data/", bill_id, "-passage-senado.csv")))

bill_data <- fetch_bill(bill_id)
bill_data %>%
  write_csv(here(paste0("data/", bill_id, "-bill-senado.csv")))

#Relatorias Data
relatorias <- fetch_relatorias(bill_id)
relatorias %>%
  write_csv(here(paste0("data/", bill_id, "-relatorias-senado.csv")))

#Last Relatoria
last_relatoria <- fetch_relatoria_atual(bill_id)
last_relatoria %>%
  write_csv(here(paste0("data/", bill_id, "last-relatoria-senado.csv")))