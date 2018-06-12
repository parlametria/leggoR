library(tidyverse)
library(here)
library(jsonlite)
source(here("code/senado-lib.R"))

bill_id <- 91341

#Voting data
voting <- fetch_voting(bill_id)
voting %>% 
    write_csv(here(paste0("data/Senado/", bill_id, "-votacoes-senado.csv")))

#Passage Data
passage <- fetch_passage(bill_id)
passage %>%
    write_csv(here(paste0("data/Senado/", bill_id, "-passage-senado.csv")))

bill_data <- fetch_bill(bill_id)
bill_data %>%
  write_csv(here(paste0("data/Senado/", bill_id, "-bill-senado.csv")))

#Relatorias data
relatorias <- fetch_relatorias(bill_id)
relatorias %>%
  write_csv(here(paste0("data/Senado/", bill_id, "-relatorias-senado.csv")))

#Current Relatoria data
current_relatoria <- fetch_current_relatoria(bill_id)
current_relatoria %>%
  write_csv(here(paste0("data/Senado/", bill_id, "-current-relatoria-senado.csv")))

#Last Relatoria
last_relatoria <- fetch_relatoria_atual(bill_id)
last_relatoria %>%
  write_csv(here(paste0("data/Senado/", bill_id, "-last-relatoria-senado.csv")))
