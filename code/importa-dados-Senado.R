args = commandArgs(trailingOnly=TRUE)
library(tidyverse)
library(here)
library(jsonlite)
source(here::here("code/senado-lib.R"))

#It receives a project id and creates a csv with the main information.
import_project <- function(bill_id){
  #Voting data
  voting <- fetch_voting(bill_id)
  voting %>% 
      write_csv(paste0(here("data/Senado/"), bill_id, "-votacoes-senado.csv"))

  #Passage Data
  passage <- fetch_passage(bill_id)
  passage %>%
      write_csv(paste0(here("data/Senado/"), bill_id, "-passage-senado.csv"))

  #Votacao Data
  bill_data <- fetch_bill(bill_id)
  bill_data %>%
    write_csv(paste0(here("data/Senado/"), bill_id , "-bill-senado.csv"))

  #Relatorias Data
  relatorias <- fetch_relatorias(bill_id)
  relatorias %>%
    write_csv(paste0(here("data/Senado/"), bill_id, "-relatorias-senado.csv"))

  #Relatorias data
  relatorias <- fetch_relatorias(bill_id)
  relatorias %>%
    write_csv(paste0(here("data/Senado/"), bill_id, "-relatorias-senado.csv"))
    
  #Current Relatoria data
  current_relatoria <- fetch_current_relatoria(bill_id)
  current_relatoria %>%
    write_csv(paste0(here("data/Senado/"), bill_id, "-current-relatoria-senado.csv"))
  
  #Last Relatoria
  last_relatoria <- fetch_last_relatoria(bill_id)
  last_relatoria %>%
    write_csv(paste0(here("data/Senado/"), bill_id, "-last-relatoria-senado.csv"))
}


if(length(args) == 1){
  import_project(args[1])
} 
       
