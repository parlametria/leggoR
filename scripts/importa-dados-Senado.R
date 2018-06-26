library(tidyverse)
library(here)
source(here::here("R/senado-lib.R"))

#' @title Importa as informações de uma proposição da internet.
#' @description Recebido um id a função roda os scripts para 
#' importar os dados daquela proposição.
#' @param bill_id Identificador da proposição que pode ser recuperado no site da casa legislativa.
#' @examples
#' import_proposicao(91341)
#' @export
import_proposicao <- function(bill_id){
  #Voting data
  voting <- fetch_votacoes(bill_id)
  voting %>% 
      write_csv(paste0(here::here("data/Senado/"), bill_id, "-votacoes-senado.csv"))

  #Passage Data
  passage <- fetch_tramitacao(bill_id)
  passage %>%
      write_csv(paste0(here::here("data/Senado/"), bill_id, "-passage-senado.csv"))

  #Votacao Data
  bill_data <- fetch_proposicao(bill_id)
  bill_data %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-bill-senado.csv"))

  #Relatorias Data
  relatorias <- fetch_relatorias(bill_id)
  relatorias %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-relatorias-senado.csv"))

  #Relatorias data
  relatorias <- fetch_relatorias(bill_id)
  relatorias %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-relatorias-senado.csv"))
    
  #Current Relatoria data
  current_relatoria <- fetch_current_relatoria(bill_id)
  current_relatoria %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-current-relatoria-senado.csv"))
  
  #Last Relatoria
  last_relatoria <- fetch_last_relatoria(bill_id)
  last_relatoria %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-last-relatoria-senado.csv"))
}
