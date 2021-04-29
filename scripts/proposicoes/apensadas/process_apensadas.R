library(tidyverse)
library(futile.logger)
library(agoradigital)

#' @title Processa proposições apensadas
#' @description Realiza o processamento das proposições apensadas para o formato usado pelo Parlametria
#' @param proposicoes_filepath Caminho para o CSV de proposições
#' @param interesses_filepath Caminho para o CSV de interesses
#' @param export_folder Folder para exportar lista da árvore de apensadas
#' @return Lista com dois dataframes:
#' Dataframe com mapeamento entre  as proposições apensadas e as proposições principais
#' Dataframe com a lista de proposições apensadas que não são monitoradas pelo Parlametria
process_apensadas <- function(proposicoes_filepath, interesses_filepath, export_folder) {

  proposicoes <- read_csv(proposicoes_filepath, col_types = cols(id_ext = "c"))

  interesses <- read_csv(interesses_filepath) %>%
    distinct(id_leggo, interesse)

  proposicoes_apensadas <- proposicoes %>%
    select(id_ext, casa, id_leggo, uri_prop_principal) %>%
    .extract_id_from_uri()

  process_lista_apensadas(proposicoes_apensadas, fresh_execution = FALSE, export_folder)

  proposicoes_apensadas <- proposicoes %>%
    filter(!is.na(uri_prop_principal)) %>%
    select(id_ext, casa, id_leggo, uri_prop_principal) %>%
    .extract_id_from_uri() %>%
    get_proposicao_principal_raiz()

  flog.info(str_glue("{proposicoes_apensadas %>% nrow()} proposições monitoradas são proposições apensadas."))

  props_apensadas <- proposicoes_apensadas %>%
    left_join(proposicoes %>% select(id_ext, casa, id_leggo_principal = id_leggo),
              by = c("id_prop_principal" = "id_ext", "casa")) %>%
    left_join(interesses, by = "id_leggo") %>%
    left_join(interesses %>% select(id_leggo_principal = id_leggo, interesse_prop_principal = interesse),
              by = "id_leggo_principal") %>%
    mutate(id_leggo_principal = if_else(interesse == interesse_prop_principal, id_leggo_principal, NA_character_))

  flog.info(str_glue("{props_apensadas %>% filter(!is.na(id_leggo_principal)) %>% distinct(id_ext, casa) %>% nrow()}",
                     " proposições monitoradas têm sua proposição principal também monitorada."))

  flog.info(str_glue("{props_apensadas %>% filter(is.na(id_leggo_principal), !is.na(interesse_prop_principal)) %>%
                     distinct(id_ext, casa) %>% nrow()}",
                     " proposições monitoradas têm sua proposição principal monitorada por outro interesse."))

  props_apensadas_alt <- props_apensadas %>%
    arrange(id_leggo, id_leggo_principal) %>%
    distinct(id_leggo, .keep_all = T) %>%
    select(id_leggo, id_leggo_prop_principal = id_leggo_principal, id_ext_prop_principal = id_prop_principal,
           casa_prop_principal = casa, interesse)

  props_apensadas_nao_monitoradas <- props_apensadas_alt %>%
    filter(is.na(id_leggo_prop_principal)) %>%
    left_join(proposicoes %>% distinct(id_ext, casa, id_leggo), by = c("id_leggo", "casa_prop_principal" = "casa")) %>%
    select(id_leggo, id_ext, casa = casa_prop_principal, id_leggo_prop_principal, id_ext_prop_principal)

  flog.info(str_glue("{props_apensadas_nao_monitoradas %>% nrow()} proposições monitoradas não têm a proposição principal monitorada"))

  return(list(props_apensadas_alt, props_apensadas_nao_monitoradas))
}

#' @title Extrai id da URI da proposição na API da respectiva casa
#' @param proposicoes Dataframe de proposições com pelo menos uma coluna: uri_prop_principal
#' @return Dataframe com as mesmas colunas originais e uma coluna a mais com o id da proposição
.extract_id_from_uri <- function(proposicoes) {
  proposicoes_com_id <- proposicoes %>%
    mutate(
      id_prop_principal = case_when(
        str_detect(uri_prop_principal, "dadosabertos.camara.leg.br") ~
          uri_prop_principal %>% str_extract("proposicoes/[0-9]+") %>% str_extract("[0-9]+"),
        str_detect(uri_prop_principal, "legis.senado.leg.br") ~
          uri_prop_principal %>% str_extract("materia/[0-9]+") %>% str_extract("[0-9]+")
      )
    )

  return(proposicoes_com_id)
}

#' @title Recupera o id da proposição que é a raiz da árvore de apensados da proposição de interesse
#' @param proposicoes Dataframe de proposições com pelo menos uma coluna: id_prop_principal
#' @return Dataframe com as mesmas colunas originais e uma coluna a mais com o id da proposição raiz (id_prop_principal_raiz)
get_proposicao_principal_raiz <- function(proposicoes) {
  proposicoes_processadas <- proposicoes %>%
    mutate(id_prop_principal_raiz = id_prop_principal)

  return(proposicoes_processadas)
}
