library(tidyverse)
library(futile.logger)
library(agoradigital)

source(here::here("scripts/proposicoes/apensadas/process_lista_apensadas.R"))

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
    agoradigital::extract_id_from_uri()

  listas <- process_lista_apensadas(proposicoes_apensadas, fresh_execution = FALSE, export_folder)

  proposicoes_apensadas_com_prop_raiz <- .find_proposicao_raiz(proposicoes_apensadas, listas)

  flog.info(str_glue("{proposicoes_apensadas_com_prop_raiz %>% nrow()} proposições monitoradas são proposições apensadas."))

  props_apensadas <- proposicoes_apensadas_com_prop_raiz %>%
    left_join(proposicoes %>% select(id_ext, casa, id_leggo_principal = id_leggo),
              by = c("id_prop_principal_raiz" = "id_ext", "casa")) %>%
    left_join(interesses, by = "id_leggo") %>%
    left_join(interesses %>% select(id_leggo_principal = id_leggo, interesse_prop_principal = interesse),
              by = "id_leggo_principal") %>%
    mutate(id_leggo_principal = if_else(interesse == interesse_prop_principal, id_leggo_principal, NA_character_))

  flog.info(str_glue("{props_apensadas %>% filter(!is.na(id_leggo_principal)) %>% distinct(id_ext, casa) %>% nrow()}",
                     " proposições monitoradas têm sua proposição principal raiz também monitorada."))

  flog.info(str_glue("{props_apensadas %>% filter(is.na(id_leggo_principal), !is.na(interesse_prop_principal)) %>%
                     distinct(id_ext, casa) %>% nrow()}",
                     " proposições monitoradas têm sua proposição principal monitorada por outro interesse."))

  props_apensadas_alt <- props_apensadas %>%
    arrange(id_leggo, id_leggo_principal) %>%
    distinct(id_leggo, .keep_all = T) %>%
    select(id_leggo, id_leggo_prop_principal = id_leggo_principal, id_ext_prop_principal = id_prop_principal,
           id_ext_prop_principal_raiz = id_prop_principal_raiz, casa_prop_principal = casa, interesse)

  props_apensadas_nao_monitoradas <- props_apensadas_alt %>%
    filter(is.na(id_leggo_prop_principal)) %>%
    left_join(proposicoes %>% distinct(id_ext, casa, id_leggo), by = c("id_leggo", "casa_prop_principal" = "casa")) %>%
    mutate(uri_prop_principal_raiz = case_when(
      casa_prop_principal == "camara" ~ str_glue(
        "https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao={id_ext_prop_principal_raiz}"),
      casa_prop_principal == "senado" ~ str_glue(
        "https://www25.senado.leg.br/web/atividade/materias/-/materia/{id_ext_prop_principal_raiz}"
      )
    )) %>%
    select(id_leggo, id_ext, casa = casa_prop_principal, id_leggo_prop_principal, id_ext_prop_principal,
           id_ext_prop_principal_raiz, uri_prop_principal_raiz, interesse)

  flog.info(str_glue("{props_apensadas_nao_monitoradas %>% nrow()} proposições monitoradas não têm a proposição principal raiz monitorada"))

  return(list(props_apensadas_alt, props_apensadas_nao_monitoradas))
}

#' @title Recupera os ids das proposições que são as raizes das árvores de apensados da proposição apensada monitorada
#' @param proposicoes_apensadas Dataframe de proposições com pelo menos três colunas: id_ext, casa, id_prop_principal
#' @param listas Lista resultado do retorno do processamento da lista da árvore de apensadas da função process_lista_apensadas
#' @return Dataframe com as mesmas colunas originais e uma coluna a mais com o id da proposição raiz (id_prop_principal_raiz)
.find_proposicao_raiz <- function(proposicoes_apensadas, listas) {
  apensadas_camara <- listas[[1]]
  apensadas_senado <- listas[[2]]

  proposicoes_filtradas <- proposicoes_apensadas %>%
    filter(!is.na(uri_prop_principal))

  proposicoes_processadas <- map2_df(proposicoes_filtradas$id_ext, proposicoes_filtradas$casa,
                                     ~ .get_proposicao_raiz(.x, .y, apensadas_camara, apensadas_senado)) %>%
    mutate(id_prop_principal_raiz = if_else(id_prop == id_prop_principal_raiz, NA_character_, id_prop_principal_raiz)) %>%
    distinct(id_prop, casa_origem, .keep_all = TRUE)

  proposicoes_merge <- proposicoes_filtradas %>%
    left_join(proposicoes_processadas, by = c("id_ext" = "id_prop", "casa" = "casa_origem"))

  return(proposicoes_merge)
}

#' @title Recupera o id da proposição que é raiz na árvore de apensados da proposição apensada monitorada
#' @param id_prop Id da proposição apensadas
#' @param casa_origem Casa de origem da proposição apensada
#' @param apensadas_camara Árvore de apensadas da Câmara
#' @param apensadas_senado Árvore de apensadas do Senado
#' @return Dataframe com o mapeamento entre a proposição (id_prop, casa_origem) e sua proposição principal raiz (id_prop_principal_raiz)
.get_proposicao_raiz <- function(id_prop, casa_origem, apensadas_camara, apensadas_senado) {

  if (casa_origem == "camara") {
    lista_apensadas <- apensadas_camara
  } else if (casa_origem == "senado") {
    lista_apensadas <- apensadas_senado
  } else {
    stop("Casa de origem inválida. Deve ser 'camara' ou 'senado'")
  }

  id_prop_principal_raiz <- .get_proposicao_raiz_from_lista(id_prop, lista_apensadas)

  return(tibble(id_prop, casa_origem, id_prop_principal_raiz))
}

#' @title Procura pela proposição raiz na lista da árvore de apensadas passada como parâmetro
#' @param id_prop Id da proposição apensadas
#' @param lista_apensadas Lista com árvore de apensadas capturada para a casa de origem da proposição
#' @return Id da proposição raiz
.get_proposicao_raiz_from_lista <- function(id_prop, lista_apensadas) {
  if (is.null(lista_apensadas[[id_prop]]) || lista_apensadas[[id_prop]] == 'raiz') {
    return(id_prop)
  } else {
    .get_proposicao_raiz_from_lista(lista_apensadas[[id_prop]], lista_apensadas)
  }
}
