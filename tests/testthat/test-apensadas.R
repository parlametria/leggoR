testthat::context('test-apensadas.R')

library(tidyverse)
library(testthat)
source(here::here("scripts/proposicoes/apensadas/process_lista_apensadas.R"))
source(here::here("scripts/proposicoes/apensadas/process_apensadas.R"))

#' @title Recupera proposição apensada no formato do dataframe usado para teste
setup_prop_apensada <- function(id_prop, casa) {
  prop <- agoradigital::fetch_proposicao(id_prop, casa) %>%
    mutate(id_leggo = "idleggo1")

  proposicao_apensada <- prop %>%
    select(id_ext = prop_id, casa, id_leggo, uri_prop_principal) %>%
    extract_id_from_uri()

  return(proposicao_apensada)
}

test_that('setup_prop_apensada() retorna a proposição principal corretamente para a câmara', {
  prop <- setup_prop_apensada("2250675", "camara")
  expect_equal(prop$id_prop_principal, "2123222")
})

test_that('setup_prop_apensada() retorna NA quando não houver proposição principal', {
  prop <- setup_prop_apensada("2190084", "camara")
  expect_true(is.na(prop$id_prop_principal))
})

test_that('setup_prop_apensada() retorna a proposição principal corretamente para o senado', {
  prop <- setup_prop_apensada("136606", "senado")
  expect_equal(prop$id_prop_principal, "135978")
})

test_that('process_lista_apensadas_por_casa() retorna a lista correta de apensamentos', {
  ## proposicao de teste PL 2215/2020 https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao=2250675
  ## Data de acesso a página da PL 03/05/2021

  prop <- setup_prop_apensada("2250675", "camara")

  lista_apensadas <- process_lista_apensadas_por_casa(prop, "camara", fresh_execution = TRUE, save_result = FALSE)

  lista_gabarito <- tibble(key = c("2250675", "2123222", "1203380", "517167"),
                           value = c("2123222", "1203380", "517167", "2190084")) %>%
    as.data.frame()

  lista_dataframe <- stack(lista) %>%
    select(key = ind, value = values) %>%
    mutate(key = as.character(key))

  expect_equal(lista_gabarito, lista_dataframe)
})

test_that('process_lista_apensadas_por_casa() retorna lista com raiz para proposição principal raiz', {
  ## proposicao de teste PL 11247/2018 https://www.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao=2190084
  ## Data de acesso a página da PL 03/05/2021

  prop <- setup_prop_apensada("2190084", "camara")

  lista_apensadas <- process_lista_apensadas_por_casa(prop, "camara", fresh_execution = TRUE, save_result = FALSE)

  lista_gabarito <- tibble(key = c("2190084"),
                           value = c("raiz")) %>%
    as.data.frame()

  lista_dataframe <- stack(lista) %>%
    select(key = ind, value = values) %>%
    mutate(key = as.character(key))

  expect_equal(lista_gabarito, lista_dataframe)
})
