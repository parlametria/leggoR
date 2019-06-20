context("Proposições")

# Setup
setup <- function(){
  CAMARA_ID <<- c(2158425, 2121442)
  SENADO_ID <<- c(91341, 111048)
  return(TRUE)
}

pls_ids <- readr::read_csv(here::here("data/tabela_geral_ids_casa.csv"),
                           col_types = list(
                             id_camara = readr::col_double(),
                             id_senado = readr::col_double(),
                             apelido = readr::col_character(),
                             tema = readr::col_character()
                           ))

current_docs <- readr::read_csv(here::here("data/documentos.csv"),
                                col_types = list(
                                  .default = readr::col_character(),
                                  id_documento = readr::col_double(),
                                  id_principal = readr::col_double(),
                                  numero = readr::col_integer(),
                                  ano = readr::col_integer(),
                                  data_apresentacao = readr::col_datetime(format = ""),
                                  codTipo = readr::col_integer(),
                                  statusProposicao.codSituacao = readr::col_integer(),
                                  statusProposicao.codTipoTramitacao = readr::col_integer(),
                                  statusProposicao.dataHora = readr::col_datetime(format = ""),
                                  statusProposicao.sequencia = readr::col_integer()
                                ))

all_pls_ids <- agoradigital::get_all_leggo_props_ids(pls_ids)

current_docs_ids <- current_docs %>%
  dplyr::select(id_documento,
                id_principal,
                casa)

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){

  test_that("fetch_proposicao() returns dataframe and is not empty", {
    proposicoes_senado <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$SENADO_ID, 'senado'))

    proposicoes_camara <- as.data.frame(CAMARA_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$CAMARA_ID, 'camara'))

    expect_true(is.data.frame(proposicoes_senado))
    expect_true(is.data.frame(proposicoes_camara))
    expect_true(nrow(proposicoes_senado) != 0)
    expect_true(nrow(proposicoes_camara) != 0)
  })

  test_that('fetch_proposicoes() returns dataframe', {
    all_proposicios <- rbind(data.frame(id = SENADO_ID, casa = 'senado'), data.frame(id = CAMARA_ID, casa = 'camara'))
    expect_true(is.data.frame(fetch_proposicoes(all_proposicios)))
  })

  test_that('fetch_proposicao() with invalid casa', {
    expect_true(fetch_proposicao(3123, 'invalid') == "Parâmetro 'casa' não identificado.")
  })

  test_that('import_proposicao() returns dataframe and is not empty', {
    proposicoes_senado <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$SENADO_ID, 'senado', '', ''))

    proposicoes_camara <- as.data.frame(CAMARA_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$CAMARA_ID, 'camara', '', ''))

    expect_true(is.data.frame(proposicoes_senado))
    expect_true(is.data.frame(proposicoes_camara))
    expect_true(nrow(proposicoes_senado) != 0)
    expect_true(nrow(proposicoes_camara) != 0)
  })

  test_that('import_proposicao() with invalid casa', {
    expect_true(fetch_proposicao(3123, 'invalid', '', '') == "Parâmetro 'casa' não identificado.")
  })

  test_that('find_new_documentos() return dataframe', {
    expect_true(is.data.frame(find_new_documentos(all_pls_ids, current_docs_ids)))
  })

  test_that('fetch_documentos_data() return dataframe', {
    expect_true(is.data.frame(fetch_documentos_data(pls_ids)))
  })

  test_that('fetch_autores_documentos() return dataframe', {
    expect_true(is.data.frame(fetch_autores_documentos(pls_ids)))
  })

}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
