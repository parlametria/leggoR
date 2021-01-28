context("Tramitação")

setup <- function(){
  CAMARA_ID <<- c(257161, 2121442)
  SENADO_ID <<- c(91341, 127753)
  tramitacao_2196833 <<- fetch_tramitacao_camara(2196833)
  tramitacao_2196833_com_data <<- fetch_tramitacao_camara(2196833, lubridate::today() - 14, lubridate::today())
  tramitacao_vazia <<- fetch_tramitacao_camara(2196833, "2014-05-05", "2015-12-01")
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that('fetch_tramitacao() in senado returns dataframe', {
    proposicoes_fetch_tramitacao <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_tramitacao(.$SENADO_ID, 'senado'))

    expect_true(is.data.frame(proposicoes_fetch_tramitacao))
  })

  test_that('fetch_tramitacao() in camara returns dataframe', {
    proposicoes_fetch_tramitacao <- as.data.frame(CAMARA_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_tramitacao(.$CAMARA_ID, 'camara'))

    expect_true(is.data.frame(proposicoes_fetch_tramitacao))
  })

  test_that('fetch_tramitacoes() returns dataframe', {
    all_proposicios <- rbind(data.frame(id = SENADO_ID, casa = 'senado'), data.frame(id = CAMARA_ID, casa = 'camara'))
    expect_true(is.data.frame(fetch_tramitacoes(all_proposicios)))
  })

  test_that('fetch_tramitacoes() returns dataframe', {
    all_proposicios <- rbind(data.frame(id = SENADO_ID, casa = 'senado'), data.frame(id = CAMARA_ID, casa = 'camara'))
    expect_true(is.data.frame(fetch_tramitacoes(all_proposicios)))
  })

  test_that('fetch_tramitacao() when the casa is invalid', {
    expect_true(fetch_tramitacao(3123, 'invalid') == "Parâmetro 'casa' não identificado.")
  })

  test_that('fetch_tramitacao_camara() return dataframe', {
    expect_true(is.data.frame(tramitacao_2196833))
    expect_true(is.data.frame(tramitacao_2196833_com_data))
    expect_true(is.data.frame(tramitacao_vazia))
  })

  test_that('fetch_tramitacao_camara() return empty when there is not events on the range', {
    expect_true(nrow(tramitacao_vazia) == 0)
  })

  test_that('fetch_tramitacao_camara() return bigger dataframe whithout date', {
    expect_true(nrow(tramitacao_2196833) >= nrow(tramitacao_2196833_com_data))
  })

}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
