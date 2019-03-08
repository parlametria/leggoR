context("fetcher")

# Setup
setup <- function(){
  PROPOSICOES_ID <<- c(91341, 257161, 2121442, 127753)
  CAMARA_ID <<- c(257161, 2121442)
  SENADO_ID <<- c(91341, 127753)
  DATA_INICIO <<- '2018-07-03'
  DATA_FIM <<- '2018-07-10'
  senado_df <<- as.data.frame(SENADO_ID)
  camara_df <<- as.data.frame(CAMARA_ID)
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that('fetch_tramitacao() returns dataframe', {
    proposicoes_fetch_tramitacao <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_tramitacao(.$SENADO_ID, 'senado'))

    expect_true(is.data.frame(proposicoes_fetch_tramitacao))
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
