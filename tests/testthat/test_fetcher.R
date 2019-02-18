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
  
  test_that("fetch_votacoes() returns dataframe", {
    proposicoes_fetch_votacoes <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_votacoes(.$SENADO_ID))
    
    expect_true(is.data.frame(proposicoes_fetch_votacoes))
  })
  
  test_that('fetch_votacoes() colnames', {
    proposicoes_fetch_votacoes <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_votacoes(.$SENADO_ID))
    expect_true(all(names(proposicoes_fetch_votacoes) %in% .COLNAMES_VOT_SEN))
  })
  
  test_that('fetch_tramitacao() returns dataframe', {
    proposicoes_fetch_tramitacao <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_tramitacao(.$SENADO_ID, 'senado'))
    
    expect_true(is.data.frame(proposicoes_fetch_tramitacao))
  })
  
  test_that('fetch_emendas() returns dataframe', {
    expect_true(is.data.frame(fetch_emendas(91341, 'senado')))
  })
  
  test_that('fetch_apensadas() returns dataframe', {
    proposicoes_fetch_apensadas <- senado_df %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_apensadas(.$SENADO_ID))
    
    expect_true(is.data.frame(proposicoes_fetch_apensadas))
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
