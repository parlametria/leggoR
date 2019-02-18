context("Proposições")

# Setup
setup <- function(){
  CAMARA_ID <<- c(2158425, 2121442)
  SENADO_ID <<- c(91341, 111048)
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  
  test_that("fetch_proposicao() returns dataframe", {
    proposicoes_senado <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$SENADO_ID, 'senado'))
    
    proposicoes_camara <- as.data.frame(CAMARA_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$CAMARA_ID, 'camara'))
    
    expect_true(is.data.frame(proposicoes_senado))
    expect_true(is.data.frame(proposicoes_camara))
  })
  
  test_that("fetch_proposicao() not empty", {
    proposicoes_senado <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$SENADO_ID, 'senado'))
    
    proposicoes_camara <- as.data.frame(CAMARA_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$CAMARA_ID, 'camara'))
    
    expect_true(nrow(proposicoes_senado) != 0)
    expect_true(nrow(proposicoes_camara) != 0)
  })
  
 
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
