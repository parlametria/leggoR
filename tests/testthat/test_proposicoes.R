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
 
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
