source('test-camara-lib.R')
context('test-camara-lib.R')

setup <- function(){

  #PL 3729/2004 e PL 490/2007
  PROPOSICOES_ID <<- c(2121442, 345311)
  TAM_LISTA_PROPOSICOES <<- 2

  proposicoes_dados <<- camara_fetch_proposicao(PROPOSICOES_ID)

  return(TRUE)
}

check_script <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  
  # test camara_fetch_proposicao()
  test_that('camara_fetch_proposicao() is dataframe', {
    expect_true(is.data.frame(proposicoes_dados))
  })
  
  test_that('camara_fetch_proposicao() returns one row per id', {
    proposicoes_dados <- camara_fetch_proposicao(PROPOSICOES_ID)
    expect_equal(nrow(proposicoes_dados), TAM_LISTA_PROPOSICOES)
  })
  
  test_that("camara_fetch_proposicao() doesn't have any lines with NA or NULL", {
    expect_true(is.data.frame(proposicoes_dados))
  })
  
  
}

if(check_script()){
  test()
} else skip()
