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

  test_that('fetch_proposicao() returns one row per id', {
    props <- camara_fetch_proposicao(PROPOSICOES_ID)
    expect_equal(nrow(props), TAM_LISTA_PROPOSICOES)
  })
  
  test_that('fetch_proposicao()')
}

if(check_script()){
  test()
} else skip()
