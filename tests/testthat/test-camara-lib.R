source('test-camara-lib.R')

setup <- function(){
  
  #PL 3729/2004 e PL 490/2007
  PROPOSICOES_ID <<- c(2121442, 345311)
  TAM_LISTA_PROPOSICOES <<- 2
  
  proposicoes_dados <<- fetch_proposicao(PROPOSICOES_ID)
  
  return(TRUE)
}

check_script <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  
  context("Testing data integrity")
  
  test_that('fetch_proposicao retorna uma row por id', {
    props <- camara_fetch_proposicao(PROPOSICOES_ID)
    expect_equal(nrow(props), TAM_LISTA_PROPOSICOES)
  })
  
}

if(check_script()){
  test()
} else skip()