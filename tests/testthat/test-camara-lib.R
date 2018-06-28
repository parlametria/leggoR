source('test-camara-lib.R')

setup <- function(){
  
  #PL 3729/2004 e PL 490/2007
  PROPOSICOES_ID <<- c(2121442, 345311)
  TAM_LISTA_PROPOSICOES <<- 4
  
  proposicoes_dados <<- fetch_proposicao(PROPOSICOES_ID)
  
  return(TRUE)
}

check_script <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  
  context("Testing integrity data")
  
  test_that('fetch_proposicao retorna uma row por id', {
    props <- camara_fetch_proposicao(2056568)
    expect_equal(nrow(props), 1)
    
    prop_id <- c(257161, 345311)
    props <- camara_fetch_proposicao(prop_id)
    expect_equal(nrow(props), 2)
  })
}

if(check_script()){
  test()
} else skip()