context("Comissões")

setup <- function(){
  COMPOSICAO_COMISSOES <<- fetch_all_composicao_comissao()
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that("fetch_all_composicao_comissao() is dataframe and  not empty", {
    expect_true(is.data.frame(COMPOSICAO_COMISSOES))
  })
  
  test_that("fetch_all_composicao_comissao() is dataframe and not empty", {
    expect_true(nrow(COMPOSICAO_COMISSOES) != 0)
  })
  
  test_that("fetch_composicao_comissoes() when the casa param is invalid", {
    expect_true(fetch_composicao_comissao("das", "invalid") == 'Parâmetro "casa" não identificado.')
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
