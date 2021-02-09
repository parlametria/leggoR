context("Orgãos")

# Setup
setup <- function(){
  COMISSOES_CAMARA <<- fetch_orgaos_camara()
  COMISSOES_SENADO <<- fetch_orgaos_senado()
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that("fetch_orgaos_camara() and fetch_orgaos_senado() is dataframe", {
    expect_true(is.data.frame(COMISSOES_CAMARA))
    expect_true(is.data.frame(COMISSOES_SENADO))
  })

  test_that("fetch_orgaos_camara() and fetch_orgaos_senado() are not empty", {
    expect_true(nrow(COMISSOES_CAMARA) != 0)
    expect_true(nrow(COMISSOES_SENADO) != 0)
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
