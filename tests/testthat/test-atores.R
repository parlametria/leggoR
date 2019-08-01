context("Atores")

setup <- function() {
  documentos_camara <- readr::read_csv("data/camara/documentos.csv")
  autores_camara <- readr::read_csv("data/camara/autores.csv")
  documentos_senado <- readr::read_csv("data/senado/documentos.csv")
  autores_senado <- readr::read_csv("data/senado/autores.csv")

  atores_camara <- create_tabela_atores_camara(documentos_camara, autores_camara)
  atores_senado <- create_tabela_atores_senado(documentos_senado, autores_senado)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function() {
  test_that("create_tabela_atores_camara() returns dataframe and is not empty", {
    expect_true(is.data.frame(atores_camara))
    expect_true(is.data.frame(atores_senado))
    expect_true(nrow(atores_camara) > 0)
    expect_true(nrow(atores_senado) > 0)
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')

