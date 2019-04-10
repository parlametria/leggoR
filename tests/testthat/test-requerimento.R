context("Requerimentos")

# Setup
setup <- function(){
  CAMARA_ID <<- c(2158425, 2121442)
  
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that('fetch_eventos_reqs_prop() returns dataframe', {
    expect_true(is.data.frame(fetch_eventos_reqs_prop(1635730,'camara')))
  })
  
  test_that('fetch_eventos_reqs_prop() with invalid casa returns empty dataframe', {
    expect_true(nrow(fetch_eventos_reqs_prop(3123, 'invalid')) == 0)
  }) 
  
  test_that('fetch_eventos_reqs_prop() for senado returns empty dataframe', {
    expect_true(nrow(fetch_eventos_reqs_prop(3123, 'senado')) == 0)
  }) 
  
  test_that('fetch_eventos_reqs_prop_camara() returns warning with id incorrect', {
    expect_warning(fetch_eventos_reqs_prop_camara(0))
  }) 
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')