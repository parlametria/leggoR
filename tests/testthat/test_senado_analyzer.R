context("senado_analyzer")

# Setup
setup <- function(){
  SENADO_ID <<- c(91341, 127753, 129808, 133943, 96813, 120768)
  DATA_INICIO <<- '2018-07-03'
  DATA_FIM <<- '2018-07-10'
  senado_df <<- as.data.frame(SENADO_ID)
  eventos <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(extract_evento_Senado(fetch_tramitacao(.$SENADO_ID, 'senado')))
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  
  test_that("extract_evento_Senado() returns dataframe", {
    expect_true(is.data.frame(.$eventos))
  })
  
  test_that('extract_evento_Senado() colnames', {
    expect_true(all(names(.$eventos) %in% .COLNAMES_EVENTO_SEN))
  })

}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
