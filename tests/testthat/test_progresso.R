context("senado_analyzer")

# Setup
setup <- function(){
  etapas <<- list()
  etapas %<>% append(list(process_etapa(1635730, "camara", as.data.frame())))
  etapas %<>% append(list(process_etapa(126084, "senado", as.data.frame())))
  etapas %<>% purrr::pmap(dplyr::bind_rows)
  progresso <<- get_progresso(etapas$proposicao, etapas$fases_eventos)
  return(TRUE)
}

test <- function() {
  test_that("get_progresso() returns dataframe", {
    expect_true(is.data.frame(progresso))
  })
  test_that("get_progresso() is not empty", {
    expect_true(nrow(progresso) != 0)
  })
}
check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}
