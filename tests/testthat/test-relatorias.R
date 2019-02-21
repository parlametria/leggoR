# testthat::context('test-relatorias.R')
#
# setup <- function(){
#   PROPOSICOES_ID <<- c(91341)
#   return(TRUE)
# }
#
# check_setup <- function(){
#   tryCatch(setup(), error = function(e){return(FALSE)})
# }
#
# test <- function(){
#   test_that('get_relatorias() is dataframe', {
#     expect_true(is.data.frame(get_relatorias(PROPOSICOES_ID, 'senado')))
#   })
#
#   test_that('get_relatorias() gets last relator', {
#     expect_true(is.data.frame(get_relatorias(PROPOSICOES_ID, 'senado', 1)))
#   })
# }
#
# if(check_setup()){
#   test()
# } else testthat::skip('Erro no setup de test-relatorias.R!')
