context("senado_analyzer")

# Setup
setup <- function(){
  SENADO_ID <<- c(103831, 120768)
  senado_df <<- as.data.frame(SENADO_ID)
  SENADO_EVENTOS <<- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(agoradigital::extract_evento_Senado(fetch_tramitacao(.$SENADO_ID, 'senado')))
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  
  test_that("extract_evento_Senado() returns dataframe", {
    expect_true(is.data.frame(SENADO_EVENTOS))
  })
  
  test_that('extract_evento_Senado() colnames', {
    expect_true(all(names(SENADO_EVENTOS) %in% .COLNAMES_EVENTO_SEN))
  })
  
  test_that('extract_evento_Senado() requerimento de audiência', {
    IDS_REQ_AUDIENCIA <- c(103831, 120768, 126364, 103831, 115926, 102721, 106330, 91341)
    expect_true(
      all(
        purrr::map(SENADO_ID, ~ any(
          agoradigital::extract_evento_Senado(
            fetch_tramitacao(.x, 'senado'))$evento == "requerimento_audiencia_publica")
          )
        )
      )
  })
  
  test_that('extract_evento_Senado() apresentação parecer', {
    IDS_APRESENTACAO_PARECER <- c(127753, 111048, 121572, 91341, 106330, 96813)
    expect_true(
      all(
        purrr::map(IDS_APRESENTACAO_PARECER, ~ any(
          agoradigital::extract_evento_Senado(
            fetch_tramitacao(.x, 'senado'))$evento == "apresentacao_parecer")
        )
      )
    )})
    
  # Garante que eventos de apresentação de parecer sem terem ocorrido
  # sejam capturados
  test_that('extract_evento_Senado() não apresentação de parecer', {
    IDS_NAO_APRESENTACAO_PARECE <- c(103831, 120768, 126364, 103831, 115926, 102721)
    expect_false(
      all(
        purrr::map(IDS_NAO_APRESENTACAO_PARECE, ~ !any(
          agoradigital::extract_evento_Senado(
            fetch_tramitacao(.x, 'senado'))$evento == "apresentacao_parecer")
        )
      )
    )
  })

}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
