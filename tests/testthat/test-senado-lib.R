testthat::context('test-senado-lib.R')

# Constantes
PROPOSICOES_ID <<- c(91341)
TAM_LISTA_PROPOSICOES <<- 2
APRECIACAO_91341 <<- 'Plenário'
TRAMITACAO_91341 <<- 'Ordinária'
phase_one <- c('^Este processo contém')
phase_two <- c(91, 99)
phase_three <- c(42, 110, 88)
phase_four <- c(52)

important_eventos <- tibble::tribble(~ evento, ~ situacao_codigo_situacao,
                                        #'aprovacao_audiencia_publica', '110',
                                        'aprovacao_parecer', '89',
                                        'aprovacao_substitutivo', '113',
                                        'pedido_vista', '90',
                                        'aprovacao_projeto', '25')

# PLS 229/2009 e PL 526/1999
# Setup
setup <- function(){
  ## tramitacao_data <<- readr::read_csv(paste0(here::here("data/Senado/"), 91341, "-tramitacao-senado.csv"))
  PROPOSICOES_ID <- c(91341)
  tramitacao_data <- agoradigital::fetch_tramitacao(PROPOSICOES_ID, 'senado', TRUE)
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  
  test_that('fetch_tramitacao()', {
    expect_true(all(names(fetch_tramitacao(PROPOSICOES_ID, 'senado')) %in% .COLNAMES_TRAMI_SEN))
  })
  
  test_that('Regime de apreciacao', {
    expect_equal(extract_forma_apreciacao_senado(PROPOSICOES_ID), APRECIACAO_91341)
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')