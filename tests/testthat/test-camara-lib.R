context('test-camara-lib.R')

#PL 3729/2004 e PL 490/2007
PROPOSICOES_ID <<- c(2121442)

tramitacao_data <<- agoradigital::fetch_tramitacao(PROPOSICOES_ID, 'camara')

test_that('get_ementas_in_camara() is dataframe', {
  expect_true(is.data.frame(get_ementas_in_camara(PROPOSICOES_ID)))
})

test_that("get_ementas_in_camara() don't return empty", {
  expect_true(nrow(get_ementas_in_camara(PROPOSICOES_ID)) != 0)
})

test_that('get_ementas_in_camara() returns correct columns names', {
  ementas <- get_ementas_in_camara(PROPOSICOES_ID)
  expect_true(all(sapply(ementas, class) %in% .COLNAMES_EMENTAS_CAMARA))
})

test_that('last_n_despacho_in_camara() is dataframe', {
  expect_true(is.data.frame(last_n_despacho_in_camara(tramitacao_data)))
})

test_that('last_n_despacho_in_camara() returns correct columns names', {
  last_n_despachos <- last_n_despacho_in_camara(tramitacao_data)
  expect_true(all(
    sapply(last_n_despachos, class) %in% .COLNAMES_LAST_DESPACHO_CAMARA
  ))
})
