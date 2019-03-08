context('test-export-data.R')

process_etapa_data <- process_etapa(91341, 'senado', tibble::tibble())
proposicao <- process_etapa_data$proposicao
tram <<- process_etapa_data$fases_eventos
temperatura <- process_etapa_data$hist_temperatura
emendas <- process_etapa_data$emendas

test_that('get_ementas_in_camara() is dataframe', {
  expect_true(is.data.frame(proposicao))
  expect_true(is.data.frame(tram))
  expect_true(is.data.frame(temperatura))
  expect_true(is.data.frame(emendas))
})

test_that("get_ementas_in_camara() don't return empty", {
  expect_true(nrow(proposicao) != 0)
  expect_true(nrow(tram) != 0)
  expect_true(nrow(temperatura) != 0)
  expect_true(nrow(emendas) != 0)
})

test_that('process_etapa() is list', {
  expect_true(is.list(process_etapa_data))
})

