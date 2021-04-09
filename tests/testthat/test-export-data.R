context('test-export-data.R')

pautas <- tibble::tribble(~data, ~sigla, ~id_ext, ~local, ~casa, ~semana, ~ano)
process_etapa_data <- process_etapa(137999, 'senado', pautas, T)
proposicao <- process_etapa_data$proposicao
tram <<- process_etapa_data$fases_eventos

test_that('Prop and Tram are dataframes', {
  expect_true(is.data.frame(proposicao))
  expect_true(is.data.frame(tram))
})

test_that("Prop and Tram don't return empty", {
  expect_true(nrow(proposicao) != 0)
  expect_true(nrow(tram) != 0)
})

test_that('process_etapa() is list', {
  expect_true(is.list(process_etapa_data))
})

