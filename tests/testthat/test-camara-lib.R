context('test-camara-lib.R')

test_that('fetch_proposicao retorna uma row por id', {
  props <- camara_fetch_proposicao(2056568)
  expect_equal(nrow(props), 1)

  prop_id <- c(257161, 345311)
  props <- camara_fetch_proposicao(prop_id)
  expect_equal(nrow(props), 2)
})
