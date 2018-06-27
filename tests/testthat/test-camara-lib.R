context('test-camara-lib.R')

test_that('fetch proposicao funciona', {
  prop_id <- c(257161, 345311)
  ## props <- agoradigital::fetch_proposicao(prop_id)
  props <- camara_fetch_proposicao(prop_id)
  expect_equal(nrow(props), 2)
})
