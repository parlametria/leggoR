context('test-senado-lib.R')
source(here::here("R/congresso-lib.R"))
source(here::here("R/senado-lib.R"))
source(here::here("R/constants.R"))

#PLS 229/2009 e PL 526/1999
PROPOSICOES_ID <<- c(91341)
TAM_LISTA_PROPOSICOES <<- 2
proposicao_data <<- fetch_proposicao(PROPOSICOES_ID, 'senado')
tramitacao_data <<- fetch_tramitacao(PROPOSICOES_ID)

test_that('fetch_votacoes() is dataframe', {
  expect_true(is.data.frame(fetch_votacoes(PROPOSICOES_ID)))
})

test_that('fetch_tramitacao() is dataframe', {
  expect_true(is.data.frame(fetch_tramitacao(PROPOSICOES_ID)))
})

test_that('fetch_deferimento() is dataframe', {
  expect_true(is.data.frame(fetch_proposicao(PROPOSICOES_ID)$proposicoes_relacionadas %>% fetch_deferimento))
})

test_that('fetch_relatorias() is dataframe', {
  expect_true(is.data.frame(fetch_relatorias(PROPOSICOES_ID)))
})

test_that('fetch_current_relatoria() is dataframe', {
  expect_true(is.data.frame(fetch_current_relatoria(PROPOSICOES_ID)))
})

test_that('fetch_last_relatoria() is dataframe', {
  expect_true(is.data.frame(fetch_last_relatoria(PROPOSICOES_ID)))
})

test_that('get_nome_ementa_Senado() is dataframe', {
  expect_true(is.data.frame(get_nome_ementa_Senado(PROPOSICOES_ID)))
})

test_that('tail_descricao_despacho_Senado() is dataframe', {
  expect_true(is.data.frame(tramitacao_data %>% tail_descricao_despacho_Senado()))
})

phase_one <- c('^Este processo contém')
phase_two <- c(91, 99)
phase_three <- c(42, 110, 88)
phase_four <- c(52)

test_that('extract_fase_Senado() is dataframe', {
  expect_true(is.data.frame(extract_fase_Senado(PROPOSICOES_ID, phase_one, phase_two, phase_three, phase_four)))
})

important_eventos <- frame_data(~ evento, ~ situacao_codigo_situacao,
                               "aprovacao_audiencia_publica", 110,
                               "aprovacao_parecer", 89,
                               "aprovacao_substitutivo", 113,
                               "pedido_vista", 90,
                               "aprovacao_projeto", 25)

test_that('extract_evento_Senado() is dataframe', {
  expect_true(is.data.frame(extract_evento_Senado(PROPOSICOES_ID, important_eventos)))
})

test_that('extract_n_last_eventos_Senado() is dataframe', {
  expect_true(is.data.frame(extract_n_last_eventos_Senado(tramitacao_data, 3)))
})

test_that('extract_comissoes_Senado() is dataframe', {
  expect_true(is.data.frame(extract_comissoes_Senado(tramitacao_data)))
})

test_that('extract_first_comissoes_Senado() is dataframe', {
  expect_true(is.data.frame(extract_first_comissoes_Senado(tramitacao_data)))
})

test_that('extract_locais() is dataframe', {
  expect_true(is.data.frame(extract_locais(tramitacao_data)))
})

test_that("fetch_votacoes()", {
  expect_true(all(names(fetch_votacoes(PROPOSICOES_ID)) %in% .COLNAMES_VOT_SEN))
})

test_that("fetch_votacoes()", {
  expect_true(all(names(fetch_tramitacao(PROPOSICOES_ID)) %in% .COLNAMES_TRAMI_SEN))
})