context('test-camara-lib.R')

#PL 3729/2004 e PL 490/2007
PROPOSICOES_ID <<- c(2121442, 345311)
TAM_LISTA_PROPOSICOES <<- 2
proposicao_data <<- fetch_proposicao_camara(PROPOSICOES_ID)
tramitacao_data <<- 
  rcongresso::fetch_tramitacao(PROPOSICOES_ID) %>%
  rename_df_columns

test_that('get_ementas_in_camara() is dataframe', {
  expect_true(is.data.frame(get_ementas_in_camara(PROPOSICOES_ID)))
})

test_that("get_ementas_in_camara() don't return empty", {
  expect_true(nrow(get_ementas_in_camara(PROPOSICOES_ID)) != 0)
})

test_that("get_ementas_in_camara() returns correct columns names", {
  ementas <- get_ementas_in_camara(PROPOSICOES_ID)
  expect_true(all(sapply(ementas, class) %in% .COLNAMES_EMENTAS_CAMARA))
})

test_that('last_n_despacho_in_camara() is dataframe', {
  expect_true(is.data.frame(last_n_despacho_in_camara(tramitacao_data)))
})

test_that("last_n_despacho_in_camara() returns correct columns names", {
  last_n_despachos <- last_n_despacho_in_camara(tramitacao_data)
  expect_true(all(sapply(last_n_despachos, class) %in% .COLNAMES_LAST_DESPACHO_CAMARA))
})

test_that('extract_last_relator_in_camara() returns string', {
  proposicoes_last_relatores <- tibble::as.tibble()
  
  proposicoes_last_relatores <- as.data.frame(PROPOSICOES_ID) %>% 
    rowwise() %>%
    do(extract_last_relator_in_camara(extract_tramitacao(.$PROPOSICOES_ID)) %>% as.tibble()) %>%
    rbind(proposicoes_last_relatores,.)
  
  expect_true(is.data.frame(proposicoes_last_relatores))
})

test_that('fetch_events() returns dataframe', {
  proposicoes_fetch_events <- tibble::as.tibble()
  
  proposicoes_fetch_events <- as.data.frame(PROPOSICOES_ID) %>% 
    rowwise() %>%
    do(fetch_events(.$PROPOSICOES_ID)) %>%
    rbind(proposicoes_fetch_events,.)
  
  expect_true(is.data.frame(proposicoes_fetch_events))
})

test_that('get_latest_events() returns dataframe', {
  proposicoes_latest_events <- tibble::as.tibble()
  
  proposicoes_latest_events <- as.data.frame(PROPOSICOES_ID) %>% 
    rowwise() %>%
    do(get_latest_events(.$PROPOSICOES_ID)) %>%
    rbind(proposicoes_latest_events,.)
  
  expect_true(is.data.frame(proposicoes_latest_events))
})

test_that('extract_locais_in_camara() returns dataframe', {
  expect_true(is.data.frame(extract_locais_in_camara(tramitacao_data)))
})

test_that("extract_locais_in_camara() returns correct columns names", {
  locais_in_camara <- extract_locais_in_camara(tramitacao_data)
  expect_true(all(sapply(locais_in_camara, class) %in% .COLNAMES_EXTRACT_LOCAIS_IN_CAMARA))
})

test_that('get_next_events() returns dataframe', {
  proposicoes_next_events <- tibble::as.tibble()
  
  proposicoes_next_events <- as.data.frame(PROPOSICOES_ID) %>% 
    rowwise() %>%
    do(get_next_events(.$PROPOSICOES_ID)) %>%
    rbind(proposicoes_next_events,.)
  
  expect_true(is.data.frame(proposicoes_next_events))
})

test_that('extract_relator_in_camara() returns dataframe', {
  expect_true(is.data.frame(proposicao_data))
})

test_that('extract_autor_in_camara() returns dataframe', {
  proposicoes_autores <- tibble::as.tibble()
  
  proposicoes_autores <- as.data.frame(PROPOSICOES_ID) %>% 
    rowwise() %>%
    do(extract_autor_in_camara(.$PROPOSICOES_ID)) %>%
    rbind(proposicoes_autores,.)
  
  expect_true(is.data.frame(proposicoes_autores))
})

test_that("extract_autor_in_camara() returns correct columns names", {
  proposicoes_autores <- tibble::as.tibble()
  
  proposicoes_autores <- as.data.frame(PROPOSICOES_ID) %>% 
    rowwise() %>%
    do(extract_autor_in_camara(.$PROPOSICOES_ID)) %>%
    rbind(proposicoes_autores,.)
  
  expect_true(all(sapply(proposicoes_autores, class) %in% .COLNAMES_AUTHOR_CAMARA))
})

test_that('fetch_apensadas() returns dataframe', {
  proposicoes_apensados <- tibble::as.tibble()
  
  proposicoes_apensados <- as.data.frame(PROPOSICOES_ID) %>% 
    rowwise() %>%
    do(fetch_apensadas(.$PROPOSICOES_ID)) %>%
    rbind(proposicoes_apensados,.)
  
  expect_true(is.data.frame(proposicoes_apensados))
})

