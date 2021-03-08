testthat::context('test-analyzer.R')

library(magrittr)

tram_91341 <<- fetch_tramitacao(91341, 'senado')
prop_91341 <<- fetch_proposicao(91341, 'senado')
tram_257161 <<- fetch_tramitacao(257161, 'camara')
prop_257161 <<- fetch_proposicao(257161, 'camara')
tram_mpv_868 <<- fetch_tramitacao(135061, 'senado')
prop_mpv_868 <<- fetch_proposicao(135061, 'senado')
mpv_sem_eficacia_867 <<- agoradigital::extract_evento_Senado(agoradigital::fetch_tramitacao(135060, 'senado', T))
mpv_sem_eficacia_868 <<- agoradigital::extract_evento_Senado(agoradigital::fetch_tramitacao(135061, 'senado', T))
mpv_rejeitada_850 <<- agoradigital::extract_evento_Senado(agoradigital::fetch_tramitacao(134245, 'senado', T))
mpv_rejeitada_816 <<- agoradigital::extract_evento_Senado(agoradigital::fetch_tramitacao(132070, 'senado', T))
mpv_aprovada_870 <<- agoradigital::extract_evento_Senado(agoradigital::fetch_tramitacao(135064, 'senado', T))


test_that('get_pesos_eventos() returns all events from both houses', {
  eventos_camara <- camara_env$eventos
  eventos_senado <- senado_env$eventos
  eventos_extra_senado <- purrr::map_df(senado_env$evento, ~ dplyr::bind_rows(.x)) %>%
    dplyr::select(evento = constant, tipo)

  pesos_eventos <- get_pesos_eventos()

  expect_true(sum(eventos_camara$evento %in% pesos_eventos$evento) == nrow(eventos_camara))
  expect_true(sum(eventos_senado$evento %in% pesos_eventos$evento) == nrow(eventos_senado))
  expect_true(sum(eventos_extra_senado$evento %in% pesos_eventos$evento) == nrow(eventos_extra_senado))
})

test_that('get_pesos_eventos() returns all events with their correct weights for all events', {
  tipos_eventos <- congresso_env$tipos_eventos
  eventos_camara <- camara_env$eventos %>% dplyr::left_join(tipos_eventos, by="tipo")
  eventos_senado <- senado_env$eventos %>% dplyr::left_join(tipos_eventos, by="tipo")
  eventos_extra_senado <- purrr::map_df(senado_env$evento, ~ dplyr::bind_rows(.x)) %>%
    dplyr::select(evento = constant, tipo) %>% dplyr::left_join(tipos_eventos, by="tipo")

  pesos_eventos <- get_pesos_eventos()

  pesos_eventos_camara <- merge(pesos_eventos,eventos_camara,by=c('evento','peso'))
  expect_true(nrow(pesos_eventos_camara) == nrow(eventos_camara))

  pesos_eventos_senado <- merge(pesos_eventos,eventos_senado,by=c('evento','peso'))
  expect_true(nrow(pesos_eventos_senado) == nrow(eventos_senado))

  pesos_eventos_extra_senado <- merge(pesos_eventos,eventos_extra_senado,by=c('evento','peso'))
  expect_true(nrow(pesos_eventos_extra_senado) == nrow(eventos_extra_senado))
})

test_that('process_proposicao() retorna abertura e encerramento do prazo das emendas', {
  id <- 91341
  casa <- "senado"
  prop <- agoradigital::fetch_proposicao(id, casa)
  tram <- agoradigital::fetch_tramitacao(id, casa)
  proc_tram <-
    agoradigital::process_proposicao(prop, tram, casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))

  expect_true(all(c("inicio_prazo_emendas", "fim_prazo_emendas") %in% proc_tram$evento))
})

test_that('extract_autor_in_camara() returns the right cols and author', {
  autor_camara <- agoradigital::extract_autor_in_camara(2121442)
  expect_true(all(sapply(autor_camara, class) %in% .COLNAMES_AUTOR_CAMARA))
  expect_true(autor_camara$autor.nome == "Senado Federal - Comissão Especial do Extrateto SF /")

})

test_that('extract_status_tramitacao() returns dataframe', {
  expect_true(is.data.frame(extract_status_tramitacao(91341, 'senado', prop_91341, tram_91341)))
  expect_true(is.data.frame(extract_status_tramitacao(257161, 'camara', prop_257161, tram_257161)))
})

test_that('extract_forma_apreciacao() is not null', {
  expect_false(is.null(extract_forma_apreciacao(tram_91341)))
  expect_false(is.null(extract_forma_apreciacao(tram_257161)))
})

test_that('extract_regime_tramitacao() is not null', {
  expect_false(is.null(extract_regime_tramitacao(tram_91341, prop_91341)))
  expect_false(is.null(extract_regime_tramitacao(tram_257161, prop_257161)))
  expect_true(extract_regime_tramitacao(tram_mpv_868, prop_mpv_868) == "Urgência")
})


test_that('get_pesos_eventos() returns dataframe and is not empty', {
  expect_true(is.data.frame(get_pesos_eventos()))
  expect_true(nrow(get_pesos_eventos()) != 0)
})

test_that('get_pesos_locais() returns dataframe and is not empty', {
  expect_true(is.data.frame(get_pesos_locais()))
  expect_true(nrow(get_pesos_locais()) != 0)
})

test_that('get_comissoes_faltantes()', {
  prop_faltante <- agoradigital::fetch_proposicao(2085536, 'camara')
  tram_faltante <- agoradigital::fetch_tramitacao(2085536, 'camara')
  process_faltante <- agoradigital::process_proposicao(prop_faltante, tram_faltante, 'camara')
  expect_true(nrow(get_comissoes_faltantes(process_faltante, 'camara')) != 0)

  prop_completa <- agoradigital::fetch_proposicao(91341, 'senado')
  tram_completa <- agoradigital::fetch_tramitacao(91341, 'senado')
  process_completa<- agoradigital::process_proposicao(prop_completa, tram_completa, 'senado')
  expect_true(nrow(get_comissoes_faltantes(process_completa, 'senado')) == 0)

})

test_that('testa_eventos_mpvs()', {
  expect_true(nrow(mpv_sem_eficacia_867 %>% dplyr::filter(evento == "perda_da_eficacia")) != 0)
  expect_true(nrow(mpv_sem_eficacia_868 %>% dplyr::filter(evento == "perda_da_eficacia")) != 0)
  expect_true(nrow(mpv_rejeitada_850 %>% dplyr::filter(evento == "rejeicao_projeto")) != 0)
  expect_true(nrow(mpv_rejeitada_816 %>% dplyr::filter(evento == "rejeicao_projeto")) != 0)
  expect_true(nrow(mpv_aprovada_870 %>% dplyr::filter(evento == "transformada_lei")) != 0)
})

test_that('testa_status()', {
  expect_true(
    (mpv_aprovada_870 %>% dplyr::rename(id_ext = prop_id) %>% agoradigital::adiciona_status() %>% dplyr::arrange(data_hora) %>% tail(1))$status == "Lei")
  expect_true(
    (mpv_rejeitada_850 %>% dplyr::rename(id_ext = prop_id) %>% agoradigital::adiciona_status() %>% dplyr::arrange(data_hora) %>% tail(1))$status == "Rejeitada")
  expect_true(
    (mpv_sem_eficacia_868 %>% dplyr::rename(id_ext = prop_id) %>% agoradigital::adiciona_status() %>% dplyr::arrange(data_hora) %>% tail(1))$status == "Caducou")
})

test_that('Checa detecção de eventos de parecer', {
  id <- 541857
  casa <- "camara"
  prop <- agoradigital::fetch_proposicao(id, casa)
  tram <- agoradigital::fetch_tramitacao(id, casa)
  proc_tram <-
    agoradigital::process_proposicao(prop, tram, casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))

  expect_true(all(
    c(
      "parecer_pela_adequacao_financeira_e_orcamentaria",
      "parecer_pela_aprovacao_com_substitutivo",
      "parecer_pela_aprovacao"
    ) %in% proc_tram$evento
  ))
})

test_that('Checa detecção de evento de parecer pela rejeição', {
  id <- 606722
  casa <- "camara"
  prop <- agoradigital::fetch_proposicao(id, casa)
  tram <- agoradigital::fetch_tramitacao(id, casa)
  proc_tram <-
    agoradigital::process_proposicao(prop, tram, casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))

  expect_true(all(
    c(
      "parecer_pela_rejeicao"
    ) %in% proc_tram$evento
  ))
})

test_that('Checa detecção de evento de virada de casa e remetida à sanção', {
  id <- 2249891
  casa <- "camara"
  prop <- agoradigital::fetch_proposicao(id, casa)
  tram <- agoradigital::fetch_tramitacao(id, casa)
  proc_tram <-
    agoradigital::process_proposicao(prop, tram, casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))

  expect_true(all(
    c(
      "virada_de_casa",
      "remetida_a_sancao_promulgacao"
    ) %in% proc_tram$evento
  ))
})

test_that('Checa detecção de evento aprovação de parecer', {
  id <- 2120019
  casa <- "camara"
  prop <- agoradigital::fetch_proposicao(id, casa)
  tram <- agoradigital::fetch_tramitacao(id, casa)
  proc_tram <-
    agoradigital::process_proposicao(prop, tram, casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))

  expect_true(all(
    c(
      "aprovacao_parecer"
    ) %in% proc_tram$evento
  ))
})

test_that('Checa detecção de evento de alteração de regime da proposição', {
  id <- 2158425
  casa <- "camara"
  prop <- agoradigital::fetch_proposicao(id, casa)
  tram <- agoradigital::fetch_tramitacao(id, casa)
  proc_tram <-
    agoradigital::process_proposicao(prop, tram, casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))

  expect_true(all(
    c(
      "alteracao_de_regime"
    ) %in% proc_tram$evento
  ))
})

test_that('Checa detecção de evento de designado relator', {
  id <- 2180392
  casa <- "camara"
  prop <- agoradigital::fetch_proposicao(id, casa)
  tram <- agoradigital::fetch_tramitacao(id, casa)
  proc_tram <-
    agoradigital::process_proposicao(prop, tram, casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))

  expect_true(all(
    c(
      "designado_relator"
    ) %in% proc_tram$evento
  ))
})




