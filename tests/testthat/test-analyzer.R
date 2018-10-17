testthat::context('test-analyzer.R')

test_that('get_historico_energia_recente() has correct function passing the parameter day', {
  data <- data.frame(data_hora = c(rep(lubridate::ymd_hms('2018-09-07 12:00:00'), 4),
                                   rep(lubridate::ymd_hms('2018-09-10 12:00:00'), 2),
                                   rep(lubridate::ymd_hms('2018-09-11 12:00:00'), 1),
                                   rep(lubridate::ymd_hms('2018-09-12 12:00:00'), 5),
                                   rep(lubridate::ymd_hms('2018-09-13 12:00:00'), 3),
                                   rep(lubridate::ymd_hms('2018-09-14 12:00:00'), 3)),
                     evento = c("apresentacao_pl","distribuicao","designado_relator","parecer",
                                "fim_prazo_emendas","voto_em_separado",
                                "redistribuicao",
                                "designado_relator","inicio_prazo_emendas","parecer","parecer_pela_aprovacao","aprovacao_parecer",
                                "designado_relator","parecer_pela_rejeicao","aprovacao_parecer",
                                "designado_relator","parecer_pela_aprovacao","aprovacao_parecer"),
                     stringsAsFactors = F)

  decaimento = 0.1
  r <- 1 - decaimento
  result <- c(
    # 2018-09-7
    sum(4 * r^0),
    # 2018-09-10
    sum(2 * r^0, 4 * r^1),
    # 2018-09-11
    sum(1 * r^0, 2 * r^1, 4 * r^2),
    # 2018-09-12
    sum(5 * r^0, 1 * r^1, 2 * r^2, 4 * r^3),
    # 2018-09-13
    sum(3 * r^0, 5 * r^1, 1 * r^2, 2 * r^3, 4 * r^4),
    # 2018-09-14
    sum(3 * r^0, 3 * r^1, 5 * r^2, 1 * r^3, 2 * r^4, 4 * r^5),
    # 2018-09-17
    sum(0 * r^0, 3 * r^1, 3 * r^2, 5 * r^3, 1 * r^4, 2 * r^5, 4 * r^6),
    # 2018-09-18
    sum(0 * r^0, 0 * r^1, 3 * r^2, 3 * r^3, 5 * r^4, 1 * r^5, 2 * r^6, 4 * r^7)
  )
  
  result <- round(result, digits = 3)
  
  energy_df <- get_historico_energia_recente(data, granularidade = 'd', decaimento = decaimento, max_date = lubridate::ymd_hms("2018-09-18 12:00:00"))
  
  expect_true(all(energy_df$energia_recente == result))
})

test_that('get_historico_energia_recente() has correct function passing the parameter week', {
  data <- data.frame(data_hora = seq(lubridate::ymd_hms("2018-09-07 12:00:00"), lubridate::ymd_hms("2018-09-28 12:00:00"), by = "1 day"),
                     evento = c("apresentacao_pl",
                                "distribuicao","designado_relator","inicio_prazo_emendas","fim_prazo_emendas","parecer","parecer_pela_aprovacao","aprovacao_parecer",
                                "designado_relator","inicio_prazo_emendas","fim_prazo_emendas","parecer","parecer_pela_rejeicao","voto_em_separado","aprovacao_parecer",
                                "designado_relator","inicio_prazo_emendas","fim_prazo_emendas","parecer","parecer_pela_aprovacao","voto_em_separado","aprovacao_parecer"),
                     stringsAsFactors = F)
  
  decaimento = 0.1
  r <- 1 - decaimento
  result <- c(
    # semana 1
    sum(1 * r^0),
    # semana 2
    sum(5 * r^0, 1 * r^1),
    # semana 3
    sum(5 * r^0, 5 * r^1, 1 * r^2),
    # semana 4
    sum(5 * r^0, 5 * r^1, 5 * r^2, 1 * r^3)
  )
  
  result <- round(result, digits = 3)
  
  energy_df <- get_historico_energia_recente(data, granularidade = 's', decaimento = decaimento, max_date = lubridate::ymd_hms("2018-09-28 12:00:00"))
  
  expect_true(all(energy_df$energia_recente == result))
})

test_that('get_pesos_eventos() returns all events from both houses', {
  eventos_camara <- camara_env$eventos
  eventos_senado <- senado_env$eventos
  eventos_extra_senado <- purrr::map_df(senado_env$evento, ~ dplyr::bind_rows(.x)) %>%
    dplyr::select(evento = constant, peso)
  
  pesos_eventos <- get_pesos_eventos()
  
  expect_true(sum(eventos_camara$evento %in% pesos_eventos$evento) == nrow(eventos_camara))
  expect_true(sum(eventos_senado$evento %in% pesos_eventos$evento) == nrow(eventos_senado))
  expect_true(sum(eventos_extra_senado$evento %in% pesos_eventos$evento) == nrow(eventos_extra_senado))
})

test_that('get_pesos_eventos() returns all events with their correct weights for all events', {
  eventos_camara <- camara_env$eventos
  eventos_senado <- senado_env$eventos
  eventos_extra_senado <- purrr::map_df(senado_env$evento, ~ dplyr::bind_rows(.x)) %>%
    dplyr::select(evento = constant, peso)
  
  pesos_eventos <- get_pesos_eventos()
  
  pesos_eventos_camara <- merge(pesos_eventos,eventos_camara,by=c('evento','peso'))
  expect_true(nrow(pesos_eventos_camara) == nrow(eventos_camara))
  
  pesos_eventos_senado <- merge(pesos_eventos,eventos_senado,by=c('evento','peso'))
  expect_true(nrow(pesos_eventos_senado) == nrow(eventos_senado))
  
  pesos_eventos_extra_senado <- merge(pesos_eventos,eventos_extra_senado,by=c('evento','peso'))
  expect_true(nrow(pesos_eventos_extra_senado) == nrow(eventos_extra_senado))
})
