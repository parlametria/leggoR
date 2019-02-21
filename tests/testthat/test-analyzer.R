# testthat::context('test-analyzer.R')
#
# test_that('get_historico_temperatura_recente() has correct function passing the parameter day', {
#   data <- data.frame(prop_id = rep(1111,18),
#                     data_hora = c(rep(lubridate::ymd_hms('2018-09-10 12:00:00'), 4),
#                                    rep(lubridate::ymd_hms('2018-09-11 12:00:00'), 2),
#                                    rep(lubridate::ymd_hms('2018-09-12 12:00:00'), 1),
#                                    rep(lubridate::ymd_hms('2018-09-13 12:00:00'), 5),
#                                    rep(lubridate::ymd_hms('2018-09-14 12:00:00'), 3),
#                                    rep(lubridate::ymd_hms('2018-09-17 12:00:00'), 3)),
#                     evento = c("apresentacao_pl","distribuicao","designado_relator","evento_x",
#                                 "fim_prazo_emendas","voto_em_separado",
#                                 "evento_y",
#                                 "designado_relator","evento_z","parecer","parecer_pela_aprovacao","aprovacao_parecer",
#                                 "designado_relator","parecer_pela_rejeicao","aprovacao_parecer",
#                                 "designado_relator","parecer_pela_aprovacao","evento_w"),
#                     local = "Plenário",
#                     stringsAsFactors = F)
#
#   decaimento = 0.1
#   r <- 1 - decaimento
#
#   peso_despacho <- congresso_env$tipos_eventos$peso[1]
#   peso_discussao <- congresso_env$tipos_eventos$peso[2]
#   peso_votacao <- congresso_env$tipos_eventos$peso[3]
#   peso_locais <- congresso_env$tipos_locais$peso[1]
#
#   # 2018-09-10
#   p_dia1 <- 4 + (3 * peso_despacho) + (4 * peso_locais)
#   # 2018-09-11
#   p_dia2 <- 2 + peso_discussao + peso_votacao + (2 * peso_locais)
#   # 2018-09-12
#   p_dia3 <- 1 + (peso_locais)
#   # 2018-09-13
#   p_dia4 <- 5 + peso_despacho + (3 * peso_votacao) + (5 * peso_locais)
#   # 2018-09-14
#   p_dia5 <- 3 + peso_despacho +  (2 * peso_votacao) + (3 * peso_locais)
#   # 2018-09-17
#   p_dia6 <- 3 + peso_despacho + peso_votacao + (3 * peso_locais)
#   # 2018-09-18
#   p_dia7 <- 0
#
#   result <- c(
#     # 2018-09-10
#     sum(p_dia1 * r^0),
#     # 2018-09-11
#     sum(p_dia2 * r^0, p_dia1 * r^1),
#     # 2018-09-12
#     sum(p_dia3 * r^0, p_dia2 * r^1, p_dia1 * r^2),
#     # 2018-09-13
#     sum(p_dia4 * r^0, p_dia3 * r^1, p_dia2 * r^2, p_dia1 * r^3),
#     # 2018-09-14
#     sum(p_dia5 * r^0, p_dia4 * r^1, p_dia3 * r^2, p_dia2 * r^3, p_dia1 * r^4),
#     # 2018-09-17
#     sum(p_dia6 * r^0, p_dia5 * r^1, p_dia4 * r^2, p_dia3 * r^3, p_dia2 * r^4, p_dia1 * r^5),
#     # 2018-09-18
#     sum(p_dia7 * r^0, p_dia6 * r^1, p_dia5 * r^2, p_dia4 * r^3, p_dia3 * r^4, p_dia2 * r^5, p_dia1 * r^6)
#   )
#
#   result <- round(result, digits = 2)
#
#   energy_df <- get_historico_temperatura_recente(data, granularidade = 'd', decaimento = decaimento, max_date = lubridate::ymd_hms("2018-09-18 12:00:00"))
#
#   expect_true(all(energy_df$temperatura_recente == result))
# })
#
# test_that('get_historico_temperatura_recente() has correct function passing the parameter week', {
#   data <- data.frame(prop_id = rep(1111,17),
#                     data_hora = c(lubridate::ymd_hms("2018-09-03 12:00:00"),
#                                   lubridate::ymd_hms("2018-09-06 12:00:00"),
#                                   seq(lubridate::ymd_hms("2018-09-10 12:00:00"), lubridate::ymd_hms("2018-09-14 12:00:00"), by = "1 day"),
#                                   seq(lubridate::ymd_hms("2018-09-17 12:00:00"), lubridate::ymd_hms("2018-09-21 12:00:00"), by = "1 day"),
#                                   seq(lubridate::ymd_hms("2018-09-24 12:00:00"), lubridate::ymd_hms("2018-09-28 12:00:00"), by = "1 day")),
#                     evento = c("apresentacao_pl", "evento_1",
#                                "distribuicao","designado_relator","evento_a","aprovacao_parecer","evento_b",
#                                "designado_relator","inicio_prazo_emendas","fim_prazo_emendas","evento_b","evento_c",
#                                "designado_relator","evento_d","evento_e","evento_f","aprovacao_parecer"),
#                     local = c("Plenário", "evento_1",
#                               "CCJ","CFT","evento_a","Plenário","evento_b",
#                               "Plenário","CFT","CCJ","evento_b","evento_c",
#                               "CCJ","evento_d","evento_e","evento_f","CCJ"),
#                     stringsAsFactors = F)
#
#   decaimento = 0.1
#   r <- 1 - decaimento
#
#   peso_despacho <- congresso_env$tipos_eventos$peso[1]
#   peso_discussao <- congresso_env$tipos_eventos$peso[2]
#   peso_votacao <- congresso_env$tipos_eventos$peso[3]
#   peso_locais <- congresso_env$tipos_locais$peso[1]
#
#   p_week1 <- 2 + peso_despacho + peso_locais
#   p_week2 <- 5 + (2 * peso_despacho) + peso_votacao + (3 * peso_locais)
#   p_week3 <- 5 + peso_despacho + (2 * peso_discussao) + (3 * peso_locais)
#   p_week4 <- 5 + peso_despacho + peso_votacao + (2 * peso_locais)
#   result <- c(
#     # semana 1
#     sum(p_week1 * r^0),
#     # semana 2
#     sum(p_week2 * r^0, p_week1 * r^1),
#     # semana 3
#     sum(p_week3 * r^0, p_week2 * r^1, p_week1 * r^2),
#     # semana 4
#     sum(p_week4 * r^0, p_week3 * r^1, p_week2 * r^2, p_week1 * r^3)
#   )
#
#   result <- round(result, digits = 2)
#
#   energy_df <- get_historico_temperatura_recente(data, granularidade = 's', decaimento = decaimento, max_date = lubridate::ymd_hms("2018-09-28 12:00:00"))
#
#   expect_true(all(energy_df$temperatura_recente == result))
# })
#
# test_that('get_historico_temperatura_recente() quando arquiva', {
#   data <- data.frame(prop_id = rep(1111,17),
#                      data_hora = c(lubridate::ymd_hms("2018-09-03 12:00:00"),
#                                    lubridate::ymd_hms("2018-09-06 12:00:00"),
#                                    seq(lubridate::ymd_hms("2018-09-10 12:00:00"), lubridate::ymd_hms("2018-09-14 12:00:00"), by = "1 day"),
#                                    seq(lubridate::ymd_hms("2018-09-17 12:00:00"), lubridate::ymd_hms("2018-09-21 12:00:00"), by = "1 day"),
#                                    seq(lubridate::ymd_hms("2018-09-24 12:00:00"), lubridate::ymd_hms("2018-09-28 12:00:00"), by = "1 day")),
#                      evento = c("arquivamento", "evento_1",
#                                 "distribuicao","designado_relator","evento_a","aprovacao_parecer","evento_b",
#                                 "designado_relator","inicio_prazo_emendas","fim_prazo_emendas","evento_b","evento_c",
#                                 "designado_relator","evento_d","evento_e","aprovacao_parecer","arquivamento"),
#                      local = c("Plenário", "evento_1",
#                                "CCJ","CFT","evento_a","Plenário","evento_b",
#                                "Plenário","CFT","CCJ","evento_b","evento_c",
#                                "CCJ","evento_d","evento_e","CCJ","evento_f"),
#                      stringsAsFactors = F)
#
#   decaimento = 0.1
#   r <- 1 - decaimento
#
#   peso_despacho <- congresso_env$tipos_eventos$peso[1]
#   peso_discussao <- congresso_env$tipos_eventos$peso[2]
#   peso_votacao <- congresso_env$tipos_eventos$peso[3]
#   peso_locais <- congresso_env$tipos_locais$peso[1]
#
#   p_week1 <- 0
#   p_week2 <- 5 + (2 * peso_despacho) + peso_votacao + (3 * peso_locais)
#   p_week3 <- 5 + peso_despacho + (2 * peso_discussao) + (3 * peso_locais)
#   p_week4 <- 0
#   result <- c(
#     # semana 1
#     p_week1,
#     # semana 2
#     p_week2,
#     # semana 3
#     p_week3,
#     # semana 4
#     p_week4
#   )
#
#   result <- round(result, digits = 2)
#
#   energy_df <- get_historico_temperatura_recente(data, granularidade = 's', decaimento = decaimento, max_date = lubridate::ymd_hms("2018-09-28 12:00:00"))
#
#   expect_true(all(energy_df$temperatura_periodo == result))
# })
#
# test_that('get_pesos_eventos() returns all events from both houses', {
#   eventos_camara <- camara_env$eventos
#   eventos_senado <- senado_env$eventos
#   eventos_extra_senado <- purrr::map_df(senado_env$evento, ~ dplyr::bind_rows(.x)) %>%
#     dplyr::select(evento = constant, tipo)
#
#   pesos_eventos <- get_pesos_eventos()
#
#   expect_true(sum(eventos_camara$evento %in% pesos_eventos$evento) == nrow(eventos_camara))
#   expect_true(sum(eventos_senado$evento %in% pesos_eventos$evento) == nrow(eventos_senado))
#   expect_true(sum(eventos_extra_senado$evento %in% pesos_eventos$evento) == nrow(eventos_extra_senado))
# })
#
# test_that('get_pesos_eventos() returns all events with their correct weights for all events', {
#   tipos_eventos <- congresso_env$tipos_eventos
#   eventos_camara <- camara_env$eventos %>% dplyr::left_join(tipos_eventos, by="tipo")
#   eventos_senado <- senado_env$eventos %>% dplyr::left_join(tipos_eventos, by="tipo")
#   eventos_extra_senado <- purrr::map_df(senado_env$evento, ~ dplyr::bind_rows(.x)) %>%
#     dplyr::select(evento = constant, tipo) %>% dplyr::left_join(tipos_eventos, by="tipo")
#
#
#   pesos_eventos <- get_pesos_eventos()
#
#   pesos_eventos_camara <- merge(pesos_eventos,eventos_camara,by=c('evento','peso'))
#   expect_true(nrow(pesos_eventos_camara) == nrow(eventos_camara))
#
#   pesos_eventos_senado <- merge(pesos_eventos,eventos_senado,by=c('evento','peso'))
#   expect_true(nrow(pesos_eventos_senado) == nrow(eventos_senado))
#
#   pesos_eventos_extra_senado <- merge(pesos_eventos,eventos_extra_senado,by=c('evento','peso'))
#   expect_true(nrow(pesos_eventos_extra_senado) == nrow(eventos_extra_senado))
# })
