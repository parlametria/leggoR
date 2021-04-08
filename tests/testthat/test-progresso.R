context("progresso")

# Setup
setup <- function(){
  etapas <<- list()
  etapas %<>% append(list(process_etapa(2088990, "camara", as.data.frame())))
  etapas %<>% append(list(process_etapa(91341, "senado", as.data.frame(), T)))
  etapas %<>% purrr::pmap(dplyr::bind_rows)
  progresso <<- get_progresso(etapas$proposicao, etapas$fases_eventos)

  etapas_pec <<- list()
  etapas_pec %<>% append(list(process_etapa(1198512, "camara", as.data.frame())))
  etapas_pec %<>% purrr::pmap(dplyr::bind_rows)
  progresso_pec <<- get_progresso(etapas_pec$proposicao, etapas_pec$fases_eventos)


  etapas_pls_plenario_1 <<- list()
  etapas_pls_plenario_1 %<>% append(list(process_etapa(2209381, "camara", as.data.frame())))
  etapas_pls_plenario_1 %<>% purrr::pmap(dplyr::bind_rows)
  progresso_pls_plenario_1 <<- get_progresso(etapas_pls_plenario_1$proposicao, etapas_pls_plenario_1$fases_eventos)

  etapas_pls_plenario_2 <<- list()
  etapas_pls_plenario_2 %<>% append(list(process_etapa(257161, "camara", as.data.frame())))
  etapas_pls_plenario_2 %<>% purrr::pmap(dplyr::bind_rows)
  progresso_pls_plenario_2 <<- get_progresso(etapas_pls_plenario_2$proposicao, etapas_pls_plenario_2$fases_eventos)

  return(TRUE)
}

test <- function() {
  test_that("get_progresso() returns dataframe", {
    expect_true(is.data.frame(progresso))
    expect_true(is.data.frame(progresso_pec))
  })
  test_that("get_progresso() is not empty", {
    expect_true(nrow(progresso) != 0)
    expect_true(nrow(progresso_pec) != 0)
  })

  test_that("Test regular PL in Comissões phase", {
    etapa_comissoes_construcao <- tibble::tibble(fase_global = 'Construção', local = 'Comissões')

    expect_true(nrow(progresso_pls_plenario_2 %>%
                       dplyr::inner_join(etapa_comissoes_construcao, by=c("fase_global", "local")) %>%
                       dplyr::filter(!is.na(data_inicio))) > 0)
  })

  test_that("PLs which have special Plenário events have their progresso correctly inferred", {
    etapa_plenario_construcao <- tibble::tibble(fase_global = 'Construção', local = 'Plenário')

    expect_true(nrow(progresso_pls_plenario_1 %>%
                       dplyr::inner_join(etapa_plenario_construcao, by=c("fase_global", "local")) %>%
                       dplyr::filter(!is.na(data_inicio))) > 0)
  })

  test_that('progresso mpv', {
    pautas <- tibble::tribble(~data, ~sigla, ~id_ext, ~local, ~casa, ~semana, ~ano)
    id_mpv_em_tramitacao <- 135061
    id_mpv_tramitacao_encerrada <- 134134

    etapas <- list()
    etapas %<>% append(list(process_etapa(id_mpv_tramitacao_encerrada, "senado", pautas = pautas, retry = T)))
    etapas %<>% purrr::pmap(dplyr::bind_rows)
    progresso_mpv_tramitacao_encerrada <-
      agoradigital::generate_progresso_df_mpv(etapas$fases_eventos, etapas$proposicao) %>%
      dplyr::mutate(data_inicio = as.character(data_inicio),
                    data_fim = as.character(data_fim))

    progresso_134134 <-
      tibble::tribble(~ casa, ~ prop_id, ~ fase_global, ~ data_inicio, ~ data_fim,
                      'senado',134134,'Comissão Mista','2018-08-17 00:00:01','2018-09-11 00:00:04',
                      'senado',134134,'Câmara dos Deputados','2018-09-11 00:00:04','2018-11-13 00:00:01',
                      'senado',134134,'Senado Federal','2018-11-13 00:00:01','2018-11-21 00:00:03',
                      'senado',134134,'Câmara dos Deputados - Revisão','2018-11-21 00:00:03','2018-12-27 00:00:00',
                      'senado',134134,'Sanção Presidencial/Promulgação','2018-12-27 00:00:00','2018-12-27 00:00:00') %>%
      dplyr::mutate(prop_id = as.integer(prop_id))

    expect_true(nrow(dplyr::anti_join(progresso_mpv_tramitacao_encerrada, progresso_134134)) == 0)

    progresso_pec_gabarito <-
      tibble::tribble(
        ~ casa, ~ prop_id, ~ fase_global,      ~ local,                     ~ data_inicio,        ~ data_fim,     ~ local_casa,
      "camara", 1198512, "Construção",          "Plenário",                 "2020-07-21 16:18:00", "2020-07-22 00:00:00", "camara",
      "camara", 1198512, "Construção",          "Comissões",                "2015-04-23 17:14:00", "2020-07-21 00:00:00", "camara",
      "camara", 1198512, "Pré-Construção",      "",                         NA,                    NA,                  NA,
      "camara", 1198512, "Pré-Revisão I",       "",                         NA,                    NA,                  NA,
      "camara", 1198512, "Revisão I",           "Comissões",                NA,                    NA,                  NA,
      "camara", 1198512, "Revisão I",           "Plenário",                 NA,                    NA,                  NA,
      "camara", 1198512, "Pré-Revisão II",      "",                         NA,                    NA,                  NA,
      "camara", 1198512, "Revisão II",          "Comissões",                NA,                    NA,                  NA,
      "camara", 1198512, "Revisão II",          "Plenário",                 NA,                    NA,                  NA,
      "camara", 1198512, "Promulgação/Veto",    "Presidência da República", NA,                    NA,                  "presidência da república",
      "camara", 1198512, "Avaliação dos Vetos", "Congresso",                NA,                    NA,                  "congresso" )

    progresso_pec_gabarito <-
      progresso_pec_gabarito %>% dplyr::mutate(prop_id = as.integer(prop_id),
                                         data_fim = as.POSIXct(data_fim))

    expect_equal(progresso_pec %>% dplyr::mutate(data_inicio = as.character(data_inicio)), progresso_pec_gabarito)


  })
}
check_api <- function() {
  tryCatch(setup(), error = function(e){return(FALSE)})
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
