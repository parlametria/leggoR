context("senado_analyzer")

# Setup
setup <- function(){
  etapas <<- list()
  etapas %<>% append(list(process_etapa(1635730, "camara", as.data.frame())))
  etapas %<>% append(list(process_etapa(126084, "senado", as.data.frame())))
  etapas %<>% purrr::pmap(dplyr::bind_rows)
  progresso <<- get_progresso(etapas$proposicao, etapas$fases_eventos)
  
  etapas_pec <<- list()
  etapas_pec %<>% append(list(process_etapa(1198512, "camara", as.data.frame())))
  etapas_pec %<>% purrr::pmap(dplyr::bind_rows)
  progresso_pec <<- get_progresso(etapas_pec$proposicao, etapas_pec$fases_eventos)
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
  
  test_that('progresso mpv', {
    agenda <- tibble::as_tibble()
    pautas <- tibble::tribble(~data, ~sigla, ~id_ext, ~local, ~casa, ~semana, ~ano)
    id_mpv_em_tramitacao <- 135061
    id_mpv_tramitacao_encerrada <- 134134
    
    etapas <- list()
    etapas %<>% append(list(process_etapa(id_mpv_tramitacao_encerrada, "senado", agenda, pautas = pautas)))
    etapas %<>% purrr::pmap(dplyr::bind_rows)
    progresso_mpv_tramitacao_encerrada <-
      agoradigital::generate_progresso_df_mpv(etapas$fases_eventos) %>% 
      dplyr::mutate(data_inicio = as.character(data_inicio),
                    data_fim = as.character(data_fim))
    
    progresso_134134 <- 
      tibble::tribble(~ casa, ~ prop_id, ~ fase_global, ~ data_inicio, ~ data_fim,
                      'senado',134134,'Comissão Mista','2018-08-17 12:00:00','2018-09-17 12:00:00',
                      'senado',134134,'Câmara dos Deputados','2018-09-17 12:00:00','2018-11-13 12:00:00',
                      'senado',134134,'Senado Federal','2018-11-13 12:00:00','2018-11-21 12:00:00',
                      'senado',134134,'Câmara dos Deputados - Revisão','2018-11-21 12:00:00','2018-12-27 12:00:00',
                      'senado',134134,'Sanção Presidencial/Promulgação','2018-12-27 12:00:00','2018-12-27 12:00:00') %>% 
      dplyr::mutate(prop_id = as.integer(prop_id))
    
    expect_true(nrow(dplyr::anti_join(progresso_mpv_tramitacao_encerrada, progresso_134134)) == 0)
    
    etapas <- list()
    etapas %<>% append(list(process_etapa(id_mpv_em_tramitacao, "senado", agenda, pautas = pautas)))
    etapas %<>% purrr::pmap(dplyr::bind_rows)
    progresso_mpv_em_tramitacao <-
      agoradigital::generate_progresso_df_mpv(etapas$fases_eventos) %>% 
      dplyr::mutate(data_inicio = as.character(data_inicio),
                    data_fim = as.character(data_fim))
    
    progresso_135061 <- 
      tibble::tribble(~ casa, ~ prop_id, ~ fase_global, ~ data_inicio, ~ data_fim,
                      'senado',135061,'Comissão Mista','2018-12-28 12:00:00',NA,
                      'senado',135061,'Câmara dos Deputados',NA,NA,
                      'senado',135061,'Senado Federal',NA,NA,
                      'senado',135061,'Câmara dos Deputados - Revisão',NA,NA,
                      'senado',135061,'Sanção Presidencial/Promulgação',NA,NA) %>% 
      dplyr::mutate(prop_id = as.integer(prop_id))
    
    expect_true(nrow(dplyr::anti_join(progresso_mpv_em_tramitacao, progresso_135061)) == 0)
    
    progresso_pec_gabarito <- 
      tibble::tribble(
        ~ casa, ~ prop_id, ~ fase_global,      ~ local,                     ~ data_inicio,        ~ data_fim,     ~ local_casa,
      "camara", 1198512, "Pré-Construção",      "",                         NA,                    NA,                  NA,        
      "camara", 1198512, "Construção",          "Comissões",                "2015-04-23 17:14:00", NA,                  "camara",    
      "camara", 1198512, "Construção",          "Plenário",                 NA,                    NA,                  NA,        
      "camara", 1198512, "Pré-Revisão I",       "",                         NA,                    NA,                  NA,        
      "camara", 1198512, "Revisão I",           "Comissões",                NA,                    NA,                  NA,        
      "camara", 1198512, "Revisão I",           "Plenário",                 NA,                    NA,                  NA,        
      "camara", 1198512, "Pré-Revisão II",      "",                         NA,                    NA,                  NA,        
      "camara", 1198512, "Revisão II",          "Comissões",                NA,                    NA,                  NA,        
      "camara", 1198512, "Revisão II",          "Plenário",                 NA,                    NA,                  NA,        
      "camara", 1198512, "Promulgação/Veto",    "Presidência da República", NA,                    NA,                  NA,        
      "camara", 1198512, "Avaliação dos Vetos", "Congresso",                NA,                    NA,                  "congresso" )
    
    progresso_pec_gabarito <-
      progresso_pec_gabarito %>% dplyr::mutate(prop_id = as.integer(prop_id),
                                         data_fim = as.POSIXct(data_fim))
    
    expect_equal(progresso_pec %>% dplyr::mutate(data_inicio = as.character(data_inicio)), progresso_pec_gabarito)
    
    
  })
}
check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}
