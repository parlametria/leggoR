context("senado_analyzer")

# Setup
setup <- function(){
  etapas <<- list()
  etapas %<>% append(list(process_etapa(1635730, "camara", as.data.frame())))
  etapas %<>% append(list(process_etapa(126084, "senado", as.data.frame())))
  etapas %<>% purrr::pmap(dplyr::bind_rows)
  progresso <<- get_progresso(etapas$proposicao, etapas$fases_eventos)
  return(TRUE)
}

test <- function() {
  test_that("get_progresso() returns dataframe", {
    expect_true(is.data.frame(progresso))
  })
  test_that("get_progresso() is not empty", {
    expect_true(nrow(progresso) != 0)
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
    
    
  })
}
check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}
