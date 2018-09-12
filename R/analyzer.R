source(here::here("R/senado_analyzer.R"))
source(here::here("R/camara_analyzer.R"))

congress_constants <- jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))$constants

#' @title Processa dados de um proposição do congresso.
#' @description Recebido um dataframe a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/<camara/Senado>.
#' @param tramitacao_df Dataframe com tramitação da proposição
#' @param casa Casa onde o PL está tramitando ('camara'/'senado').
#' @importFrom magrittr '%>%'
#' @export
process_proposicao <- function(proposicao_df, tramitacao_df, casa, out_folderpath=NULL) {
  proc_tram_data <- NULL
  prop_id <- NULL
  if (tolower(casa) == congress_constants$camara_label) {
    proc_tram_data <- process_proposicao_camara_df(proposicao_df = proposicao_df, tramitacao_df=tramitacao_df)
    prop_id <- proc_tram_data[1,"prop_id"]
  } else if (tolower(casa) == congress_constants$senado_label) {
    proc_tram_data <- process_proposicao_senado_df(proposicao_df = proposicao_df, tramitacao_df=tramitacao_df)
    prop_id <- proc_tram_data[1,"prop_id"]
  }
  
  if((!is.null(proc_tram_data)) & (!is.null(out_folderpath))) {
      readr::write_csv(proc_tram_data, paste0(out_folderpath,'/',casa,'/',prop_id,'-fases-tramitacao-',casa,'.csv'))
  }
  return(proc_tram_data)
}

#' @title Retorna energia de uma proposição no congresso.
#' @description Recebido o dataframe da tramitação contendo as colunas: data_hora e evento,
#' retorna um valor que indica a energia da proposição
#' @param tramitacao_df Dataframe da tramitação contendo as colunas: data_hora e evento
#' @param days_ago Quantidade de dias a serem analizados. O padrão é 30
#' @param pivot_day Dia de partida de onde começará a análise. O padrão é o dia atual
#' @return Energia de uma proposição.
#' @importFrom magrittr '%>%'
#' @export
get_energia <- function(tramitacao_df, days_ago = 30, pivot_day = lubridate::today()) {
  working_days <- ((days_ago / 7) * 5)
  
  start_date = pivot_day - lubridate::days(days_ago)
  
  qtd_eventos <-
    tramitacao_df %>%
    dplyr::filter(data_hora >= start_date) %>%
    dplyr::filter(!is.na(evento)) %>%
    nrow()
  
  qtd_eventos / working_days
}

#' @title Retorna o histórico da energia de uma proposição no congresso.
#' @description Recebido o dataframe da tramitação contendo as colunas: data_hora e evento,
#' retorna um dataframe com o valor da energia recente calculado para cada dia útil da tramitação de uma proposição,
#' computado levando em consideração os eventos que aconteceram nos 30 dias anteriores à data pivô, e aplicando um decaimento exponencial.
#' @param events_df Dataframe da tramitação contendo as colunas: data_hora e evento
#' @param tamanho_janela Quantidade de dias a serem analizados. O padrão é 30
#' @param decaimento A porcentagem de redução do valor da energia por dia. O padrão é 10% (0.1)
#' @param pivot_day Dia de partida de onde começará a análise. O padrão é o dia atual
#' @return Dataframe com o valor da energia recente para cada dia útil da tramitação de uma proposição.
#' @importFrom magrittr '%>%'
#' @export
get_historico_energia_recente <- function(eventos_df, tamanho_janela = 30, decaimento = 0.1, pivot_day = lubridate::today()) {
  #Remove tempo do timestamp da tramitação
  extended_events <- eventos_df %>%
    dplyr::mutate(data_hora = lubridate::floor_date(data_hora, unit="day"))
  
  #Adiciona linhas para os dias úteis nos quais não houve movimentações na tramitação
  #Remove linhas referentes a dias de recesso parlamentar
  full_dates <- data.frame(data_hora = seq(min(extended_events$data_hora), max(extended_events$data_hora), by = "1 day"))
  extended_events <- merge(full_dates,extended_events, by="data_hora", all.x = TRUE) %>%
    dplyr::filter(!(lubridate::wday(data_hora) %in% c(1,7))) %>%
    dplyr::filter(lubridate::month(data_hora) != 1) %>%
    dplyr::filter(!((lubridate::month(data_hora) == 2) & (lubridate::day(data_hora) < 2))) %>%
    dplyr::filter(!((lubridate::month(data_hora) == 7) & (lubridate::day(data_hora) > 17))) %>%
    dplyr::filter(!((lubridate::month(data_hora) == 12) & (lubridate::day(data_hora) > 22)))
  
  #Agrupa eventos por dia
  energia_diaria <- extended_events %>%
    dplyr::group_by(data_hora) %>%
    dplyr::summarize(energia_dia = sum(!is.na(evento)))
  
  #Computa soma deslizante com decaimento exponencial
  weights <- (1-decaimento) ^ (tamanho_janela:1)
  energia_total <- data.frame(energia_total = sigmoid1(round(roll::roll_sum(data.matrix(energia_diaria$energia_dia), tamanho_janela, weights, min_obs = 1), digits=3)))
  historico_energia <- dplyr::bind_cols(energia_diaria,energia_total)
}

#' @title Extrai o regime de tramitação de um PL
#' @description Obtém o regime de tramitação de um PL
#' @param tram_df Dataframe da tramitação do PL.
#' @return String com a situação do regime de tramitação da pl.
#' @examples
#' extract_regime_tramitacao(fetch_tramitacao(91341,'senado', TRUE))
#' @export
extract_regime_tramitacao <- function(tram_df) {
  casa <- tram_df[1, "casa"]
  regime <- NULL
  
  if (casa == congress_constants$camara_label) {
    regime <- extract_regime_tramitacao_camara(tram_df)
  } else if (casa == congress_constants$senado_label) {
    regime <- extract_regime_tramitacao_senado(tram_df)
  }
  
  regime
}

#' @title Extrai a forma de apreciação de um PL
#' @description Obtém a forma de apreciação de um PL
#' @param proposicao_id id do PL
#' @return String que define a forma de apreciação do PL
#' @examples
#' extract_forma_apreciacao(fetch_tramitacao(91341, 'senado', TRUE))
#' @export
#' @importFrom stats filter
extract_forma_apreciacao <- function(tram_df) {
  casa <- tram_df[1, "casa"]
  prop_id <- tram_df[1, "prop_id"]
  apreciacao <- NULL
  
  if (casa == congress_constants$camara_label) {
    apreciacao <- extract_forma_apreciacao_camara(prop_id)
  } else if (casa == congress_constants$senado_label) {
    apreciacao <- extract_forma_apreciacao_senado(prop_id)
  }
  
  apreciacao
}

#' @title Extrai o status da tramitação de um PL
#' @description Obtém o status da tramitação de um PL
#' @param tram_df Dataframe da tramitação do PL.
#' @return Dataframe contendo id, regime de tramitação e forma de apreciação do PL
#' @examples
#' extract_status_tramitacao(fetch_tramitacao(91341, 'senado', TRUE))
#' @export
#' @importFrom stats filter
extract_status_tramitacao <- function(tram_df) {
  regime <- extract_regime_tramitacao(tram_df)
  apreciacao <- extract_forma_apreciacao(tram_df)
  status_tram <- data.frame(prop_id=tram_df[1,]$prop_id,regime_tramitacao=regime,forma_apreciacao=apreciacao)
}
