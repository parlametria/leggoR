source(here::here("R/senado_analyzer.R"))
source(here::here("R/camara_analyzer.R"))
source(here::here("R/congresso-lib.R"))

congress_env <- jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))
congress_constants <- congress_env$constants

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
#' @param granularidade Granularidade do dado histórico da energia desejada ('d' = dia, 's' = semana, 'm' = mês)
#' @param decaimento A porcentagem de redução do valor da energia por dia. Valor deve estar entre 0 e 1.
#' @param max_date Último dia a ser considerado no cálculo da energia. Padrão: dia atual.
#' @return Dataframe com o valor da energia recente para cada dia útil da tramitação de uma proposição.
#' @importFrom magrittr '%>%'
#' @export
#' @examples 
#' \dontrun{
#' id <- 345311
#' casa <- 'camara'
#' prop <- agoradigital::fetch_proposicao(id,casa,TRUE)
#' tram <- agoradigital::fetch_tramitacao(id,casa,TRUE)
#' proc_tram <- agoradigital::process_proposicao(prop,tram,casa)
#' get_historico_energia_recente(proc_tram, granularidade = 's', decaimento = 0.05)
#' }
get_historico_energia_recente <- function(eventos_df, granularidade = 's', decaimento = 0.05, max_date = lubridate::now()) {
  #Remove tempo do timestamp da tramitação
  eventos_extendidos <- eventos_df %>%
    dplyr::mutate(data = lubridate::floor_date(data_hora, unit="day"))
  
  #Adiciona linhas para os dias úteis nos quais não houve movimentações na tramitação
  #Remove linhas referentes a dias de recesso parlamentar
  full_dates <- data.frame(data = seq(min(eventos_extendidos$data), max_date, by = "1 day"))
  eventos_extendidos <- merge(full_dates, eventos_extendidos, by="data", all.x = TRUE) %>%
    filtra_dias_nao_uteis_congresso()
    
  energia_periodo <- data.frame()
  
  #Agrupa eventos por período
  if (granularidade == 'd') {
    energia_periodo <- eventos_extendidos %>%
      dplyr::group_by(data)
  } else if (granularidade == 's') {
    energia_periodo <- eventos_extendidos %>%
      dplyr::mutate(semana = lubridate::week(data),
                    ano = lubridate::year(data)) %>%
      dplyr::group_by(ano,semana)
  } else if (granularidade == 'm') {
    energia_periodo <- eventos_extendidos %>%
      dplyr::mutate(mes = lubridate::month(data),
                    ano = lubridate::year(data)) %>%
      dplyr::group_by(ano,mes)
  }
  
  energia_periodo <- energia_periodo %>% 
    dplyr::summarize(periodo = dplyr::first(data),
                     energia_periodo = sum(!is.na(evento))) %>%
    dplyr::ungroup() %>%
    dplyr::select(periodo,energia_periodo) %>%
    dplyr::arrange(periodo)
  
  #Computa soma deslizante com decaimento exponencial
  tamanho_janela <- nrow(energia_periodo)
  weights <- (1-decaimento) ^ ((tamanho_janela-1):0)
  energia_recente <- data.frame(energia_recente = round(roll::roll_sum(data.matrix(energia_periodo$energia_periodo), tamanho_janela, weights, min_obs = 1), digits=3))
  historico_energia <- dplyr::bind_cols(energia_periodo,energia_recente) %>%
    dplyr::select(periodo,
                  energia_periodo,
                  energia_recente)
  
  return(historico_energia)
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

#' @title Extrai se uma proposição está em pauta
#' @description Extrai se uma proposição está em pauta
#' @param proposicao_id id do PL
#' @return TRUE se estiver em pauta FALSE caso contrário
#' @examples
#' extract_pauta(fetch_agenda('2018-07-03', '2018-07-10', 'senado'), 117839)
#' @export
extract_pauta <- function(agenda, proposicao_id) {
  proposicao_id %in% agenda$codigo_materia
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
  pauta <- extract_pauta(fetch_agenda(as.Date(cut(Sys.Date(), "week")), as.Date(cut(Sys.Date(), "week")) + 4, tram_df[1,]$casa), tram_df[1,]$prop_id)
  status_tram <- data.frame(prop_id=tram_df[1,]$prop_id,regime_tramitacao=regime,forma_apreciacao=apreciacao, em_pauta = pauta)
}

#' @title Extrai o progresso de um PL
#' @description Extrai o progresso de um PL
#' @param proposicao_df Dataframe da tramitação do PL.
#' @param tramitacao_df Dataframe da proposição do PL. 
#' @param casa Casa (Senado ou Câmara)
#' @param out_folderpath Caminho destino do csv resultante 
#' @return Dataframe contendo id, fase global, data de inicio e data de fim (data atual, se nao houver fim)
#' @examples
#' pls_senado_camara <- readr::read_csv(here::here("data/tabela_ids_senado_camara.csv"))
#' pls_senado_camara %>% get_progresso(fetch_tramitacao(257161, 'camara', T), fetch_proposicao(257161, 'camara', '', '', normalized = T), 'camara')
#' pls_senado_camara %>% get_progresso(fetch_tramitacao(115926, 'senado', T), fetch_proposicao(115926, 'senado', '', '', normalized = T), 'senado')
#' @export
get_progresso <- function(pls_senado_camara, tramitacao_df, proposicao_df, casa, out_folderpath=NULL) {
  prop_id <- proposicao_df[1, "prop_id"]
  
  casa_origem <- 
    dplyr::if_else(
      stringr::str_detect(tolower(proposicao_df$casa_origem), 'senado federal'),
                          congress_constants$senado_label,
                          congress_constants$camara_label)
  
  if (tolower(casa) == congress_constants$camara_label) {
    another_prop_id <- 
      pls_senado_camara %>% 
      dplyr::filter(prop_id == id_camara) %>%
      plyr::rename(c("id_senado" = "id")) %>%
      dplyr::select(id)
    
  } else if (tolower(casa) == congress_constants$senado_label) {
    prop_id = prop_id$prop_id
    another_prop_id <- 
      pls_senado_camara %>%
      dplyr::filter(prop_id == id_senado) %>% 
      plyr::rename(c("id_camara" = "id")) %>%
      dplyr::select(id)
    
  }
  if(nrow(another_prop_id) > 0){
    if(casa_origem == tolower(casa)){
      progresso_data <- get_progresso_both(prop_id, another_prop_id$id, casa)
    } else{
      progresso_data <- get_progresso_both(another_prop_id$id, prop_id, casa_origem)
    }
  } else {
    progresso_data <- extract_progresso(tramitacao_df, proposicao_df, casa)
  }
  
  progresso_data <- 
    progresso_data %>% 
    dplyr::mutate(local_casa = casa)
  progresso_data$casa <- casa
  progresso_data$prop_id <- prop_id
  
  if((!is.null(progresso_data)) & (!is.null(out_folderpath))) {
    readr::write_csv(progresso_data, paste0(out_folderpath,'/',casa,'/',prop_id,'-progresso-',casa,'.csv'))
    }
  
  return(progresso_data)
  
}	

#' @title Extrai o progresso de um PL
#' @description Extrai o progresso de um PL
#' @param proposicao_df Dataframe da tramitação do PL.
#' @param tramitacao_df Dataframe da proposição do PL. 
#' @param casa Casa (Senado ou Câmara)
#' @return Dataframe contendo id, fase global, data de inicio e data de fim (data atual, se nao houver fim)
get_progresso_both <- function(origem_id, revisao_id, casa_origem){
  casa_revisora <- dplyr::if_else(stringr::str_detect(tolower('senado federal'), tolower(casa_origem)), 'camara', 'senado')
  
  tram_origem <- 
    fetch_tramitacao(origem_id, casa_origem, T) %>% 
    extract_casas(fetch_proposicao(origem_id, casa_origem, '', '', normalized = T), casa_origem) %>%
    dplyr::mutate(pl_id = origem_id)
  
  tram_destino <- 
    fetch_tramitacao(revisao_id, casa_revisora, T) %>% 
    extract_casas(fetch_proposicao(revisao_id, casa_revisora, '', '', normalized = T), casa_revisora) %>%
    dplyr::mutate(pl_id = revisao_id)
  
  df <- 
    dplyr::bind_rows(tram_origem, tram_destino) %>%
    generate_progresso_df()
  
  return(df)
}

#' @title Extrai as próximas audiências públicas de uma PL na Câmara
#' @description Extrai as próximas audiências públicas de uma PL na Câmara
#' @param initial_date data inicial no formato dd/mm/yyyy
#' @param end_date data final no formato dd/mm/yyyy
#' @param fases_tramitacao_df dataframe da PL preprocessada
#' @return Dataframe com as próximas audiências públicas de uma PL na Câmara
#' @examples
#' get_next_audiencias_publicas_in_camara('01/01/2017', '30/10/2018', process_proposicao(fetch_proposicao(2121442, 'camara', 'Lei do Teto Remuneratório', 'Agenda Nacional'), fetch_tramitacao(2121442, 'camara', T), 'camara'))
#' @export
get_next_audiencias_publicas_in_camara <- function(initial_date, end_date, fases_tramitacao_df){
  num_requerimentos_audiencias_publicas <- 
    extract_num_requerimento_audiencia_publica_in_camara(fases_tramitacao_df)
  
  next_audiencias_publicas_by_orgao <- 
    fetch_audiencias_publicas_by_orgao_camara(initial_date, end_date, fases_tramitacao_df)
  
  if(nrow(next_audiencias_publicas_by_orgao) > 0){
    next_audiencias_publicas_pl <-
      next_audiencias_publicas_by_orgao %>% 
      dplyr::mutate(
        num_requerimento = dplyr::if_else(
          stringr::str_extract_all(
            objeto, camara_env$num_requerimento$regex) != 'character(0)', 
          stringr::str_extract_all(objeto, camara_env$num_requerimento$regex), 
          list(0))) %>%  
      tidyr::unnest() %>% 
      dplyr::filter(
        num_requerimento %in% 
          num_requerimentos_audiencias_publicas$num_requerimento)
    
    if(nrow(next_audiencias_publicas_pl) > 0){
      
      next_audiencias_publicas_pl <-
        merge(next_audiencias_publicas_pl, 
              num_requerimentos_audiencias_publicas %>% 
                dplyr::select(prop_id, casa, num_requerimento), 
              by = 'num_requerimento')
      
      next_audiencias_publicas_pl <-
        next_audiencias_publicas_pl %>% 
        dplyr::select(-num_requerimento) %>% 
        dplyr::group_by(data) %>% 
        dplyr::distinct()
      
    } else {
      next_audiencias_publicas_pl <- 
        tibble::frame_data(~ comissao, ~ cod_reuniao, ~ num_reuniao, ~ data, ~ hora, ~ local, 
                   ~ estado, ~ tipo, ~ titulo_reuniao, ~ objeto, ~ proposicoes,
                   ~prop_id, ~casa)
    }
    
  } else {
    next_audiencias_publicas <- 
      tibble::frame_data(~ comissao, ~ cod_reuniao, ~ num_reuniao, ~ data, ~ hora, ~ local, 
                 ~ estado, ~ tipo, ~ titulo_reuniao, ~ objeto, ~ proposicoes,
                 ~prop_id, ~casa)
  }
  return(next_audiencias_publicas_pl)
}
