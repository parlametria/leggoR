source(here::here("R/senado_analyzer.R"))
source(here::here("R/camara_analyzer.R"))
source(here::here("R/congresso-lib.R"))
source(here::here("R/relatores.R"))

congresso_env <- jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))
congress_constants <- congresso_env$constants

#' @title Processa dados de uma proposição do congresso.
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
    proc_tram_data <-
      process_proposicao_camara_df(
        proposicao_df = proposicao_df, tramitacao_df = tramitacao_df)
    prop_id <- proc_tram_data[1, "prop_id"]
  } else if (tolower(casa) == congress_constants$senado_label) {
    proc_tram_data <-
      process_proposicao_senado_df(
        proposicao_df = proposicao_df, tramitacao_df = tramitacao_df)
    prop_id <- proc_tram_data[1, "prop_id"]
  }

  if (!is.null(proc_tram_data) & !is.null(out_folderpath)) {
    readr::write_csv(
      proc_tram_data,
      paste0(
        out_folderpath, "/", casa, "/", prop_id, "-fases-tramitacao-", casa, ".csv"))
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

  start_date <- pivot_day - lubridate::days(days_ago)

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
  eventos_sem_horario <- eventos_df %>%
    dplyr::mutate(data = lubridate::floor_date(data_hora, unit="day"))

  #Adiciona linhas para os dias úteis nos quais não houve movimentações na tramitação
  #Remove linhas referentes a dias de recesso parlamentar
  full_dates <- data.frame(data = seq(min(eventos_sem_horario$data), max_date, by = "1 day"))
  eventos_extendidos <- merge(full_dates, eventos_sem_horario, by="data", all.x = TRUE) %>%
    filtra_dias_nao_uteis_congresso() %>%
    dplyr::mutate(peso_base = dplyr::if_else(is.na(prop_id),0,1)) %>%
    dplyr::left_join(get_pesos_eventos(), by="evento") %>%
    dplyr::mutate(peso = dplyr::if_else(is.na(peso),0,as.numeric(peso))) %>%
    dplyr::mutate(peso_final = peso_base + peso) %>%
    dplyr::select(-tipo, -label)
    

  energia_periodo <- data.frame()

  #Agrupa eventos por período
  if (granularidade == "d") {
    energia_periodo <- eventos_extendidos %>%
      dplyr::group_by(data)
  } else if (granularidade == "s") {
    energia_periodo <- eventos_extendidos %>%
      dplyr::mutate(semana = lubridate::week(data),
                    ano = lubridate::year(data)) %>%
      dplyr::group_by(ano, semana)
  } else if (granularidade == "m") {
    energia_periodo <- eventos_extendidos %>%
      dplyr::mutate(mes = lubridate::month(data),
                    ano = lubridate::year(data)) %>%
      dplyr::group_by(ano, mes)
  }

  energia_periodo <- energia_periodo %>%
    dplyr::summarize(periodo = dplyr::first(data),
                     energia_periodo = sum(peso_final, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(periodo, energia_periodo) %>%
    dplyr::arrange(periodo)

  #Computa soma deslizante com decaimento exponencial
  tamanho_janela <- nrow(energia_periodo)
  weights <- (1 - decaimento) ^ ((tamanho_janela - 1):0)
  energia_recente <- data.frame(energia_recente = round(roll::roll_sum(data.matrix(energia_periodo$energia_periodo), tamanho_janela, weights, min_obs = 1), digits=2))
  historico_energia <- dplyr::bind_cols(energia_periodo, energia_recente) %>%
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

#' @title Cria uma coluna com o nome pauta
#' @description Extrai se um vectors de proposições estarão em pauta
#' @param agenda Dataframe com a agenda
#' @param tabela_geral_ids_casa Dataframe com as colunas id_senado, id_camara
#' @return Dataframe com a coluna em_pauta
#' @examples
#' extract_pauta(fetch_agenda_geral('2018-10-01', '2018-10-26'), c("91341", "2121442", "115926", "132136"))
#' @export
extract_pauta <- function(agenda, tabela_geral_ids_casa, export_path) {
  proposicao_id <- as.vector(as.matrix(tabela_geral_ids_casa[,c("id_camara", "id_senado")]))
  proposicao_id <- proposicao_id[!is.na(proposicao_id)]
  pautas <-
    agenda %>%
    dplyr::mutate(em_pauta = id_ext %in% proposicao_id) %>%
    dplyr::filter(em_pauta) %>%
    dplyr::mutate(semana = lubridate::week(data),
                  ano = lubridate::year(data)) %>%
    dplyr::arrange(unlist(sigla), semana, desc(em_pauta))
  
  readr::write_csv(pautas, paste0(export_path, "/pautas.csv"))
}

#' @title Extrai o status da tramitação de um PL
#' @description Obtém o status da tramitação de um PL
#' @param tram_df Dataframe da tramitação do PL.
#' @return Dataframe contendo id, regime de tramitação e forma de apreciação do PL
#' @examples
#' extract_status_tramitacao(91341, 'senado')
#' @export
#' @importFrom stats filter
extract_status_tramitacao <- function(proposicao_id, casa) {
  tram_df <- fetch_tramitacao(proposicao_id, casa, TRUE)
  regime <- extract_regime_tramitacao(tram_df)
  apreciacao <- extract_forma_apreciacao(tram_df)
  relator_nome <- extract_relator_name(proposicao_id, casa)

  status_tram <-
      data.frame(
          prop_id = tram_df[1, ]$prop_id,
          regime_tramitacao = regime,
          forma_apreciacao = apreciacao,
          relator_nome = relator_nome
      )
}

#' @title Extrai o progresso de um PL
#' @description Extrai o progresso de um PL
#' @param proposicao_df Dataframe da tramitação do PL.
#' @param tramitacao_df Dataframe da proposição do PL.
#' @param casa Casa (Senado ou Câmara)
#' @param out_folderpath Caminho destino do csv resultante
#' @return Dataframe contendo id, fase global, data de inicio e data de fim (data atual, se nao houver fim)
#' @examples
#' etapas <- list()
#' etapas %<>% append(list(process_etapa(2088990, "camara", fetch_agenda_geral('2018-07-03', '2018-07-10'))))
#' etapas %<>% append(list(process_etapa(91341, "senado", fetch_agenda_geral('2018-07-03', '2018-07-10'))))
#' etapas %<>% purrr::pmap(dplyr::bind_rows)
#' get_progresso(etapas$proposicao, etapas$fases_eventos)
#' @export
get_progresso <- function(proposicao_df, tramitacao_df) {
  progresso_data <-
    tramitacao_df %>%
    agoradigital:::extract_casas(proposicao_df) %>%
    agoradigital:::generate_progresso_df() %>%
    dplyr::mutate(local_casa = casa) %>%
    ## TODO: isso está ruim, deveria usar o id da proposição e não da etapa...
    tidyr::fill(prop_id, casa) %>%
    tidyr::fill(prop_id, casa, .direction = "up") 
  return(progresso_data)
}

#' @title Recupera os eventos e seus respectivos pesos
#' @description Retorna um dataframe com o superconjunto dos eventos das duas casas (Câmara e Senado) e seus respectivos pesos
#' @return Dataframe contendo evento e peso
#' @examples
#' get_pesos_eventos()
#' @export
get_pesos_eventos <- function() {
  eventos_camara <- camara_env$eventos
  eventos_senado <- senado_env$eventos
  tipos_eventos <- congresso_env$tipos_eventos

  eventos_extra_senado <- purrr::map_df(senado_env$evento, ~ dplyr::bind_rows(.x)) %>%
    dplyr::select(evento = constant, tipo)

  pesos_eventos <- dplyr::bind_rows(eventos_camara, eventos_senado, eventos_extra_senado) %>%
    dplyr::group_by(evento) %>%
    dplyr::summarise(tipo = dplyr::first(tipo)) %>% 
    dplyr::left_join(tipos_eventos, by="tipo") %>%
    dplyr::arrange()

  return(pesos_eventos)
}
