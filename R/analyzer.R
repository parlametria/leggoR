source(here::here("R/senado_analyzer.R"))
source(here::here("R/camara_analyzer.R"))
source(here::here("R/congresso-lib.R"))
source(here::here("R/relatorias.R"))
source(here::here("R/tramitacoes.R"))
source(here::here("R/proposicoes.R"))

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

#' @title Retorna temperatura de uma proposição no congresso.
#' @description Recebido o dataframe da tramitação contendo as colunas: data_hora e evento,
#' retorna um valor que indica a temperatura da proposição
#' @param tramitacao_df Dataframe da tramitação contendo as colunas: data_hora e evento
#' @param days_ago Quantidade de dias a serem analizados. O padrão é 30
#' @param pivot_day Dia de partida de onde começará a análise. O padrão é o dia atual
#' @return Temperatura de uma proposição.
#' @importFrom magrittr '%>%'
#' @export
get_temperatura <- function(tramitacao_df, days_ago = 30, pivot_day = lubridate::today()) {
  working_days <- ((days_ago / 7) * 5)

  start_date <- pivot_day - lubridate::days(days_ago)

  qtd_eventos <-
    tramitacao_df %>%
    dplyr::filter(data_hora >= start_date) %>%
    dplyr::filter(!is.na(evento)) %>%
    nrow()

  qtd_eventos / working_days
}

#' @title Retorna o histórico da temperatura de uma proposição no congresso.
#' @description Recebido o dataframe da tramitação contendo as colunas: data_hora e evento,
#' retorna um dataframe com o valor da temperatura recente calculado para cada dia útil da tramitação de uma proposição,
#' computado levando em consideração os eventos que aconteceram nos 30 dias anteriores à data pivô, e aplicando um decaimento exponencial.
#' @param events_df Dataframe da tramitação contendo as colunas: data_hora e evento
#' @param granularidade Granularidade do dado histórico da temperatura desejada ('d' = dia, 's' = semana, 'm' = mês)
#' @param decaimento A porcentagem de redução do valor da temperatura por dia. Valor deve estar entre 0 e 1.
#' @param max_date Último dia a ser considerado no cálculo da temperatura. Padrão: dia atual.
#' @return Dataframe com o valor da temperatura recente para cada dia útil da tramitação de uma proposição.
#' @importFrom magrittr '%>%'
#' @export
#' @examples
#' \dontrun{
#' id <- 345311
#' casa <- 'camara'
#' prop <- agoradigital::fetch_proposicao(id,casa,TRUE)
#' tram <- agoradigital::fetch_tramitacao(id,casa,TRUE)
#' proc_tram <- agoradigital::process_proposicao(prop,tram,casa)
#' get_historico_temperatura_recente(proc_tram, granularidade = 's', decaimento = 0.05)
#' }
get_historico_temperatura_recente <- function(eventos_df, granularidade = 's', decaimento = 0.25, max_date = lubridate::now()) {
  #Remove tempo do timestamp da tramitação
  eventos_sem_horario <- eventos_df %>%
    dplyr::mutate(data = lubridate::floor_date(data_hora, unit="day"))

  #Adiciona linhas para os dias úteis nos quais não houve movimentações na tramitação
  #Remove linhas referentes a dias de recesso parlamentar
  full_dates <- data.frame(data = seq(min(eventos_sem_horario$data), max_date, by = "1 day"))
  pesos_eventos <-
    get_pesos_eventos() %>%
    dplyr::select(-tipo, -label)
  pesos_locais <-
    get_pesos_locais() %>%
    dplyr::select(-tipo, -label) %>%
    dplyr::rename(peso_local = peso)
  eventos_extendidos <- merge(full_dates, eventos_sem_horario, by="data", all.x = TRUE) %>%
    filtra_dias_nao_uteis_congresso() %>%
    dplyr::mutate(peso_base = dplyr::if_else(is.na(prop_id),0,1)) %>%
    dplyr::left_join(pesos_eventos, by="evento") %>%
    dplyr::left_join(pesos_locais, by="local") %>%
    dplyr::mutate(peso_evento = dplyr::if_else(is.na(peso),0,as.numeric(peso))) %>%
    dplyr::mutate(peso_local = dplyr::if_else(is.na(peso_local),0,as.numeric(peso_local))) %>%
    dplyr::mutate(peso_final = peso_base + peso_evento + peso_local)


  temperatura_periodo <- data.frame()

  get_arquivamento <- function(df, colunas) {
    df %>%
      dplyr::filter(evento == "arquivamento") %>%
      dplyr::select(colunas) %>%
      dplyr::mutate(dummy = "Dummy")
  }

  #Agrupa eventos por período
  if (granularidade == "d") {
    temperatura_periodo <- eventos_extendidos %>%
      dplyr::group_by(data)
    data_arquivamento <-
      temperatura_periodo %>%
      get_arquivamento(c("data"))
  } else if (granularidade == "s") {
    temperatura_periodo <- eventos_extendidos %>%
      dplyr::mutate(semana = lubridate::week(data),
                    ano = lubridate::year(data)) %>%
      dplyr::group_by(ano, semana)
    data_arquivamento <-
      temperatura_periodo %>%
      get_arquivamento(c("semana", "ano"))
  } else if (granularidade == "m") {
    temperatura_periodo <- eventos_extendidos %>%
      dplyr::mutate(mes = lubridate::month(data),
                    ano = lubridate::year(data)) %>%
      dplyr::group_by(ano, mes)
    data_arquivamento <-
      temperatura_periodo %>%
      get_arquivamento(c("mes", "ano"))
  }

  temperatura_periodo <-
    temperatura_periodo %>%
    dplyr::summarize(periodo = dplyr::first(data),
                     temperatura_periodo = sum(peso_final, na.rm = T)) %>%
    dplyr::ungroup()
  temperatura_periodo <-
    suppressMessages(dplyr::left_join(temperatura_periodo, data_arquivamento)) %>%
    dplyr::mutate(temperatura_periodo = dplyr::if_else(!is.na(dummy), 0, temperatura_periodo)) %>%
    dplyr::select(periodo, temperatura_periodo) %>%
    dplyr::arrange(periodo)

  #Computa soma deslizante com decaimento exponencial
  tamanho_janela <- nrow(temperatura_periodo)
  weights <- (1 - decaimento) ^ ((tamanho_janela - 1):0)
  temperatura_recente <- data.frame(temperatura_recente = round(roll::roll_sum(data.matrix(temperatura_periodo$temperatura_periodo), tamanho_janela, weights, min_obs = 1), digits=2))
  historico_temperatura <- dplyr::bind_cols(temperatura_periodo, temperatura_recente) %>%
    dplyr::select(periodo,
                  temperatura_periodo,
                  temperatura_recente)

  return(historico_temperatura)
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
                  ano = lubridate::year(data),
                  local = ifelse(is.na(local), "Local não informado", local)) %>%
    dplyr::arrange(unlist(sigla), semana, desc(em_pauta)) %>%
    unique() %>%
    dplyr::group_by(data, sigla) %>%
    dplyr::arrange(data) %>%
    dplyr::filter(row_number()==n()) %>%
    dplyr::ungroup() %>%
    fix_nomes_locais() %>%
    dplyr::select(-em_pauta)

  readr::write_csv(pautas, paste0(export_path, "/pautas.csv"))
}


#' @title Simplifica nomes dos locais das reuniões no dataframe de Pautas
#' @description Simplifica nomes dos locais das reuniões no dataframe de Pautas
#' @param pautas_df Dataframe das pautas de um determinado período de tempo
#' @return Dataframe das pautas com os nomes dos locais simplificados
#' @examples
#' fix_nomes_locais(pauta_df)
fix_nomes_locais <- function(pautas_df) {
  pautas_locais_clean <- pautas_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(local_clean = stringr::str_split(local, ' - ')[[1]][1]) %>%
    dplyr::mutate(local_clean = dplyr::if_else(local_clean == 'Plenário da Câmara dos Deputados' || local_clean == 'PLEN', 'Plenário', local_clean)) %>%
    dplyr::mutate(local_clean = dplyr::if_else(grepl("\\d",local_clean),'Comissão Especial', local_clean)) %>%
    dplyr::mutate(local = local_clean) %>%
    dplyr::ungroup() %>%
    dplyr::select(-local_clean)

  return(pautas_locais_clean)
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
  relator_nome <- get_last_relator_name(proposicao_id, casa)

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
#' @return Datafram
#' e contendo id, fase global, data de inicio e data de fim (data atual, se nao houver fim)
#' @examples
#' etapas <- list()
#' etapas %<>% append(list(process_etapa(1635730, "camara", fetch_agenda_geral('2018-07-03', '2018-07-10'))))
#' etapas %<>% append(list(process_etapa(126084, "senado", fetch_agenda_geral('2018-07-03', '2018-07-10'))))
#' etapas %<>% purrr::pmap(dplyr::bind_rows)
#' get_progresso(etapas$proposicao, etapas$fases_eventos)
#' @export
get_progresso <- function(proposicao_df, tramitacao_df) {
  progresso_data <-
    tramitacao_df %>%
    agoradigital:::extract_casas(proposicao_df) %>%
    agoradigital:::generate_progresso_df() %>%
    dplyr::mutate(local_casa = dplyr::if_else(!is.na(data_inicio) & fase_global == congresso_env$fases_global$fase_global[[7]], 'presidencia', casa)) %>%
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

#' @title Recupera os locais e seus respectivos pesos
#' @description Retorna um dataframe com o superconjunto dos locais das duas casas (Câmara e Senado) e seus respectivos pesos
#' @return Dataframe contendo local e peso
#' @examples
#' get_pesos_locais()
#' @export
get_pesos_locais <- function() {
  locais_camara <- camara_env$locais
  locais_senado <- senado_env$locais
  tipos_locais <- congresso_env$tipos_locais

  pesos_locais <- dplyr::bind_rows(locais_camara, locais_senado) %>%
    dplyr::group_by(local) %>%
    dplyr::summarise(tipo = dplyr::first(tipo)) %>%
    dplyr::left_join(tipos_locais, by="tipo") %>%
    dplyr::arrange()

  return(pesos_locais)
}

#' @title Extrai as próximas audiências públicas de uma PL
#' @description Extrai as próximas audiências públicas de uma PL a
#' @param initial_date data inicial no formato dd/mm/yyyy
#' @param end_date data final no formato dd/mm/yyyy
#' @param fases_tramitacao_df dataframe da PL preprocessada
#' @return Dataframe com as próximas audiências públicas de uma PL
#' @examples
#' get_next_audiencias_publicas('01/01/2017', '30/10/2018', process_proposicao(fetch_proposicao(2121442, 'camara', 'Lei do Teto Remuneratório', 'Agenda Nacional'), fetch_tramitacao(2121442, 'camara', T), 'camara'), casa='camara')
#' @export
get_next_audiencias_publicas <- function(initial_date, end_date, fases_tramitacao_df, casa) {
 next_audiencias_data <- NULL
  if (tolower(casa) == congress_constants$camara_label) {

    next_audiencias_publicas_by_orgao <-
      fetch_audiencias_publicas_by_orgao_camara(
      initial_date,
      end_date,
      fases_tramitacao_df)

    next_audiencias_data <-
      get_next_audiencias_publicas_in_camara(
        initial_date, end_date,
        fases_tramitacao_df,
        next_audiencias_publicas_by_orgao)

  } else if (tolower(casa) == congress_constants$senado_label) {

    # TODO: Adicionar get_next_audiencias_publicas_in_senado()
  }

  return(next_audiencias_data)
}

#' @title Extrai autores do voto em separado
#' @description Retorna um dataframe com a coluna autor_voto_separado
#' @param df Dataframe da tramitação já com os eventos reconhecidos
#' @return Dataframe
#' @examples
#  get_autores_voto_separado(
#  agoradigital::process_proposicao(agoradigital::fetch_proposicao(46249, 'camara'), agoradigital::fetch_tramitacao(46249, 'camara', TRUE), 'camara'))
#' @export
get_autores_voto_separado <- function(df) {
  df %>%
    dplyr::mutate(autor_voto_separado = dplyr::case_when(
      evento == "voto_em_separado" ~
        stringr::str_extract(texto_tramitacao, stringr::regex(camara_env$autor_voto_separado$regex, ignore_case=TRUE))))
}

