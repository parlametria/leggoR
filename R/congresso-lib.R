congresso_env <- jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))

#' @title Verfica se um elemento está contido em um vetor
#' @description Verfica se um elemento está contido em um vetor
#' @param element Elemento que pode estar contido
#' @param set Vetor de elementos
#' @return Valor booleano que indica se o elemento está contido no vetor.
#' @export
detect_fase <- function(element, set) {
  element %in% set
}

#' @title Extrai as casas globais de um PL
#' @description Extrai as casas globais de um PL
#' @param full_proposicao_df Dataframe da proposição do PL.
#' @param full_tramitacao_df Dataframe da tramitação do PL.
#' @param sigla Sigla da proposição
#' @return Dataframe com uma nova coluna chamada fase_global
extract_casas <- function(full_proposicao_df, full_tramitacao_df, sigla){
  if (tolower(sigla) == 'pec') {
    eventos_fases <- congresso_env$eventos_fases_pec
  }else {
    eventos_fases <- congresso_env$eventos_fases
  }

  full_ordered_tram <- full_tramitacao_df %>% 
    dplyr::mutate(data = as.Date(data_hora, "UTC -3")) %>% 
    dplyr::arrange(data, sequencia) %>% 
    .corrige_evento_inicial_senado()

  #number delimiting events
  full_ordered_tram <- full_ordered_tram %>%
    dplyr::group_by(evento) %>%
    dplyr::mutate(evento_num = dplyr::if_else(evento %in% c('apresentacao_pl','virada_de_casa','remetida_a_sancao_promulgacao'),
                                              paste0(evento,dplyr::row_number()),
                                              '')) %>%
    dplyr::ungroup() %>% 
    .corrige_evento_apresentacao_duplicado()

  #label first event when happened before the presentation
  if (full_ordered_tram[1,"evento_num"] != 'apresentacao_pl1')
    full_ordered_tram[1,"evento_num"] <- 'primeiro_evento'

  delimiting_events <- full_ordered_tram %>% dplyr::filter(evento_num != '') %>%
    dplyr::left_join(eventos_fases, by = 'evento_num')

  camara_ordered_tram <- tibble::tibble()
  senado_ordered_tram <- tibble::tibble()

  if ('camara' %in% full_ordered_tram$casa) {
    camara_ordered_tram <- full_ordered_tram %>%
      dplyr::filter(casa == 'camara') %>%
      dplyr::left_join(eventos_fases, by = 'evento_num') %>%
      tidyr::fill(fase_global) %>%
      dplyr::bind_rows(delimiting_events) %>%
      dplyr::arrange(data, sequencia) %>%
      dplyr::distinct() %>%
      tidyr::fill(fase_global, .direction = 'up') %>%
      dplyr::filter(casa == 'camara') %>%
      extract_local_global_in_camara() %>%
      dplyr::group_by(fase_global) %>%
      tidyr::fill(local) %>%
      dplyr::ungroup()
  }


  if ('senado' %in% full_ordered_tram$casa) {
    senado_ordered_tram <- full_ordered_tram %>%
      dplyr::filter(casa == 'senado') %>%
      dplyr::left_join(eventos_fases, by = 'evento_num') %>%
      tidyr::fill(fase_global) %>%
      dplyr::bind_rows(delimiting_events) %>%
      dplyr::arrange(data, sequencia) %>%
      dplyr::distinct() %>%
      tidyr::fill(fase_global, .direction = 'up') %>%
      dplyr::filter(casa == 'senado') %>%
      extract_local_global_in_senado() %>%
      dplyr::group_by(fase_global) %>%
      tidyr::fill(local) %>%
      dplyr::ungroup()
  }

  full_ordered_tram_fases <- dplyr::bind_rows(camara_ordered_tram,senado_ordered_tram) %>%
    dplyr::distinct() %>%
    dplyr::arrange(data, sequencia) %>%
    dplyr::select(-data) %>% 
    dplyr::mutate(local_casa = dplyr::if_else(fase_global == 'Sanção/Veto' | fase_global == 'Promulgação/Veto','presidência da república',
                                              dplyr::if_else(fase_global == 'Avaliação dos Vetos','congresso',casa)))
}

#' @title Recupera o progresso de um PL
#' @description Retorna um dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @param tramitacao_df Dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @param sigla Sigla da proposição (Ex: PL, PEC, etc)
#' @param flag_cong_remoto TRUE se o filtro do congresso remoto deve estar ativo ou não. O filtro
#' do congresso remoto remove eventos da tramitação que ocorreram em comissões durante o período em
#' que todas as deliberações não ocorreram em comissões remotas.
#' @return Dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @examples
#'  generate_progresso_df(tramitacao_df)
generate_progresso_df <- function(tramitacao_df, sigla, flag_cong_remoto = TRUE) {
  
  if (flag_cong_remoto) {
    tramitacao_df<- tramitacao_df %>% 
      .remove_eventos_comissao_cong_remoto()
  }
  
  df <-
    tramitacao_df %>%
    dplyr::arrange(data_hora, fase_global)  %>%
    dplyr::filter(!is.na(fase_global)) %>%
    dplyr::mutate(end_data = dplyr::lead(data_hora)) %>%
    dplyr::group_by(
      casa, prop_id, fase_global, local, sequence = data.table::rleid(fase_global)) %>%
    dplyr::summarise(
      data_inicio = min(data_hora, na.rm = T),
      data_fim = max(end_data, na.rm = (sum(is.na(end_data)) != dplyr::n()))) %>%
    dplyr::select(-sequence) %>%
    dplyr::filter(!is.na(local)) %>%
    dplyr::group_by(fase_global) %>%
    dplyr::mutate(data_fim_anterior = dplyr::lag(data_fim)) %>%
    dplyr::mutate(data_fim_proc = as.Date(data_fim, "UTC -3"),
                  data_fim_anterior_proc = as.Date(data_fim_anterior, "UTC -3")) %>% 
    dplyr::filter(is.na(data_fim_anterior) | data_fim_proc >= data_fim_anterior_proc) %>%
    dplyr::select(-data_fim_anterior, -data_fim_proc, -data_fim_anterior_proc) %>%
    dplyr::arrange(data_inicio)
  
  df <- df %>% 
    .altera_data_inicio_plenario_pos_comissao(tramitacao_df)
  
  if (nrow(df %>% dplyr::group_by(fase_global, local) %>% dplyr::filter(dplyr::n() > 1)) > 0) {
    df %<>%
      dplyr::group_by(prop_id, casa, fase_global, local) %>%
      dplyr::summarise(data_inicio = min(data_inicio),
                       data_fim = max(data_fim)) %>%
      dplyr::arrange(data_inicio)
  }

  df$data_fim[nrow(df)] <- NA

  df <- df %>% 
    .padroniza_fases_globais_tramitacao(sigla)
  
  if (sum(is.na(df$casa)) == nrow(df)) {
    tramitacao_df <-
      tramitacao_df %>%
      dplyr::select(prop_id, casa) %>%
      head(1)
    df <-
      df %>%
      dplyr::mutate(casa = tramitacao_df$casa) %>%
      dplyr::mutate(prop_id = tramitacao_df$prop_id)
  }

  #Adding correct casa column value for phases: Sanção/Veto and Avaliação dos Vetos.
  df <- df %>%
    dplyr::mutate(local_casa = dplyr::if_else(fase_global %in% c('Sanção/Veto','Avaliação dos Vetos', 'Promulgação/Veto'),
                                             tolower(local),
                                             casa)) %>% 
    .corrige_data_inicial_camara(tramitacao_df = tramitacao_df)

  return(df)
}

#' @title Retorna um progresso de uma mpv
#' @description Retorna um dataframe contendo os dados sobre o progresso de uma mpv
#' @param tramitacao_df Dataframe processessado da tramitação
#' @return Dataframe contendo o progresso
#' @export
generate_progresso_df_mpv <- function(tramitacao_df) {
  tramitacao_df <-
    tramitacao_df %>%
    dplyr::filter(casa == 'senado') %>% 
    dplyr::arrange(data_hora) %>%
    dplyr::mutate(fase_global =
                    dplyr::case_when(
                      destino_tramitacao_local_nome_casa_local == "Câmara dos Deputados" ~ destino_tramitacao_local_nome_casa_local,
                      stringr::str_detect(tolower(texto_tramitacao), "encaminhad(.) ao senado federal") ~ "Senado Federal",
                      stringr::str_detect(tolower(texto_tramitacao), "sancionada") ~ "Sanção Presidencial/Promulgação",
                      dplyr::row_number() == 1 ~ "Comissão Mista")) %>%
    tidyr::fill(fase_global) %>% 
    .corrige_eventos_mpv_cong_remoto()

  df <-
    tramitacao_df %>%
    dplyr::mutate(end_data = dplyr::lead(data_hora)) %>%
    dplyr::group_by(
      casa, prop_id, fase_global, sequence = data.table::rleid(fase_global)) %>%
    dplyr::summarise(
      data_inicio = min(data_hora, na.rm = T),
      data_fim = max(end_data)) %>%
    dplyr::select(-sequence) %>%
    dplyr::group_by(fase_global) %>%
    dplyr::arrange(data_inicio)

  df <- df %>%
    dplyr::group_by(fase_global) %>%
    dplyr::mutate(Index=1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fase_global = dplyr::if_else(Index == 2, "Câmara dos Deputados - Revisão", fase_global)) %>%
    dplyr::select(-Index)

  if (nrow(df) == 1) {
    df <-
      df %>%
      dplyr::mutate(data_fim = NA)
  }

  df <-
    df %>%
    dplyr::right_join(congresso_env$fases_global_mpv, by = c("fase_global")) %>%
    dplyr::ungroup()

  df %>%
    tidyr::fill(casa, prop_id, .direction = "downup") %>%
    unique() %>%
    dplyr::mutate(data_fim =
                    dplyr::if_else(fase_global == "Sanção Presidencial/Promulgação",
                                   data_inicio,
                                   as.POSIXct(data_fim))) %>%
    dplyr::arrange(data_inicio)
}

#' @title Recupera o número de linha em que houve virada_de_casa
#' @description Recupera o número da linha em que houve evento virada_de_casa
#' @param df Dataframe da tramitação processada da proposiçao
#' @return número da ultima linha cujo evento é virada_de_casa
#' @examples
#'  get_linha_virada_de_casa(fetch_tramitacao(2121442, 'camara', T) %>% extract_events_in_camara())
get_linha_virada_de_casa <- function(proc_tram_df) {
  linha_virada_de_casa = which(proc_tram_df$evento == 'virada_de_casa')
  return(ifelse(length(linha_virada_de_casa) == 0, nrow(proc_tram_df), linha_virada_de_casa))
}

#' @title Recupera o número de linha em que houve remetida_a_sancao_promulgacao
#' @description Recupera o número da linha em que houve evento remetida_a_sancao_promulgacao
#' @param df Dataframe da tramitação processada da proposiçao
#' @return número da ultima linha cujo evento é remetida_a_sancao_promulgacao
#' @examples
#'  get_linha_remetida_a_sancao_promulgacao(fetch_tramitacao(2251392, 'camara', F) %>% extract_events_in_camara())
get_linha_remetida_a_sancao_promulgacao <- function(proc_tram_df) {
  linha_virada_de_casa = which(proc_tram_df$evento == 'remetida_a_sancao_promulgacao')
  return(ifelse(length(linha_virada_de_casa) == 0, nrow(proc_tram_df), linha_virada_de_casa))
}

#' @title Recupera o número de linha em que houve evento vetada_totalmente ou transformada_lei
#' @description Recupera o número da linha em que houve evento vetada_totalmente ou transformada_lei
#' @param full_tramitacao_df Dataframe da tramitação processada da proposiçao
#' @return Dataframe de tramitação com o evento de tramitação 
#' @examples
#'  get_linha_finalizacao_tramitacao(fetch_tramitacao(2121442, 'camara', T) %>% extract_events_in_camara())
get_linha_finalizacao_tramitacao <- function(proc_tram_df) {
  linha_vetada = which(proc_tram_df$evento == 'vetada_totalmente')
  linha_lei = which(proc_tram_df$evento == 'transformada_lei')
  return(ifelse(length(linha_vetada) == 0, ifelse(length(linha_lei) == 0, nrow(proc_tram_df), linha_lei), linha_vetada))
}

#' @title Determina evento inicial de apresentação da Proposição no Senado. Caso não exista evento de apresentação
#' da PL no senado, considera a inclusão na ordem do dia como evento de apresentação.
#' @description Determina evento inicial de apresentação da PL no Senado.
#' @param tramitacao_df Dataframe da tramitação processada da proposiçao
#' @return Dataframe da tramitação com o evento inicial corrigido no Senado.
#' @examples
#'  .corrige_evento_inicial_senado(tramitacao_df)
.corrige_evento_inicial_senado <- function(tramitacao_df) {
  
  filtro_evento_apresentacao_pl <- tramitacao_df %>% 
    dplyr::filter(casa == "senado", 
                  evento == "apresentacao_pl")
  
  # Trata caso especial de apresentação no Senado
  if (filtro_evento_apresentacao_pl %>% nrow() == 0) {
    
    tramitacao_df <- tramitacao_df %>% 
      dplyr::group_by(evento) %>% 
      dplyr::mutate(n_row_event = dplyr::if_else(evento == "incluida_ordem_dia", 
                                                 dplyr::row_number(), 
                                                 NA_integer_)) %>% 
      dplyr::ungroup() %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(evento = dplyr::if_else(evento == "incluida_ordem_dia" && n_row_event == 1,
                                            "apresentacao_pl",
                                            evento)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-n_row_event)
    
    return(tramitacao_df)
  }
  
  return(tramitacao_df)
}

#' @title Elimina segundo evento de apresentação da proposição dentro de uma mesma fase global
#' @description Remove o segundo evento de apresentação da proposição dentro de uma mesma fase global
#' @param tramitacao_df Dataframe da tramitação processada da proposiçao
#' @return Dataframe da tramitação com evento de apresentações corrigidos
#' @examples
#'  .corrige_evento_apresentacao_duplicado(tramitacao_df)
.corrige_evento_apresentacao_duplicado <- function(tramitacao_df) {
  casa_apresentacao_2 <- tramitacao_df %>% 
    dplyr::filter(evento_num == "apresentacao_pl2") %>% 
    dplyr::pull(casa)
  
  casa_apresentacao_3 <- tramitacao_df %>% 
    dplyr::filter(evento_num == "apresentacao_pl3") %>% 
    dplyr::pull(casa)
  
  if (length(casa_apresentacao_2) != 0 & length(casa_apresentacao_3) != 0) {
    if (casa_apresentacao_2 == casa_apresentacao_3) {
      tramitacao_df <- tramitacao_df %>% 
        dplyr::filter(evento_num != "apresentacao_pl3")
    }
  }
  
  return(tramitacao_df)
}

#' @title Corrige data inicial de tramitação no plenário da Câmara
#' @description A partir do evento de aprovação unânime do requerimento de urgência verbal do Presidente,
#' corrige data inicial de tramitação em plenário. 
#' @param df Dataframe com o formato do progresso
#' @param tramitacao_df Dataframe da tramitação processada da proposiçao
#' @return Dataframe de progresso com data inicial de plenário da Câmara corrigida
#' @examples
#'  .corrige_data_inicial_camara(df, tramitacao_df)
.corrige_data_inicial_camara <- function(df, tramitacao_df) {
  
  if ("evento" %in% names(tramitacao_df)) {
    evento_req_urgencia_verbal <- tramitacao_df %>% 
      dplyr::filter(casa == "camara") %>% 
      dplyr::filter(stringr::str_detect(evento, "req_urgencia_unanime_verbal"))
    
    if (evento_req_urgencia_verbal %>% nrow() > 0) {
      df <- df %>% 
        dplyr::mutate(data_inicio = dplyr::if_else(casa == "camara" & local == "Plenário",
                                              evento_req_urgencia_verbal$data_hora,
                                              data_inicio))
    }
  }
  return(df)
}

#' @title Ignora eventos ocorridos em Comissões durante o período do Congresso Remoto (Pandemia Covid-19).
#' @description Com a não instalação/utilização de comissões permanentes durante o período do Congresso Remoto esta função
#' ignora do dataframe de tramitações os eventos com local classificado como Comissões.
#' @param tramitacao_df Dataframe da tramitação processada da proposiçao.
#' @return Dataframe da tramitação com os eventos de comissão ignorados.
#' @examples
#'  .remove_eventos_comissao_cong_remoto(tramitacao_df)
.remove_eventos_comissao_cong_remoto <- function(tramitacao_df) {
  cong_remoto_inicio <- congresso_env$congresso_remoto$data_inicio
  
  tramitacao_df <- tramitacao_df %>% 
    dplyr::mutate(data = as.Date(data_hora, "UTC -3")) %>% 
    dplyr::mutate(fase_comissoes_remoto = dplyr::if_else(data > congresso_env$congresso_remoto$data_inicio,
                                                         dplyr::if_else(!is.na(local) & local == "Comissões",
                                                                        TRUE,
                                                                        FALSE),
                                                         FALSE)) %>% 
    dplyr::filter(!fase_comissoes_remoto) %>% 
    dplyr::select(-data, -fase_comissoes_remoto)
  
  return(tramitacao_df)
}

#' @title Corrige o local de tramitação de eventos durante o período do Congresso Remoto (Pandemia Covid-19)
#' @description Com a não instalação/utilização de comissões permanentes durante o período do Congresso Remoto esta função
#' corrige o local de tramitação de eventos durante o período do Congresso Remoto (ignorando comissões).
#' @param tramitacao_df Dataframe da tramitação processada da proposiçao com a coluna fase_global
#' @return Dataframe da tramitação com a correção dos eventos de comissão na etapa de fase_global
#' @examples
#'  .corrige_eventos_mpv_cong_remoto(tramitacao_df)
.corrige_eventos_mpv_cong_remoto <- function(tramitacao_df) {
  cong_remoto_inicio <- congresso_env$congresso_remoto$data_inicio
  
  tramitacao_df <- tramitacao_df %>% 
    dplyr::mutate(data = as.Date(data_hora, "UTC -3")) %>%
    dplyr::mutate(fase_global = dplyr::if_else(data > cong_remoto_inicio,
                                                dplyr::if_else(fase_global == "Comissão Mista",
                                                               "Congresso Nacional",
                                                               fase_global),
                                                fase_global)) %>% 
    dplyr::select(-data)
    
  return(tramitacao_df)
}

#' @title Corrige data inicial de tramitação no plenário da Câmara após a ocorrência da fase de comissões
#' @description A partir dos eventos de alteração de regime e designação de relator, altera a data de início 
#' da tramitação em plenário considerando o primeiro evento após a fase de comissões.
#' @param df Dataframe com o formato do progresso
#' @param tramitacao_df Dataframe da tramitação processada da proposiçao
#' @return Dataframe do progresso com a data inicial de tramitação no plenário da Câmara corrigida
#' @examples
#'  .altera_data_inicio_plenario_pos_comissao(df, tramitacao_df)
.altera_data_inicio_plenario_pos_comissao <- function(df, tramitacao_df) {
  fase_comissoes <- df %>%
    dplyr::filter(casa == "camara",
                  local == "Comissões",
                  !is.na(data_fim))
  
  if (fase_comissoes %>% nrow() > 0) {
    eventos_tramitacao_plenario <- tramitacao_df %>% 
      dplyr::filter(data_hora <= fase_comissoes$data_fim,
                    casa == "camara",
                    local == "Plenário") %>% 
      dplyr::filter(stringr::str_detect(evento, "alteracao_de_regime|designado_relator"))
    
    if (eventos_tramitacao_plenario %>% nrow() == 0) {
      primeiro_evento_plenario_pos_comissao <- tramitacao_df %>% 
        dplyr::filter(data_hora >= fase_comissoes$data_fim,
                      casa == "camara",
                      local == "Plenário") %>% 
        head(1) %>% 
        dplyr::pull(data_hora)
      
      if (length(primeiro_evento_plenario_pos_comissao) != 0) {
        df <- df %>% 
          dplyr::mutate(data_inicio = dplyr::if_else(casa == "camara" & local == "Plenário",
                                                     primeiro_evento_plenario_pos_comissao,
                                                     data_inicio))
      } else {
        df <- df %>% 
          .corrige_fase_plenario_pre_comissoes()
      }
    }
  }
  return(df)
}

#' @title Padroniza as fases globais do dataframe de progresso
#' @description A partir da sigla da proposição, padroniza as fases globais do dataframe de progresso
#' @param df Dataframe com o formato do progresso
#' @return Dataframe do progresso com as fases globais padronizadas segundo a sigla da proposição
#' @examples
#'  .padroniza_fases_globais_tramitacao(df, sigla)
.padroniza_fases_globais_tramitacao <- function(df, sigla) {
  if (tolower(sigla) == "pec") {
    df %<>%
      dplyr::right_join(congresso_env$fases_global_pec, by = c("local", "fase_global")) %>%
      dplyr::ungroup()
  }else {
    df %<>%
      dplyr::right_join(congresso_env$fases_global, by = c("local", "fase_global")) %>%
      dplyr::ungroup()
  }
  return(df)
}

#' @title Ignora fase de plenário caso as comissões ainda não tenham sido encerradas/finalizadas
#' @description Caso haja detecção de eventos de plenário anteriores a fase de comissões (ainda não encerrada), passa
#' a ignorar as datas de plenário uma vez que efetivamente, em termos de progresso, essa fase ainda não iniciou. E 
#' só se iniciará após a fase de comissões.
#' @param df Dataframe com o formato do progresso
#' @return Dataframe do progresso a data da fase de plenário para a Câmara corrigida
#' @examples
#'  .corrige_fase_plenario_pre_comissoes(df)
.corrige_fase_plenario_pre_comissoes <- function(df) {
  data_fim_plenario <- df %>% dplyr::filter(local == "Plenário", casa == "camara") %>% dplyr::pull(data_fim)
  data_fim_comissoes <- df %>% dplyr::filter(local == "Comissões", casa == "camara") %>% dplyr::pull(data_fim)
  
  if ((length(data_fim_plenario) != 0) & (length(data_fim_comissoes) != 0)) {
    if (data_fim_plenario < data_fim_comissoes) {
      df <- df %>%
        dplyr::mutate(
          data_inicio = ifelse(casa == "camara" & local == "Plenário",
                               NA,
                               data_inicio),
          data_fim = ifelse(casa == "camara" & local == "Plenário",
                            NA,
                            data_fim)
        ) %>% 
        dplyr::mutate(data_inicio = as.POSIXct(data_inicio, origin = "1970-01-01"),
                      data_fim = as.POSIXct(data_fim, origin = "1970-01-01"))
    }
  }
  
  return(df)
}
