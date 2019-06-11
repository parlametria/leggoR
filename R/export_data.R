#' @title Baixa dados sobre tramitação
#' @description RBaixa os dados sobre tramitação
#' @param sigla_tipo Sigla da proposição
#' @param id Id da proposição
#' @param casa senado ou camara
#' @return dataframe com a tramitação
get_tram <- function(sigla_tipo, id, casa) {
  if (sigla_tipo == 'mpv') {
    agoradigital::fetch_tramitacao(id, casa, TRUE)
  } else {
    agoradigital::fetch_tramitacao(id, casa)
  }
}

#' @title Baixa dados gerais sobre proposição
#' @description Realiza todas os processamentos necessários para criar
#' a tabela com dados gerais da proposiçaõ
#' @param id Id da proposição
#' @param casa senado ou camara
#' @return lista com os dataframes da proposição e da tramitação
get_prop <- function(id, casa) {
  prop_tram <- list()
  prop <- agoradigital::fetch_proposicao(id, casa)
  tram <- get_tram(tolower(prop$sigla_tipo), id, casa)
  status <- agoradigital::extract_status_tramitacao(id, casa, prop, tram)
  prop <- merge(prop, status, by = "prop_id") %>%
    dplyr::rename(id_ext = prop_id, apelido = apelido_materia)
  list(prop = prop, tram = tram)
}

#' @title Processa a tramitação
#' @description Realiza todas os processamentos necessários para criar
#' a tabela com dados processados da tramitação
#' @param prop Proposição
#' @param id Id da proposição
#' @param casa senado ou camara
#' @param tram df da tramitação
#' @return dataframe com a tramitação processada
get_tram_processada <- function(prop, id, casa, tram) {
  agoradigital::process_proposicao(
    prop, 
    tram, 
    casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))
}

#' @title Histórico da temperatura
#' @description Realiza todas os processamentos necessários para criar
#' a tabela com o histórico de temperatura
#' @param proc_tram Proposição processada
#' @param pautas Tabela das pautas
#' @param extended_prop Proposição processada
#' @return dataframe com o histórico de temperatura
get_historico_temperatura <- function(proc_tram, pautas, extended_prop) {
  agoradigital::get_historico_temperatura_recente(eventos_df = proc_tram, pautas = pautas) %>%
    dplyr::mutate(id_ext = extended_prop$id_ext, casa = extended_prop$casa) %>%
    dplyr::select(id_ext, casa, periodo, temperatura_periodo, temperatura_recente)
}

#' @title Processa a proposição
#' @description Realiza todas os processamentos necessários para criar
#' as tabelas para uma proposição
#' @param id Id da proposição
#' @param casa senado ou camara
#' @return list com os dataframes: proposicao, fases_eventos,
#' hist_temperatura e emendas
process_etapa <- function(id, casa, pautas, proposicoes_df) {
  extended_prop <- 
    proposicoes_df %>% 
    dplyr::filter(casa == casa & id_ext == id)
  
  is_new_prop <- nrow(extended_prop) == 0
  
  if (is_new_prop) {
    prop_tram <- get_prop(id, casa)
    extended_prop <- prop_tram$prop
    tram <- prop_tram$tram
  } else {
    tram <- get_tram(tolower(prop$sigla_tipo), id, casa)
  }
  
  proc_tram <-
    get_tram_processada(extended_prop, id, casa, tram)

  historico_temperatura <-
    get_historico_temperatura(proc_tram, pautas, extended_prop)
  temperatura_value <-
    historico_temperatura %>%
    dplyr::slice(dplyr::n()) %>%
    .$temperatura_recente
  
  if (is_new_prop) {
    extended_prop <-
      extended_prop %>%
      dplyr::mutate(temperatura = temperatura_value)
  }

  emendas <- rcongresso::fetch_emendas(id, casa, extended_prop$sigla_tipo, extended_prop$numero, extended_prop$ano)
  
  if (casa == 'camara') {
    emendas <- 
      emendas %>% 
      dplyr::mutate(inteiro_teor = agoradigital::get_emendas_links(codigo_emenda))
  }

  list(
    proposicao = extended_prop,
    fases_eventos = proc_tram,
    hist_temperatura = historico_temperatura,
    emendas = emendas
    )
}

safe_process_etapa <- purrr::safely(
    process_etapa,
    otherwise =
      list(
        proposicao = tibble::tribble(
          ~ id_ext,
          ~ sigla_tipo,
          ~ numero,
          ~ ano,
          ~ ementa,
          ~ data_apresentacao,
          ~ casa,
          ~ casa_origem,
          ~
            autor_nome,
          ~ autor_uf,
          ~ autor_partido,
          ~ apelido_materia,
          ~ tema,
          ~ regime_tramitacao,
          ~
            forma_apreciacao,
          ~ relator_nome,
          ~ temperatura
        ),
        fases_eventos = tibble::tribble(
          ~ prop_id,
          ~ casa,
          ~ data_hora,
          ~ sequencia,
          ~ texto_tramitacao,
          ~ sigla_local,
          ~ id_situacao,
          ~
            descricao_situacao,
          ~ link_inteiro_teor,
          ~ evento,
          ~ local,
          ~ tipo_documento,
          ~ nivel,
          ~
            titulo_evento
        ),
        hist_temperatura = tibble::tribble(
          ~ id_ext,
          ~ casa,
          ~ periodo,
          ~ temperatura_periodo,
          ~ temperatura_recente
        ),
        emendas = tibble::tribble(
          ~ prop_id,
          ~ codigo_emenda,
          ~ data_apresentacao,
          ~ numero,
          ~ local,
          ~ autor,
          ~ casa,
          ~ tipo_documento
        )
      )
  )


#' @title Adiciona uma coluna para indicar se a proposição pulou alguma fase
#' @description Verifica se a proposição pulou uma fase
#' @param progresso_df DataFrame com o progresso da tramitação
#' @return Dataframe
adiciona_coluna_pulou <- function(progresso_df) {
  progresso_df$fases_faltantes_abaixo = rev(cumsum(is.na(rev(progresso_df$data_inicio))))

  progresso_df %>%
    dplyr::mutate(
      pulou = dplyr::if_else((is.na(data_inicio) & 
                                (fases_faltantes_abaixo < (dplyr::n() - dplyr::row_number() + 1))), T, F)) %>%
    dplyr::select(-fases_faltantes_abaixo)
}

#' @title Adiciona locais
#' @description Adiciona locais faltantes ao progresso
#' @param progresso_df DataFrame com o progresso da tramitação
#' @return Dataframe
adiciona_locais_faltantes_progresso <- function(progresso_df) {
  progresso_df %>%
    dplyr::mutate(local_casa = dplyr::case_when(
      local %in% c('Presidência da República', 'Congresso') ~
        tolower(local),
      (fase_global == "Revisão I" & is.na(local_casa)) ~ ifelse(.$casa[[1]] == 'senado', 'camara', 'senado'),
      fase_global == "Revisão II" ~ .$casa[[1]],
      is.na(local_casa) ~ casa,
      TRUE ~ local_casa))
}

#' @title Adiciona o status da proposição
#' @description Verifica se a proposição esta Ativa, Arquivada ou virou Lei
#' @param tramitacao_df DataFrame com a tramitacao depois de ser processada pelo process_pl
#' @return Dataframe
adiciona_status <- function(tramitacao_df) {
  tramitacao_df %>%
    dplyr::group_by(id_ext, casa) %>%
    dplyr::mutate(status = dplyr::case_when(evento == "arquivamento" ~ "Arquivada",
                                                        evento == "desarquivamento" ~ "Ativa",
                                                        dplyr::lead(evento, order_by = data) == "transformada_lei" ~ "Lei")) %>%
    tidyr::fill(status) %>%
    dplyr::mutate(status = dplyr::case_when(is.na(status) ~ "Ativa",
                  T ~ status))
}

#' @title Calcula o progresso
#' @description Calcula o progresso da proposição
#' @param etapas lista com a proposição, tramitação, emendas e temperatura
#' @return Dataframe
get_progresso_final <- function(etapas) {
  if (tolower(etapas$proposicao$sigla_tipo) == 'mpv') {
    agoradigital::generate_progresso_df_mpv(etapas$fases_eventos) %>% 
      dplyr::mutate(local = "", local_casa = "", pulou = FALSE)
  }else {
    agoradigital::get_progresso(etapas$proposicao, etapas$fases_eventos) %>%
      adiciona_coluna_pulou() %>%
      adiciona_locais_faltantes_progresso()
  } 
}

#' @title Gera etapas
#' @description Gera a lista etapas com todas as tabelas necessárias para o back
#' @param row_num Row number da proposição na tabela
#' @param id_camara Id da proposição na camara
#' @param id_senado Id da proposição no senado
#' @param apelido Apelido da proposição
#' @param tema_pl Tema da proposição
#' @param total_rows número de linhas da tabela com os ids das proposições
#' @return Dataframe
process_pl <- function(row_num, id_camara, id_senado, apelido, tema_pl, total_rows, pautas, proposicoes_df) {
   cat(paste(
     "\n--- Processando",row_num,"/",total_rows,":", apelido, "\ncamara:", id_camara,
     "\nsenado", id_senado, "\n"))

  etapas <- list()
  if (!is.na(id_camara)) {
    etapa_processada <- safe_process_etapa(id_camara, "camara", pautas = pautas, proposicoes_df = proposicoes_df)
    etapas %<>% append(list(etapa_processada$result))
    if (!is.null(etapa_processada$error)) {
      print(etapa_processada$error)  
      return(etapas)
    }
  }
  if (!is.na(id_senado)) {
    etapa_processada <- safe_process_etapa(id_senado, "senado", pautas = pautas, proposicoes_df = proposicoes_df)
    etapas %<>% append(list(etapa_processada$result))
    if (!is.null(etapa_processada$error)) {
      print(etapa_processada$error)  
      return(etapas)
    }
  }
  
  etapas %<>% purrr::pmap(dplyr::bind_rows)
  if (nrow(etapas$proposicao) != 0) {
    etapas[["progresso"]] <- get_progresso_final(etapas)
  }
  etapas$proposicao %<>%
    dplyr::mutate(tema = tema_pl)
  Sys.sleep(5*stats::runif(1))
  return(etapas)
}

#' @title Proposições individuais
#' @description Filtra a tabela geral de ids para uma casa
#' @param pls tabela geral de ids
#' @param casa senado ou camara
#' @return Dataframe
get_proposicoes_individuais <- function(pls, casa) {
  pls %>%
    dplyr::mutate(casa = casa) %>%
    dplyr::select(casa,
                  id_casa = dplyr::if_else(casa == "camara", "id_camara", "id_senado"),
                  apelido,
                  tema) %>%
    dplyr::filter(!is.na(id_casa))
}

#' @title Proposições que não baixaram
#' @description Retorna as proposições que falharam ao baixar
#' @param proposicoes proposições já baixadas
#' @param proposicoes_individuais_a_baixar tabela geral de ids filtrada
#' @param proposicoes_que_nao_baixaram proposições que qinda 
#' @return Dataframe
get_proposicoes_que_nao_baixaram <- function(proposicoes, proposicoes_individuais_a_baixar, proposicoes_que_nao_baixaram) {
  proposicoes_baixadas <-
    proposicoes %>%
    dplyr::select(casa,
                  id_casa = id_ext,
                  apelido,
                  tema)
  
  proposicoes_que_nao_baixaram_temp <- dplyr::anti_join(proposicoes_individuais_a_baixar, proposicoes_baixadas)
  proposicoes_que_nao_baixaram %>%
    dplyr::filter((id_camara %in% proposicoes_que_nao_baixaram_temp$id_casa) |
                    (id_senado %in% proposicoes_que_nao_baixaram_temp$id_casa)) %>%
    dplyr::mutate(row_num = dplyr::row_number())
}

#' @title Exporta dados de proposições
#' @description Exporta para uma pasta CSVs com dados sobre uma lista de proposições.
#' @param pls dataframe com proposições.
#' @param export_path pasta para onde exportar dados.
#' @export
export_data <- function(pls, export_path, proposicoes_df_path) {
  pautas <- tibble::tribble(~data, ~sigla, ~id_ext, ~local, ~casa, ~semana, ~ano)
  
  tryCatch({
    pautas <- readr::read_csv(paste0(export_path, "pautas.csv"))
  },
  error = function(msg) {
  })
  
  res <- list()
  count <- 0
  proposicoes_que_nao_baixaram <- pls
  
  proposicoes_individuais_a_baixar <-
    dplyr::bind_rows(get_proposicoes_individuais(pls, "camara"),
                     get_proposicoes_individuais(pls, "senado"))
  proposicoes_df <- readr::read_csv(proposicoes_df_path)
  while (count < 5 ) {
    cat(paste("\n--- Tentativa ", count + 1,"\n"))
    res <- 
      append(res, proposicoes_que_nao_baixaram %>% purrr::pmap(process_pl, nrow(proposicoes_que_nao_baixaram), pautas = pautas, 
                                                                    proposicoes_df = proposicoes_df))
    proposicoes <-
      purrr::map_df(res, ~ .$proposicao) %>%
      unique()
    
    proposicoes_que_nao_baixaram <- 
      get_proposicoes_que_nao_baixaram(proposicoes, proposicoes_individuais_a_baixar, proposicoes_que_nao_baixaram)

    if(nrow(proposicoes_que_nao_baixaram) == 0) {
      print("Downloaded all propositions! =)")
      break()
    }
    count <- count + 1
  }
  
  if (nrow(proposicoes_que_nao_baixaram) > 0) {
    print("Could not download the following propositions:")
    print(proposicoes_que_nao_baixaram)
  }
  
  
  tramitacoes <-
    purrr::map_df(res, ~ .$fases_eventos) %>%
    dplyr::rename(id_ext = prop_id, data = data_hora) %>%
    adiciona_status() %>% 
    unique()
  tramitacoes_que_viraram_lei <-
    tramitacoes %>%
    dplyr::filter(evento == "transformada_lei") %>% 
    unique()
  tramitacoes <-
    tramitacoes %>%
    dplyr::filter(!(id_ext %in% tramitacoes_que_viraram_lei$id_ext))
  hists_temperatura <- purrr::map_df(res, ~ .$hist_temperatura) %>% 
    unique()
  progressos <-
    purrr::map_df(res, ~ .$progresso) %>%
    dplyr::rename(id_ext = prop_id) %>% 
    unique()
  emendas <-
    purrr::map_df(res, ~ .$emendas) %>%
    dplyr::rename(id_ext = prop_id) %>% 
    unique()
  comissoes <-
    agoradigital::fetch_all_composicao_comissao() %>% 
    dplyr::rename(id_parlamentar = id)

  ## export data to CSVs
  escreve_csvs(proposicoes, export_path, tramitacoes, hists_temperatura, progressos, emendas, comissoes) 
}

#' @title Escreve os csvs
#' @description Escreve todos os csvs necessários
#' @param proposicoes tabela com os dados de proposicoes
#' @param export_path pasta para onde exportar dados.
#' @param tramitacoes tabela com os dados de tramitações
#' @param progressos tabela com os dados de progressos
#' @param emendas tabela com os dados de emendas
#' @param comissoes tabela com os dados de comissoes
escreve_csvs <- function(proposicoes, export_path, tramitacoes, hists_temperatura, progressos, emendas, comissoes) {
  readr::write_csv(proposicoes, paste0(export_path, "/proposicoes.csv"))
  readr::write_csv(tramitacoes, paste0(export_path, "/trams.csv"))
  readr::write_csv(
    hists_temperatura, paste0(export_path, "/hists_temperatura.csv"))
  readr::write_csv(progressos, paste0(export_path, "/progressos.csv"))
  readr::write_csv(emendas, paste0(export_path, "/emendas_raw.csv"))
  readr::write_csv(comissoes, paste0(export_path, "/comissoes.csv"))
}
