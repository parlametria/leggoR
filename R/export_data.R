congresso_env <-
  jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))

#' @title Adiciona uma coluna para indicar se a proposição pulou alguma fase
#' @description Verifica se a proposição pulou uma fase
#' @param progresso_df DataFrame com o progresso da tramitação
#' @return Dataframe
adiciona_coluna_pulou <- function(progresso_df) {
  progresso_df$fases_faltantes_abaixo = rev(cumsum(is.na(rev(
    progresso_df$data_inicio
  ))))

  progresso_df %>%
    dplyr::mutate(pulou = dplyr::if_else((
      is.na(data_inicio) &
        (fases_faltantes_abaixo < (dplyr::n() - dplyr::row_number() + 1))
    ), T, F)) %>%
    dplyr::select(-fases_faltantes_abaixo)
}

#' @title Adiciona coluna para indicar se a MPV pulou de fase ou não.
#' @description Uma MPV pode pular a fase de comissões no período de Congresso Remoto.
#' @param progresso_df DataFrame com o progresso da tramitação da MPV
#' @return Dataframe
adiciona_coluna_pulou_mpv <- function(progresso_df) {
  cong_remoto_inicio <-
    congresso_env$congresso_remoto$data_mudanca_mpvs

  data_fase_camara <- progresso_df %>%
    dplyr::filter(fase_global == "Câmara dos Deputados") %>%
    dplyr::pull(data_inicio)

  data_inicio_comissao_mista <- progresso_df %>%
    dplyr::filter(fase_global == "Comissão Mista") %>%
    dplyr::pull(data_inicio)

  # Uma MPV pula a fase de comissão mista quando a fase de início do Plenário tiver
  # ocorrido durante a pandemia
  if (length(data_fase_camara) > 0 & !is.na(data_fase_camara)) {
    if (data_fase_camara > cong_remoto_inicio) {
      progresso_df <- progresso_df %>%
        dplyr::mutate(
          pulou = dplyr::case_when(
            fase_global == "Comissão Mista" &
              (
                data_fase_camara >= cong_remoto_inicio &
                  data_inicio >= cong_remoto_inicio
              ) ~ TRUE,
            fase_global == "Comissão Mista" &
              is.na(data_inicio) ~ TRUE,
            T ~ FALSE
          )
        )
    }
  }

  return(progresso_df)
}

#' @title Adiciona locais
#' @description Adiciona locais faltantes ao progresso
#' @param progresso_df DataFrame com o progresso da tramitação
#' @return Dataframe
adiciona_locais_faltantes_progresso <- function(progresso_df) {
  progresso_df %>%
    dplyr::mutate(
      local_casa = dplyr::case_when(
        local %in% c('Presidência da República', 'Congresso') ~
          tolower(local),
        (fase_global == "Revisão I" &
           is.na(local_casa)) ~ ifelse(.$casa[[1]] == 'senado', 'camara', 'senado'),
        fase_global == "Revisão II" ~ .$casa[[1]],
        is.na(local_casa) ~ casa,
        TRUE ~ local_casa
      )
    )
}

#' @title Retorna o progresso baseado do tipo da proposição
#' @description Retorna o progresso dependendo do tipo da proposição.
#' Para MPVs, ele é gerado diferente.
#' @param proposicao Dataframe contendo os metadados de proposição
#' @param fases_eventos Dataframe com a tramitação já processada
#' @return Dataframe de progresso
.get_progresso_etapas <- function(proposicao, fases_eventos) {
  sigla <- tolower(proposicao$sigla_tipo)
  if (length(sigla) > 1) {
    sigla <- sigla[1]
  }

  ## Processa dados do progresso
  if (sigla == 'mpv') {
    progresso <-
      agoradigital::generate_progresso_df_mpv(fases_eventos, proposicao) %>%
      dplyr::mutate(local = "", local_casa = "") %>%
      adiciona_coluna_pulou_mpv()

  } else {
    progresso <-
      agoradigital::get_progresso(proposicao, fases_eventos) %>%
      adiciona_coluna_pulou() %>%
      adiciona_locais_faltantes_progresso()
  }

  return(progresso)
}

#' @title Adiciona o status da proposição
#' @description Verifica se a proposição esta Ativa, Arquivada ou virou Lei
#' @param tramitacao_df DataFrame com a tramitacao depois de ser processada pelo process_pl
#' @return Dataframe
#' @export
adiciona_status <- function(tramitacao_df) {
  tramitacao_df %>%
    dplyr::group_by(id_ext, casa) %>%
    dplyr::mutate(evento = dplyr::case_when(
      descricao_situacao == "TRANSFORMADA EM NORMA JURÍDICA" ~ "transformada_em_norma_juridica",
      TRUE ~ evento
      )
    ) %>%
    dplyr::mutate(
      status = dplyr::case_when(
        evento == "arquivamento" ~ "Arquivada",
        evento == "desarquivamento" ~ "Ativa",
        evento == "perda_da_eficacia" ~ "Caducou",
        evento == "rejeicao_projeto" ~ "Rejeitada",
        evento == "transformada_lei" ~ "Lei",
        evento == "transformada_em_norma_juridica" ~ "Aprovada"
      )
    ) %>%
    tidyr::fill(status, .direction = "up") %>%
    tidyr::fill(status, .direction = "down") %>%
    dplyr::mutate(status = dplyr::case_when(is.na(status) ~ "Ativa",
                                            T ~ status))
}

#' @title Extrai a última data de tramitação
#' @description Recebe um dataframe e retorna a última data de uma proposição, independente da
#' casa
#' @param tramitacao_df DataFrame com a tramitacao 
#' @param id_camara ID da Câmara
#' @param id_senado ID do Senado
#' @return Data da última tramitação, se existir. Caso contrário, retorna NULL.
#' @export
.get_data_ultima_tramitacao <- function(tramitacoes, id_camara, id_senado) {
  ultima_tramitacao_data <- NULL
  if (!is.null(tramitacoes)) {
    ultima_tramitacao_data_df <- tramitacoes %>%
      dplyr::filter((casa == "camara" &
                       id_ext == id_camara) |
                      (casa == "senado" & id_ext == id_senado)) %>%
      dplyr::distinct(data)
    if (nrow(ultima_tramitacao_data_df) > 0) {
      ultima_tramitacao_data <- ultima_tramitacao_data_df %>%
        dplyr::arrange(dplyr::desc(data)) %>%
        head(1) %>%
        dplyr::pull(data) %>%
        as.Date() %>%
        as.character()
    }
  }
  
  return(ultima_tramitacao_data)
}
#' @title Gera etapas
#' @description Gera a lista etapas com todas as tabelas necessárias para o back
#' @param row_num Row number da proposição na tabela
#' @param id_camara Id da proposição na camara
#' @param id_senado Id da proposição no senado
#' @param total_rows número de linhas da tabela com os ids das proposições
#' @return Dataframe
process_pl <- function(row_num,
                       id_camara,
                       id_senado,
                       total_rows,
                       pautas,
                       proposicoes = NULL,
                       tramitacoes = NULL,
                       progressos = NULL,
                       locais_atuais = NULL,
                       sleep_time = .DEF_REQ_SLEEP_TIME_IN_SECS) {
  
    # Sys.sleep(sleep_time)
    cat(
      paste(
        "\n\n--- Processando",
        row_num,
        "/",
        total_rows,
        "\ncamara:",
        id_camara,
        "\nsenado:",
        id_senado,
        "\n"
      )
    )
    
    etapas <- list()
    houve_modificacao <- TRUE
    ultima_tramitacao_data <-
      .get_data_ultima_tramitacao(tramitacoes, id_camara, id_senado)
    id_leggo_key = digest::digest(paste0(id_camara , " ", id_senado),
                                  algo = "md5",
                                  serialize = F)

    if (!is.na(id_camara)) {
      etapa_processada_camara <- process_etapa_otimizada(
        id_camara,
        "camara",
        pautas = pautas,
        proposicoes = proposicoes,
        tramitacoes = tramitacoes,
        ultima_tramitacao_data = ultima_tramitacao_data,
        return_modified_tag = T,
        houve_modificacao = houve_modificacao
      )
      
      houve_modificacao <-
        .get_foi_modificada(etapa_processada_camara, houve_modificacao)
      
      if(!is.null(etapa_processada_camara$result$foi_modificada)) {
        etapa_processada_camara$result$foi_modificada <- NULL
      }
      
      if (!is.null(etapa_processada_camara$error)) {
        print(etapa_processada_camara$error)
        return(etapa_processada_camara$result)
      }
    
      etapas %<>% append(list(etapa_processada_camara$result))
      
    } else {
      houve_modificacao <- FALSE
    }
    
    if (!is.na(id_senado)) {

      etapa_processada_senado <- process_etapa_otimizada(
        id_senado,
        "senado",
        pautas = pautas,
        proposicoes = proposicoes,
        tramitacoes = tramitacoes,
        ultima_tramitacao_data = ultima_tramitacao_data,
        return_modified_tag = T,
        houve_modificacao = houve_modificacao
      )
      
      houve_modificacao <-
        .get_foi_modificada(etapa_processada_senado, houve_modificacao)
      
      if(!is.null(etapa_processada_senado$result$foi_modificada)) {
        etapa_processada_senado$result$foi_modificada <- NULL
      }
      
      if (!is.null(etapa_processada_senado$error)) {
        print(etapa_processada_senado$error)
        return(etapa_processada_senado$result)
      }
      
      etapas %<>% append(list(etapa_processada_senado$result))
    }

    if (is.na(id_camara) & is.na(id_senado)) {
      message("A proposição não foi processada!")
      print("A proposição está preenchida com o id_camara e o id_senado como NA.")
      return(etapas)
    }

    etapas %<>% purrr::pmap(dplyr::bind_rows)
    
    if (is.null(etapas$proposicao) || nrow(etapas$proposicao) == 0) {
      return(etapas)
    }
    
    etapas$tramitacoes_etapas_atuais <-
      .filter_tramitacoes_etapa_atual(etapas$fases_eventos,
                                      etapas$proposicao)
    
    if (houve_modificacao) {
      ## Processa dados de Progresso
      etapas[["progresso"]] <-
        .get_progresso_etapas(etapas$proposicao, etapas$tramitacoes_etapas_atuais)
      
      ## Adiciona id_leggo às proposições
      etapas$proposicao <- etapas$proposicao %>%
        dplyr::mutate(id_leggo = id_leggo_key)
      
      ## Processa dados de Locais Atuais da proposição
      etapas[["local_atual"]] <-
        agoradigital::processa_local_atual(proposicao_df = etapas$proposicao,
                                           id_leggo = id_leggo_key)
      
      Sys.sleep(5 * stats::runif(1))
      
    } else {
      if (!is.null(progressos)) {
        etapas[["progresso"]] <- progressos %>%
          dplyr::filter((casa == 'camara' &
                           id_ext == id_camara) |
                          (casa == "senado" &
                             id_ext == id_senado)) %>%
          dplyr::rename(prop_id = id_ext)
      }
      
      if (!is.null(locais_atuais)) {
        etapas[["local_atual"]] <- locais_atuais %>%
          dplyr::filter(id_leggo == id_leggo_key)
      }
    }
    
    ## Processa dados de temperatura
    etapas[["hist_temperatura"]] <-
      agoradigital::get_historico_temperatura_recente_id_leggo(
        tram = etapas$tramitacoes_etapas_atuais,
        id_leggo = id_leggo_key,
        pautas = pautas
      )
    
    return(etapas)
  }

#' @title Formata tabela de proposições
#' @description Recebi um dataframe com id_camara, id_senado
#' casa, apelido e tema e formata
#' @param pls dataframe com proposições.
#' @export
converte_tabela_geral_ids_casa <- function(pls) {
  pls_ids <- pls %>%
    dplyr::rowwise(.) %>%
    dplyr::mutate(concat_chave_leggo = paste0(id_camara, " ", id_senado)) %>%
    dplyr::mutate(id_leggo = digest::digest(concat_chave_leggo, algo="md5", serialize=F)) %>%
    dplyr::select(-concat_chave_leggo)

  proposicoes_individuais_a_baixar_camara <-
    pls_ids %>%
    dplyr::mutate(casa = "camara") %>%
    dplyr::select(id_leggo,
                  casa,
                  id_casa = id_camara) %>%
    dplyr::filter(!is.na(id_casa))

  proposicoes_individuais_a_baixar_senado <-
    pls_ids %>%
    dplyr::mutate(casa = "senado") %>%
    dplyr::select(id_leggo,
                  casa,
                  id_casa = id_senado) %>%
    dplyr::filter(!is.na(id_casa))

  dplyr::bind_rows(
    proposicoes_individuais_a_baixar_camara,
    proposicoes_individuais_a_baixar_senado
  )
}

#' @title Exporta dados de proposições
#' @description Exporta para uma pasta CSVs com dados sobre uma lista de proposições.
#' @param pls dataframe com proposições.
#' @param export_path pasta para onde exportar dados.
#' @export
fetch_props <- function(pls, export_path) {
  pautas <-
    tibble::tribble(~ data, ~ sigla, ~ id_ext, ~ local, ~ casa, ~ semana, ~
                      ano)

  tryCatch({
    pautas <- readr::read_csv(paste0(export_path, "pautas.csv"))
  },
  error = function(msg) {

  })

  props_filepath = paste0(export_path, "proposicoes.csv")
  trams_filepath = paste0(export_path, "trams.csv")
  temps_filepath = paste0(export_path, "hists_temperatura.csv")
  progs_filepath = paste0(export_path, "progressos.csv")
  locais_props_filepath = paste0(export_path, "props_locais_atuais.csv")

  parlamentares <- agoradigital::read_parlamentares(export_path)
  proposicoes <- agoradigital::read_proposicoes(props_filepath)
  tramitacoes <- agoradigital::read_tramitacoes(trams_filepath)
  progressos <- agoradigital::read_progressos(progs_filepath)
  locais_atuais <- agoradigital::read_locais_atuais(locais_props_filepath)


  res <- list()
  count <- 0
  proposicoes_que_nao_baixaram <- pls
  proposicoes_individuais_a_baixar <-
    converte_tabela_geral_ids_casa(pls)

  while (count < 5) {
    cat(paste("\n--- Tentativa ", count + 1, "\n"))
    sleep_time = .DEF_REQ_SLEEP_TIME_IN_SECS ^ (count + 1)
    res <- append(
      res,
      proposicoes_que_nao_baixaram %>%
        purrr::pmap(
          process_pl,
          nrow(proposicoes_que_nao_baixaram),
          pautas = pautas,
          proposicoes = proposicoes,
          tramitacoes = tramitacoes,
          progressos = progressos,
          locais_atuais = locais_atuais,
          sleep_time = sleep_time
        )
    )

    proposicoes <-
      purrr::map_df(res, ~ .$proposicao) %>%
      dplyr::rename(id_ext = prop_id) %>%
      unique() %>% 
      process_proposicoes(parlamentares)
    
    proposicoes_baixadas <- proposicoes %>%
      dplyr::select(id_leggo,
                    casa,
                    id_casa = id_ext)


    proposicoes_que_nao_baixaram_temp <-
      dplyr::anti_join(
        proposicoes_individuais_a_baixar,
        proposicoes_baixadas,
        by = c("id_leggo", "casa", "id_casa")
      )
    
    proposicoes_que_nao_baixaram <- proposicoes_que_nao_baixaram %>%
      dplyr::filter((id_camara %in% proposicoes_que_nao_baixaram_temp$id_casa) |
                      (id_senado %in% proposicoes_que_nao_baixaram_temp$id_casa) |
                      (is.na(id_camara) & is.na(id_senado))
      )

    if (nrow(proposicoes_que_nao_baixaram) == 0) {
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
  hists_temperatura <- purrr::map_df(res, ~ .$hist_temperatura) %>%
    unique()
  progressos <-
    purrr::map_df(res, ~ .$progresso) %>%
    dplyr::rename(id_ext = prop_id) %>%
    unique()
  props_locais_atuais <- purrr::map_df(res, ~ .$local_atual)
  proposicoes_sem_local_atual <- agoradigital::verifica_proposicoes_com_local_detectado(proposicoes, props_locais_atuais)

  status_proposicoes <- tramitacoes %>%
    dplyr::arrange(dplyr::desc(data)) %>%
    dplyr::group_by(id_ext, casa) %>%
    dplyr::summarise(status = first(status)) %>%
    dplyr::ungroup()

  proposicoes <- proposicoes %>%
    dplyr::left_join(status_proposicoes,
                     by = c("id_ext", "casa"))

  ## export data to CSVs
  readr::write_csv(proposicoes, props_filepath)
  readr::write_csv(tramitacoes, trams_filepath)
  readr::write_csv(hists_temperatura, temps_filepath)
  readr::write_csv(progressos, progs_filepath)
  readr::write_csv(props_locais_atuais, locais_props_filepath)

}
