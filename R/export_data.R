congresso_env <-
  jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))

#' @title Monta lista de dataframes para etapas não modificadas
#' @description Cria lista de dataframes para a etapa que não sofreu alteração
#' @param etapa_processada Lista de datframes com a etapa a ser montada
#' @param proposicao Dataframe de proposição
#' @param tramitacao Dataframe de tramitação
#' @return Etapa processada contendo dados que não mudaram
.monta_etapa_processada_nao_modificada <- function(etapa_processada, proposicao, tramitacao) {
  etapa_processada$result$proposicao <- proposicao %>%
    dplyr::rename(prop_id = id_ext)

  etapa_processada$result$fases_eventos <- tramitacao %>%
    dplyr::select(
      prop_id = id_ext,
      casa,
      data_hora = data,
      sequencia,
      texto_tramitacao,
      sigla_local,
      id_situacao,
      descricao_situacao,
      link_inteiro_teor,
      evento,
      local,
      ambito,
      uri_ultimo_relator,
      tipo_documento,
      titulo_evento,
      nivel,
      temperatura_local,
      temperatura_evento
    )

  return(etapa_processada)
}


#' @title Checa se existem novas tramitações
#' @description Compara a data da última tramitação atual e a
#' que vem do df de tram
#' @param data_ultima_tramitacao Data da última tramitação atual
#' @param tram Dataframe que pode conter novas tramitações
#' @return Flag indicando se houve mudança na tramitação de uma proposição
.checa_novas_tramitacoes <-
  function(data_ultima_tramitacao = NULL, tram) {
    if (is.null(data_ultima_tramitacao)) {
      return(TRUE)
    }

    ultima_data_tram <- tram %>%
      dplyr::distinct(data_hora) %>%
      dplyr::filter(data_hora == max(data_hora)) %>%
      dplyr::pull(data_hora)

    if (ultima_data_tram > data_ultima_tramitacao) {
      return(TRUE)
    }

    return(FALSE)
  }


#' @title Processa a proposição
#' @description Realiza todas os processamentos necessários para criar
#' as tabelas para uma proposição
#' @param id Id da proposição
#' @param casa senado ou camara
#' @param retry Flag indicando se é necessário tentar novamente em caso de
#' erro.
#' @return list com os dataframes: proposicao, fases_eventos,
#' hist_temperatura
process_etapa <-
  function(id,
           casa,
           pautas,
           data_ultima_tramitacao = NULL,
           retry = FALSE) {
    prop <- agoradigital::fetch_proposicao(id, casa, retry = retry)
    if (tolower(prop$sigla_tipo) == 'mpv') {
      tram <-
        agoradigital::fetch_tramitacao(id, casa, TRUE, retry = retry)
    } else {
      tram <- agoradigital::fetch_tramitacao(id, casa, retry = retry)
    }

    if (.checa_novas_tramitacoes(data_ultima_tramitacao, tram)) {
      futile.logger::flog.info(paste0(
        "Houve modificação na proposição de id ",
        id,
        " na(o) ",
        casa,
        ". Processando dados de proposição e tramitação para essa proposição...\n"
      ))
      proc_tram <-
        agoradigital::process_proposicao(prop, tram, casa) %>%
        dplyr::mutate(data_hora = as.POSIXct(data_hora))

      status <-
        agoradigital::extract_status_tramitacao(id, casa, prop, tram)

      extended_prop <-
        merge(prop, status, by = "prop_id")

      return(list(
        proposicao = extended_prop,
        fases_eventos = proc_tram,
        foi_modificada = TRUE
      ))
    } else {
      futile.logger::flog.info(paste0(
        "Não houve modificação na proposição de id ",
        id,
        " na(o) ",
        casa,
        ".\n"
      ))
      return(list(
        proposicao = NULL,
        fases_eventos = NULL,
        foi_modificada = FALSE
      ))
    }
  }

safe_process_etapa <- purrr::safely(
  process_etapa,
  otherwise =
    list(
      proposicao = tibble::tribble(
        ~ prop_id,
        ~ sigla_tipo,
        ~ numero,
        ~ ano,
        ~ ementa,
        ~ data_apresentacao,
        ~ casa,
        ~ casa_origem,
        ~ sigla_ultimo_local,
        ~ sigla_casa_ultimo_local,
        ~ nome_ultimo_local,
        ~ data_ultima_situacao,
        ~ uri_prop_principal,
        ~ regime_tramitacao,
        ~ forma_apreciacao,
        ~ relator_id,
        ~ relator_nome,
        ~ relator_partido,
        ~ relator_uf,
        ~ relator_data
      ),
      fases_eventos = tibble::tribble(
        ~ prop_id,
        ~ casa,
        ~ data_hora,
        ~ sequencia,
        ~ texto_tramitacao,
        ~ sigla_local,
        ~ id_situacao,
        ~ descricao_situacao,
        ~ link_inteiro_teor,
        ~ evento,
        ~ local,
        ~ uri_ultimo_relator,
        ~ tipo_documento,
        ~ titulo_evento,
        ~ nivel,
        ~ temperatura_local,
        ~ temperatura_evento
      )
    )
)


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

#' @title Gera etapas
#' @description Gera a lista etapas com todas as tabelas necessárias para o back
#' @param row_num Row number da proposição na tabela
#' @param id_camara Id da proposição na camara
#' @param id_senado Id da proposição no senado
#' @param total_rows número de linhas da tabela com os ids das proposições
#' @return Dataframe
process_pl <-
  function(row_num,
           id_camara,
           id_senado,
           total_rows,
           pautas,
           proposicoes = NULL,
           tramitacoes = NULL,
           progressos = NULL,
           locais_atuais = NULL,
           sleep_time = .DEF_REQ_SLEEP_TIME_IN_SECS) {
    Sys.sleep(sleep_time)
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

    proposicao_camara <- NULL
    proposicao_senado <- NULL

    tramitacoes_camara <- NULL
    tramitacoes_senado <- NULL

    ultima_tramitacao_camara <- NULL
    ultima_tramitacao_senado <- NULL

    if (!is.null(proposicoes) && !is.null(tramitacoes)) {
      ultima_tramitacao_data <- tramitacoes %>%
        dplyr::filter((casa == "camara" &
                  id_ext == id_camara) |
                 (casa == "senado" & id_ext == id_senado)) %>%
        dplyr::distinct(data) %>%
        dplyr::filter(data == max(data)) %>%
        dplyr::pull(data)

      if (!is.na(id_camara)) {
        proposicao_camara <- proposicoes %>%
          dplyr::filter(id_ext == id_camara, casa == "camara")

        tramitacoes_camara <- tramitacoes %>%
          dplyr::filter(id_ext == id_camara, casa == "camara")

      }

      if (!is.na(id_senado)) {
        proposicao_senado <- proposicoes %>%
          dplyr::filter(id_ext == id_senado, casa == "senado")

        tramitacoes_senado <- tramitacoes %>%
          dplyr::filter(id_ext == id_senado, casa == "senado")

      }

    }

    etapas <- list()
    houve_modificacao <- TRUE

    if (!is.na(id_camara)) {
      etapa_processada <-
        safe_process_etapa(id_camara,
                           "camara",
                           pautas = pautas,
                           data_ultima_tramitacao = ultima_tramitacao_data)

      if (!etapa_processada$result$foi_modificada) {
        etapa_processada <- etapa_processada %>%
          .monta_etapa_processada_nao_modificada(proposicao_camara, tramitacoes_camara)
        houve_modificacao <- FALSE

      } else {
        etapa_processada$result$proposicao <-
          etapa_processada$result$proposicao %>%
          dplyr::mutate(sigla = stringr::str_glue("{sigla_tipo} {numero}/{ano}"))
      }
      etapa_processada$result$foi_modificada <- NULL

      etapas %<>% append(list(etapa_processada$result))
      if (!is.null(etapa_processada$error)) {
        print(etapa_processada$error)
        return(etapas)
      }
    }
    if (!is.na(id_senado)) {
      
      # Checa se houve modificação na Câmara. Se sim, é necessário gerar todo o dado para o Senado também.
      if (houve_modificacao) {
        ultima_tramitacao_data <- NULL
      }
      
      etapa_processada <-
        safe_process_etapa(id_senado,
                           "senado",
                           pautas = pautas,
                           data_ultima_tramitacao = ultima_tramitacao_data)

      if (!etapa_processada$result$foi_modificada) {
        etapa_processada <- etapa_processada %>%
          .monta_etapa_processada_nao_modificada(proposicao_senado, tramitacoes_senado)
        houve_modificacao <- houve_modificacao | FALSE

      } else {
        etapa_processada$result$proposicao <-
          etapa_processada$result$proposicao %>%
          dplyr::mutate(sigla = stringr::str_glue("{sigla_tipo} {numero}/{ano}"))
        houve_modificacao <- TRUE
      }

      etapa_processada$result$foi_modificada <- NULL
      etapas %<>% append(list(etapa_processada$result))
      if (!is.null(etapa_processada$error)) {
        print(etapa_processada$error)
        return(etapas)
      }
    }

    if (is.na(id_camara) & is.na(id_senado)) {
      message("A proposição não foi processada!")
      print("A proposição está preenchida com o id_camara e o id_senado como NA.")
      return(etapas)
    }

    etapas %<>% purrr::pmap(dplyr::bind_rows)

    id_leggo_key = digest::digest(paste0(id_camara , " ", id_senado),
                                  algo = "md5",
                                  serialize = F)

    if (houve_modificacao) {
      if (nrow(etapas$proposicao) != 0) {
        sigla <- tolower(etapas$proposicao$sigla_tipo)
        if (length(sigla) > 1) {
          sigla <- sigla[1]
        }

        ## Processa dados do progresso
        if (sigla == 'mpv') {
          etapas[["progresso"]] <-
            agoradigital::generate_progresso_df_mpv(etapas$fases_eventos, etapas$proposicao) %>%
            dplyr::mutate(local = "", local_casa = "") %>%
            adiciona_coluna_pulou_mpv()
        } else {
          etapas[["progresso"]] <-
            agoradigital::get_progresso(etapas$proposicao, etapas$fases_eventos) %>%
            adiciona_coluna_pulou() %>%
            adiciona_locais_faltantes_progresso()
        }
        
        etapas$proposicao <- etapas$proposicao %>% 
          dplyr::mutate(id_leggo = id_leggo_key)

        ## Processa dados de Locais Atuais da proposição
        etapas[["local_atual"]] <-
          agoradigital::processa_local_atual(proposicao_df = etapas$proposicao,
                                             id_leggo = id_leggo_key)

        Sys.sleep(5 * stats::runif(1))

      }

    } else {
      if (!is.null(progressos)) {
        etapas[["progresso"]] <- progressos %>%
          dplyr::filter((casa == 'camara' &
                    id_ext == id_camara) |
                   (casa == "senado" & id_ext == id_senado)) %>%
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
          tram = etapas$fases_eventos,
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
    pautas <- readr::read_csv(paste0(export_path, "/pautas.csv"))
  },
  error = function(msg) {

  })

  props_filepath = paste0(export_path, "/proposicoes.csv")
  trams_filepath = paste0(export_path, "/trams.csv")
  temps_filepath = paste0(export_path, "/hists_temperatura.csv")
  progs_filepath = paste0(export_path, "/progressos.csv")
  locais_props_filepath = paste0(export_path, "/props_locais_atuais.csv")

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
      dplyr::select(-c(ano)) %>%
      dplyr::rename(id_ext = prop_id) %>%
      dplyr::select(-c(tema, apelido_materia)) %>%
      unique() %>%
      agoradigital::mapeia_nome_relator_para_id(parlamentares)

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
    dplyr::arrange(desc(data)) %>%
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
