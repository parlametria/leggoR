congresso_env <-
  jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))

#' @title Processa a proposição
#' @description Realiza todas os processamentos necessários para criar
#' as tabelas para uma proposição
#' @param id Id da proposição
#' @param casa senado ou camara
#' @return list com os dataframes: proposicao, fases_eventos,
#' hist_temperatura
process_etapa <- function(id, casa, pautas) {
  prop <- agoradigital::fetch_proposicao(id, casa)
  if (tolower(prop$sigla_tipo) == 'mpv') {
    tram <- agoradigital::fetch_tramitacao(id, casa, TRUE)
  } else {
    tram <- agoradigital::fetch_tramitacao(id, casa)
  }
  proc_tram <-
    agoradigital::process_proposicao(prop, tram, casa) %>%
    dplyr::mutate(data_hora = as.POSIXct(data_hora))
  status <-
    agoradigital::extract_status_tramitacao(id, casa, prop, tram)
  extended_prop <-
    merge(prop, status, by = "prop_id")
  
  list(proposicao = extended_prop,
       fases_eventos = proc_tram)
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
        ~ autor_nome,
        ~ autor_uf,
        ~ autor_partido,
        ~ regime_tramitacao,
        ~ forma_apreciacao,
        ~ relator_id,
        ~ relator_nome,
        ~ relator_partido,
        ~ relator_uf,
        ~ relator_data,
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
        ~ descricao_situacao,
        ~ link_inteiro_teor,
        ~ evento,
        ~ local,
        ~ tipo_documento,
        ~ nivel,
        ~ titulo_evento
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
    dplyr::mutate(
      status = dplyr::case_when(
        evento == "arquivamento" ~ "Arquivada",
        evento == "desarquivamento" ~ "Ativa",
        evento == "perda_da_eficacia" ~ "Caducou",
        evento == "rejeicao_projeto" ~ "Rejeitada",
        evento == "transformada_lei" ~ "Lei"
      )
    ) %>%
    tidyr::fill(status, .direction = "up") %>%
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
    
    etapas <- list()
    if (!is.na(id_camara)) {
      etapa_processada <-
        safe_process_etapa(id_camara, "camara", pautas = pautas)
      etapas %<>% append(list(etapa_processada$result))
      if (!is.null(etapa_processada$error)) {
        print(etapa_processada$error)
        return(etapas)
      }
    }
    if (!is.na(id_senado)) {
      etapa_processada <-
        safe_process_etapa(id_senado, "senado", pautas = pautas)
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
    if (nrow(etapas$proposicao) != 0) {
      sigla <- tolower(etapas$proposicao$sigla_tipo)
      if (length(sigla) > 1) {
        sigla <- sigla[1]
      }
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
      etapas[["hist_temperatura"]] <-
        agoradigital::get_historico_temperatura_recente_id_leggo(
          tram = etapas$fases_eventos,
          id_leggo = row_num,
          pautas = pautas
        )
    }
    etapas$proposicao %<>%
      dplyr::mutate(id_leggo = row_num)
    Sys.sleep(5 * stats::runif(1))
    return(etapas)
  }

#' @title Formata tabela de proposições
#' @description Recebi um dataframe com id_camara, id_senado
#' casa, apelido e tema e formata
#' @param pls dataframe com proposições.
#' @export
converte_tabela_geral_ids_casa <- function(pls) {
  pls_ids <- pls %>%
    dplyr::mutate(id_leggo = dplyr::row_number())
  
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
    tibble::tribble( ~ data, ~ sigla, ~ id_ext, ~ local, ~ casa, ~ semana, ~
                       ano)
  
  tryCatch({
    pautas <- readr::read_csv(paste0(export_path, "pautas.csv"))
  },
  error = function(msg) {
    
  })
  
  parlamentares <- tryCatch({
    export_path_parlamentares <- export_path
    
    if (!stringr::str_detect(export_path, "\\/$")) {
      export_path_parlamentares <- paste0(export_path, "/")
    }
    
    readr::read_csv(
      paste0(export_path_parlamentares, "parlamentares.csv"),
      col_types = readr::cols("legislatura"="i",
                              .default = "c")
    )
  },
  error = function(msg) {
    print("Erro ao importar dados de parlamentares em fetch_props:")
    print(msg)
    return(
      tibble::tribble(
        ~ id_parlamentar,
        ~ id_parlamentar_parlametria,
        ~ casa,
        ~ nome_eleitoral,
        ~ nome_civil,
        ~ cpf,
        ~ sexo,
        ~ partido,
        ~ uf,
        ~ situacao,
        ~ em_exercicio
      )
    )
  })
  
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
  
  ## export data to CSVs
  readr::write_csv(proposicoes, paste0(export_path, "/proposicoes.csv"))
  readr::write_csv(tramitacoes, paste0(export_path, "/trams.csv"))
  readr::write_csv(hists_temperatura,
                   paste0(export_path, "/hists_temperatura.csv"))
  readr::write_csv(progressos, paste0(export_path, "/progressos.csv"))
  
}

#' @title Recupera a casa da proposição que será usada para recuperar os seus autores
#' @description A partir do id da proposição na Câmara e no Senado recupera qual a casa que será
#' usada para recuperação dos autores.
#' @param id_camara Id da proposição na Câmara
#' @param id_senado Id da proposição no Senado
#' @return "camara" se a câmara deve ser usada para recuperar as informações dos autores ou "senado" caso o
#' senado deva ser usado.
get_casa_proposicao <- function(id_camara, id_senado) {
  if (is.na(id_camara)) {
    casa_proposicao = "senado"
  } else if (is.na(id_senado)) {
    casa_proposicao = "camara"
  } else {

    # prop_casa <- agoradigital::fetch_proposicao(id_senado, "senado") %>%
    #   dplyr::pull(casa_proposicao)

    prop_casa <- tryCatch({
      agoradigital::fetch_proposicao(id_senado, "senado") %>%
        dplyr::pull(casa_origem)
    }, error = function(e) {
      message(e)
      return(NA)
    })

    if (is.na(prop_casa)) {
      prop_casa <- tryCatch({
        agoradigital::fetch_proposicao(id_camara, "camara") %>%
          dplyr::pull(casa_origem)
      }, error = function(e) {
        message(e)
        return(NULL)
      })

      if(is.na(prop_casa) | prop_casa == "senado") {
        casa_proposicao = "senado"
      } else {
        casa_proposicao = "camara"
      }

    } else {
      if (prop_casa == "camara") {
        casa_proposicao = "camara"
      } else {
        casa_proposicao = "senado"
      }
    }
  }

  return(casa_proposicao)
}

#' @title Recupera autores a partir dos ids da proposição na Câmara e no Senado
#' @description A partir do id da proposição na Câmara e no Senado recupera autores
#' @param id_leggo Id da proposição no Leggo
#' @param id_camara Id da proposição na Câmara
#' @param id_senado Id da proposição no Senado
#' @param total_rows Número total de proposições para análise. Usada apenas no print do log de execução.
#' @return Dataframe com os autores da proposição
process_autores_pl <- function(id_leggo, id_camara, id_senado, total_rows = 1) {
  print(paste("Recuperando atores para a proposição: câmara", id_camara, " senado", id_senado,
              "-", id_leggo, "/", total_rows))

  casa_origem <- get_casa_proposicao(id_camara, id_senado)
  print(paste0("Casa da proposição: ", casa_origem))

  autores <- tryCatch({
    if (casa_origem == "camara") {
      autores <- agoradigital::fetch_autores_documento(id_camara, "camara") %>%
        dplyr::mutate(id_autor = as.character(id_autor),
                      id_ext = id_camara,
                      id_leggo = id_leggo
                      ) %>%
        dplyr::select(id_leggo,
                      id_ext,
                      id_autor,
                      tipo_autor = tipo,
                      nome_autor = nome)

    } else if (casa_origem == "senado") {
      autores <- agoradigital::fetch_autores_documento(id_senado, "senado") %>%
        dplyr::mutate(id_autor = as.character(id_parlamentar),
                      id_ext = id_senado,
                      id_leggo = id_leggo) %>%
        dplyr::select(id_leggo,
                      id_ext,
                      id_autor,
                      tipo_autor = descricao_tipo_autor,
                      nome_autor = nome_autor)
    } else {
      stop("Casa inválida!")
    }
  }, error = function(e) {
    message(e)
    return(tibble::tribble(~id_leggo, ~id_ext, ~id_autor, ~tipo_autor, ~nome_autor))
  })

  return(autores)
}

#' @title Exporta dados dos autores das proposições (matérias legislativas) monitoradas pelo Leggo
#' @description Exporta para uma pasta o CSV que liga uma proposição aos seus autores
#' @param pls dataframe com proposições.
#' @param export_path pasta para onde exportar dados.
#' @export
process_autores_props <- function(pls_ids_filepath, export_path) {
  set.seed(123)
  pls <- readr::read_csv(pls_ids_filepath) %>%
    dplyr::mutate(id_leggo = 1:nrow(.)) %>%
    dplyr::select(id_leggo, id_camara, id_senado)

  total_rows <- pls %>% nrow()

  pls_autores <- purrr::pmap_df(
    list(
      pls$id_leggo,
      pls$id_camara,
      pls$id_senado,
      total_rows
    ),
    ~ process_autores_pl(..1, ..2, ..3, total_rows = total_rows)
  )

  autores_leggo <- pls %>%
    dplyr::select(id_leggo, id_camara, id_senado) %>%
    dplyr::inner_join(pls_autores %>%
                       select(id_leggo, id_autor, nome_autor, tipo_autor),
                     by = c("id_leggo")) %>%
    fuzzyjoin::regex_left_join(congresso_env$tipos_autores, by = c(nome_autor = "regex"), ignore_case = T) %>%
    dplyr::mutate(id_autor = dplyr::if_else(!is.na(id_entidade),
                                            as.character(id_entidade),
                                            id_autor)) %>%
    dplyr::select(-regex, -id_entidade) %>%
    dplyr::mutate(id_autor_parlametria = dplyr::case_when(
      tipo_autor == "Deputado" ~ paste0(1, id_autor),
      tipo_autor == "Senador" ~ paste0(2, id_autor),
      TRUE ~ paste0(3, id_autor)
    )) %>%
    dplyr::select(id_leggo, id_camara, id_senado, id_autor_parlametria, id_autor)

  pls_sem_autores_baixados <- pls %>%
    dplyr::filter(!id_leggo %in% (autores_leggo %>% dplyr::pull(id_leggo)))

  if (pls_sem_autores_baixados %>% nrow() > 0) {
    message("Não foram recuperados os Autores para as seguintes proposições")
    print(pls_sem_autores_baixados)
  }

  readr::write_csv(autores_leggo, paste0(export_path, "/autores_leggo.csv"), na = "None")
}