#' @title Ler a tabela de ids
#' @param pls_ids_filepath caminho do arquivo
#' @export
read_pls_ids <- function(pls_ids_filepath) {
  pls_ids <- readr::read_csv(pls_ids_filepath,
                             col_types = list(
                               id_camara = readr::col_double(),
                               id_senado = readr::col_double(),
                               apelido = readr::col_character(),
                               tema = readr::col_character(),
                               prioridade = readr::col_character()
                             ))
  pls_ids
}

#' @title Ler arquivos de documentos da camara
#' @param file_path caminho do arquivo
#' @export
read_current_docs_camara <- function(file_path) {
  if (file.exists(file_path)) {
    current_docs <- readr::read_csv(file_path,
                                    col_types = list(
                                      .default = readr::col_character(),
                                      id_documento = readr::col_double(),
                                      id_principal = readr::col_double(),
                                      numero = readr::col_integer(),
                                      ano = readr::col_integer(),
                                      data_apresentacao = readr::col_datetime(format = ""),
                                      cod_tipo_documento = readr::col_integer(),
                                      status_proposicao_cod_situacao = readr::col_integer(),
                                      status_proposicao_cod_tipo_tramitacao = readr::col_integer(),
                                      status_proposicao_data_hora = readr::col_datetime(format = ""),
                                      status_proposicao_sequencia = readr::col_integer()
                                    ))
  } else {
    current_docs <- tibble::tribble(~id_documento, ~id_principal, ~casa, ~sigla_tipo, ~numero, ~ano,
                                    ~data_apresentacao, ~ementa, ~cod_tipo_documento, ~descricao_tipo_documento,
                                    ~ementa_detalhada, ~keywords, ~status_proposicao_cod_situacao,
                                    ~status_proposicao_cod_tipo_tramitacao, ~status_proposicao_data_hora,
                                    ~status_proposicao_descricao_situacao, ~status_proposicao_descricao_tramitacao,
                                    ~status_proposicao_despacho, ~status_proposicao_regime,
                                    ~status_proposicao_sequencia, ~status_proposicao_sigla_orgao,
                                    ~status_proposicao_uri_orgao, ~status_proposicao_uri_ultimo_relator,
                                    ~status_proposicao_url, ~uri_documento, ~uri_autores, ~uri_prop_posterior,
                                    ~uri_prop_principal, ~uri_ultimo_relator, ~url_inteiro_teor,
                                    ~status_proposicao_ambito, ~uri_orgao_numerador)
  }
  return(current_docs)
}

#' @title Ler arquivos de autores da camara
#' @param file_path caminho do arquivo
#' @export
read_current_autores_camara <- function(file_path) {
  if (file.exists(file_path)) {
    current_autores <- readr::read_csv(file_path,
                                       col_types = list(
                                         .default = readr::col_character(),
                                         id_autor = readr::col_double(),
                                         cod_tipo_autor = readr::col_integer()
                                       ))
  } else {
    current_autores <- tibble::tribble(~id_autor, ~nome, ~tipo_autor, ~uri_autor, ~id_documento,
                                       ~casa, ~partido, ~uf, ~cod_tipo_autor)
  }
  return(current_autores)
}

#' @title Ler arquivos de deputados
#' @param file_path caminho do arquivo
#' @export
read_deputados <- function(file_path) {
  deputados <- readr::read_csv(file_path,
                               col_types = list(
                                 .default = readr::col_character(),
                                 data_falecimento = readr::col_date(format = ""),
                                 data_nascimento = readr::col_date(format = ""),
                                 id = readr::col_double(),
                                 ultimo_status_gabinete_andar = readr::col_double(),
                                 ultimo_status_gabinete_sala = readr::col_double(),
                                 ultimo_status_id = readr::col_double(),
                                 ultimo_status_id_legislatura = readr::col_double()
                               ))

  deputados <- deputados %>% dplyr::select(id, partido = ultimo_status_sigla_partido, uf = ultimo_status_sigla_uf, dplyr::everything())

}

#' @title Ler arquivos de documentos do senado
#' @param file_path caminho do arquivo
#' @export
read_current_docs_senado <- function(file_path) {
  if (file.exists(file_path)) {
    current_docs <- readr::read_csv(file_path,
                                    col_types = list(
                                      .default = readr::col_character(),
                                      autoria_texto = readr::col_character(),
                                      casa = readr::col_character(),
                                      data_texto = readr::col_date(format = ""),
                                      identificacao_comissao_codigo_comissao = readr::col_double()
                                    ))
  } else {
    current_docs <- tibble::tribble(~ano_materia, ~autoria_texto, ~casa, ~id_principal,
                                    ~id_documento, ~data_texto, ~descricao_identificacao_materia,
                                    ~descricao_objetivo_processo, ~descricao_subtipo_materia,
                                    ~descricao_texto, ~descricao_tipo_texto, ~formato_texto,
                                    ~identificacao_comissao_codigo_comissao,
                                    ~identificacao_comissao_nome_casa_comissao,
                                    ~identificacao_comissao_nome_comissao,
                                    ~identificacao_comissao_sigla_casa_comissao,
                                    ~identificacao_comissao_sigla_comissao, ~indicador_tramitando,
                                    ~nome_casa_identificacao_materia, ~numero_emenda, ~numero_materia,
                                    ~sigla_casa_identificacao_materia, ~sigla_subtipo_materia,
                                    ~tipo_documento, ~url_texto)
  }
  return(current_docs)
}

#' @title Ler arquivos de autores do senado
#' @param file_path caminho do arquivo
#' @export
read_current_autores_senado <- function(file_path) {

  if (file.exists(file_path)) {
    current_autores <- readr::read_csv(file_path,
                                       col_types = list(
                                         .default = readr::col_character(),
                                         id_principal = readr::col_double(),
                                         id_documento = readr::col_double(),
                                         id_autor = readr::col_double()
                                       ))
  } else {
    current_autores <- tibble::tribble(~id_principal, ~id_documento, ~casa, ~nome_autor, ~partido,
                                       ~uf, ~tipo_autor, ~id_autor)
  }

  return(current_autores)
}

#' @title Ler arquivos de senadores
#' @param file_path caminho do arquivo
#' @export
read_senadores <- function(file_path) {
  senadores <- readr::read_csv(file_path,
                                     col_types = list(
                                       .default = readr::col_character(),
                                       id_parlamentar = readr::col_double()
                                     ))
}

#' @title Ler arquivos de proposição
#' @param file_path caminho do arquivo
#' @export
read_props <- function(file_path) {
  readr::read_csv(col_types =  readr::cols(
      .default = readr::col_character(),
      id_ext =  readr::col_double(),
      numero =  readr::col_double(),
      data_apresentacao =  readr::col_datetime(format = ""),
      id_leggo =  readr::col_character()
    ), file_path)
}

#' @title Ler arquivo emendas_raw.csv
#' @param file_path caminho do arquivo
#' @export
read_emendas_raw <- function(file_path) {

  if (file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = list(
        .default = readr::col_character(),
        id_ext = readr::col_double(),
        codigo_emenda = readr::col_double(),
        numero = readr::col_double(),
        data_apresentacao = readr::col_date(format = "")
      )
    )
  } else {
    df <- tibble::tribble(~id_ext, ~codigo_emenda, ~data_apresentacao,
                          ~numero, ~local, ~autor, ~casa, ~tipo_documento, ~inteiro_teor)
  }

  return(df)
}

#' @title Ler arquivo novas_emendas.csv
#' @param file_path caminho do arquivo
#' @export
read_novas_emendas <- function(file_path) {

  if (file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = list(
        .default = readr::col_character(),
        id_ext = readr::col_double(),
        codigo_emenda = readr::col_double(),
        numero = readr::col_double(),
        data_apresentacao = readr::col_date(format = "")
      )
    )
  } else {
    df <- tibble::tribble(~id_ext, ~codigo_emenda, ~data_apresentacao,
                          ~numero, ~local, ~autor, ~casa, ~tipo_documento, ~inteiro_teor)
  }

  return(df)
}

#' @title Ler arquivo emendas.csv
#' @param file_path caminho do arquivo
#' @export
read_emendas <- function(file_path) {

  if (file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = list(
        .default = readr::col_character(),
        id_ext = readr::col_double(),
        codigo_emenda = readr::col_double(),
        numero = readr::col_double(),
        data_apresentacao = readr::col_date(format = ""),
        distancia = readr::col_double()
      )
    )
  } else {
    df <- tibble::tribble(~id_ext, ~codigo_emenda, ~data_apresentacao,
                          ~numero, ~local, ~autor, ~casa, ~tipo_documento,
                          ~inteiro_teor, ~distancia)
  }

  return(df)
}

#' @title Ler arquivo de distâncias (ex: 1198512_all_dist.csv)
#' @param file_path caminho do arquivo
#' @export
read_distance_file <- function(file_path) {

  if (file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = list(
        .default = readr::col_character(),
        id_emenda = readr::col_double(),
        id_proposicao = readr::col_double(),
        num_linha_proposicao = readr::col_integer(),
        distancia = readr::col_double()
      )
    )
  } else {
    df <- tibble::tribble(~id_emenda, ~id_proposicao,
                          ~casa, ~num_linha_proposicao, ~distancia)
  }

  return(df)
}


#' @title Ler arquivo de proposições
#' @param file_path caminho do arquivo
#' @export
read_proposicoes <- function(file_path) {
  if (file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = readr::cols(
          id_ext = readr::col_double(),
          sigla_tipo = readr::col_character(),
          numero = readr::col_integer(),
          ementa = readr::col_character(),
          data_apresentacao = readr::col_datetime(format = ""),
          casa = readr::col_character(),
          casa_origem = readr::col_character(),
          autor_nome = readr::col_character(),
          autor_uf = readr::col_character(),
          autor_partido = readr::col_character(),
          regime_tramitacao = readr::col_character(),
          forma_apreciacao = readr::col_character(),
          relator_id = readr::col_integer(),
          relator_id_parlametria = readr::col_integer(),
          id_leggo = readr::col_character(),
          uri_prop_principal = readr::col_character(),
          sigla = readr::col_character(),
          status = readr::col_character()
        )
    )
  } else {
    df <- tibble::tibble(
      id_ext = double(),
      sigla_tipo = character(),
      numero = double(),
      ementa = character(),
      data_apresentacao = date(),
      casa = character(),
      casa_origem = character(),
      autor_nome = character(),
      autor_uf = character(),
      autor_partido = character(),
      regime_tramitacao = character(),
      forma_apreciacao = character(),
      relator_id = double(),
      relator_id_parlametria = double(),
      id_leggo = character(),
      uri_prop_principal = character(),
      sigla = character(),
      status = character()
    )
  }

  return(df)
}

#' @title Ler arquivo de tramitações
#' @param file_path caminho do arquivo
#' @export
read_tramitacoes <- function(file_path) {
  if (file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = readr::cols(
        .default = readr::col_character(),
        id_ext = readr::col_double(),
        data = readr::col_datetime(format = ""),
        sequencia = readr::col_integer(),
        id_situacao = readr::col_integer(),
        ambito = readr::col_logical(),
        uri_ultimo_relator = readr::col_logical(),
        nivel = readr::col_integer(),
        temperatura_local = readr::col_double(),
        temperatura_evento = readr::col_double(),
        data_audiencia = readr::col_date(format = ""),
        origem_tramitacao_local_nome_casa_local = readr::col_character()
      )
    )
  } else {
    df <- tibble::tibble(
      id_ext = double(),
      casa = character(),
      data = date(),
      sequencia = integer(),
      texto_tramitacao = character(),
      sigla_local = character(),
      id_situacao = integer(),
      descricao_situacao = character(),
      link_inteiro_teor = character(),
      evento = character(),
      local = character(),
      ambito = character(),
      uri_ultimo_relator = character(),
      tipo_documento = character(),
      titulo_evento = character(),
      nivel = integer(),
      temperatura_local = double(),
      temperatura_evento = double(),
      fase = character(),
      situacao_descricao_situacao = character(),
      data_audiencia = date(),
      global = character(),
      origem_tramitacao_local_nome_casa_local = character(),
      status = character()
    )
  }

  return(df)
}


#' @title Ler arquivo de progressos
#' @param file_path caminho do arquivo
#' @export
read_progressos <- function(file_path) {
  if (file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = readr::cols(
        casa = readr::col_character(),
        id_ext = readr::col_double(),
        fase_global = readr::col_character(),
        local = readr::col_character(),
        data_inicio = readr::col_datetime(format = ""),
        data_fim = readr::col_datetime(format = ""),
        local_casa = readr::col_character(),
        pulou = readr::col_logical()
      )
    )
  } else {
    df <- tibble::tibble(
      casa = character(),
      id_ext = double(),
      fase_global = character(),
      local = character(),
      data_inicio = date(),
      data_fim = date(),
      local_casa = character(),
      pulou = logical()
    )
  }

  return(df)
}


#' @title Ler arquivo de locais atuais das proposições
#' @param file_path caminho do arquivo
#' @export
read_locais_atuais <- function(file_path) {
  if (file.exists(file_path)) {
    df <- readr::read_csv(
      file_path,
      col_types = readr::cols(
        id_leggo = readr::col_character(),
        sigla_ultimo_local = readr::col_character(),
        casa_ultimo_local = readr::col_character(),
        nome_ultimo_local = readr::col_character(),
        data_ultima_situacao = readr::col_datetime(format = ""),
        tipo_local = readr::col_character()
      )
    )

  } else {
    df <- tibble::tibble(
      id_leggo = character(),
      sigla_ultimo_local = character(),
      casa_ultimo_local = character(),
      nome_ultimo_local = character(),
      data_ultima_situacao = date(),
      tipo_local = character()
    )
  }

  return(df)
}
