#' @title Monta lista de dataframes para etapas não modificadas
#' @description Cria lista de dataframes para a etapa que não sofreu alteração
#' @param etapa_processada Lista de datframes com a etapa a ser montada
#' @param id ID da etapa da proposição
#' @param casa_arg Casa da etapa da proposição
#' @param proposicao Dataframe de proposição
#' @param tramitacao Dataframe de tramitação
#' @return Etapa processada contendo dados que não mudaram
.build_etapa_processada_nao_modificada <-
  function(etapa_processada,
           id,
           casa_arg,
           proposicao,
           tramitacao) {
    etapa_processada$result$proposicao <- proposicao %>%
      dplyr::filter(id_ext == id, casa == casa_arg) %>%
      dplyr::rename(prop_id = id_ext)

    etapa_processada$result$fases_eventos <- tramitacao %>%
      dplyr::filter(id_ext == id, casa == casa_arg) %>%
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


#' @title Monta a lista retornada pela função 'process_etapa'
#' @description Baseado em alguns parâmetros, monta lista de retorno da função
#' process_etapa.
#' @param prop Dataframe contendo os metadados de proposição (e status_tram)
#' @param tram Dataframe com a tramitação já processada
#' @param modified Flag indicando se houve modificação na proposição e a lista montada
#' deve ser retornada
#' @param return_modified_tag Flag indicando se é necessário retornar a informação
#' de que a proposição foi modificada.
#' @return Flag indicando se houve mudança na tramitação de uma proposição
.build_list_etapa_processada <-
  function(prop,
           tram,
           modified = TRUE,
           return_modified_tag = FALSE) {
    etapa = list()

    if (modified) {
      etapa$proposicao = prop
      etapa$fases_eventos = tram
    }

    if (return_modified_tag) {
      etapa$foi_modificada = modified
    }

    return(etapa)
  }

#' @title Processa a proposição
#' @description Realiza todas os processamentos necessários para criar
#' as tabelas para uma proposição
#' @param id Id da proposição
#' @param casa senado ou camara
#' @param retry Flag indicando se é necessário tentar novamente em caso de
#' erro.
#' @param return_modified_tag Flag indicando se é necessário retornar a informação
#' de que os dados da proposição foram modificados.
#' @return list com os dataframes: proposicao, fases_eventos,
#' hist_temperatura
process_etapa <- function(id,
                          casa,
                          pautas,
                          data_ultima_tramitacao = NULL,
                          return_modified_tag = FALSE,
                          retry = FALSE) {
  prop <- agoradigital::fetch_proposicao(id, casa, retry = retry)

  if (tolower(prop$sigla_tipo) == 'mpv') {
    tram <-
      agoradigital::fetch_tramitacao(id, casa, TRUE, retry = retry)
  } else {
    tram <- agoradigital::fetch_tramitacao(id, casa, retry = retry)
  }

  modified <- .checa_novas_tramitacoes(data_ultima_tramitacao, tram)

  if (modified) {
    futile.logger::flog.info(
      stringr::str_glue(
        "Houve modificação na proposição de id {id} na(o) {casa}. Processando dados para essa proposição...\n"
      )
    )

    proc_tram <-
      agoradigital::process_proposicao(prop, tram, casa) %>%
      dplyr::mutate(data_hora = as.POSIXct(data_hora))

    status <-
      agoradigital::extract_status_tramitacao(id, casa, prop, tram)

    extended_prop <-
      merge(prop, status, by = "prop_id") %>%
      dplyr::mutate(sigla = stringr::str_glue("{sigla_tipo} {numero}/{ano}"))

    return(
      .build_list_etapa_processada(extended_prop, proc_tram, modified, return_modified_tag)
    )

  } else {
    futile.logger::flog.info(stringr::str_glue("Não houve modificação na proposição de id {id} na(o) {casa}."))

    return(.build_list_etapa_processada(NULL, NULL, modified, return_modified_tag))
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
