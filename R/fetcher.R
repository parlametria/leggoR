source(here::here("R/utils.R"))
source(here::here("R/agendas.R"))
senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
senado_constants <- senado_env$constants

#' @title Busca votações de uma proposição no Senado
#' @description Retorna dataframe com os dados das votações de uma proposição no Senado.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as votações de uma proposição no Senado
#' @examples
#' fetch_votacoes(91341)
#' @export
fetch_votacoes <- function(proposicao_id) {
  url_base_votacoes <-
    paste0(senado_env$endpoints_api$url_base, "votacoes/")

  url <- paste0(url_base_votacoes, proposicao_id)
  json_votacoes <- fetch_json_try(url)
  votacoes_data <-
    json_votacoes %>%
    magrittr::extract2("VotacaoMateria") %>%
    magrittr::extract2("Materia")
  votacoes_ids <-
    votacoes_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as.tibble() %>%
    unique()
  votacoes_df <-
    votacoes_data %>%
    magrittr::extract2("Votacoes") %>%
    purrr::map_df( ~ .) %>%
    tidyr::unnest()

  votacoes_df <-
    votacoes_df %>%
    tibble::add_column(!!!votacoes_ids)

  votacoes_df <- votacoes_df[,!sapply(votacoes_df, is.list)]
  rename_votacoes_df(votacoes_df)
}

#' @title Deferimento de requerimentos.
#' @description Verifica deferimento ou não para uma lista de IDs de requerimentos.
#' @param proposicao_id ID de um ou vários requerimentos
#' @return Dataframe com IDs dos requerimentos e informação sobre deferimento.
#' @examples
#' fetch_deferimento(c("102343", "109173", "115853"))
#' @importFrom utils tail
#' @export
fetch_deferimento <- function(proposicao_id) {
  deferimento_regexes <- senado_env$deferimento
  regexes <-
    tibble::frame_data(
      ~ deferimento,
      ~ regex,
      "indeferido",
      deferimento_regexes$regex$indeferido,
      "deferido",
      deferimento_regexes$regex$deferido
    )

  fetch_one_deferimento <- function(proposicao_id) {
    json <-
      paste0(senado_env$endpoints_api$url_base,
             "movimentacoes/",
             proposicao_id) %>%
      jsonlite::fromJSON()

    resultados <-
      json$MovimentacaoMateria$Materia$OrdensDoDia$OrdemDoDia$DescricaoResultado
    # handle NULL
    if (is.null(resultados))
      resultados <- c('')

    resultados %>%
      tibble::as.tibble() %>%
      dplyr::mutate(proposicao_id = proposicao_id) %>%
      fuzzyjoin::regex_left_join(regexes, by = c(value = "regex")) %>%
      tidyr::fill(deferimento) %>%
      tail(., n = 1) %>%
      dplyr::select(proposicao_id, deferimento)
  }

  proposicao_id %>%
    unlist %>%
    unique %>%
    lapply(fetch_one_deferimento) %>%
    plyr::rbind.fill()
}

#' @title Renomeia as colunas do dataframe de votação no Senado
#' @description Renomeia as colunas do dataframe de votação no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe da votação no Senado
#' @return Dataframe com as colunas renomeadas
#' @export
rename_votacoes_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace(
      "sessao_plenaria_|tramitacao_identificacao_tramitacao_|identificacao_parlamentar_",
      ""
    )

  names(df) <- new_names

  df
}

#' @title Retorna as sessões deliberativas de uma proposição no Senado
#' @description Retorna dataframe com os dados das sessões deliberativas de uma proposição no Senado.
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as sessões deliberativas de uma proposição no Senado
#' @examples
#' fetch_sessions(91341)
#' @export
fetch_sessions <- function(bill_id) {
  url_base_sessions <-
    "http://legis.senado.leg.br/dadosabertos/materia/ordia/"
  url <- paste0(url_base_sessions, bill_id)

  json_sessions <- jsonlite::fromJSON(url, flatten = T)

  sessions_data <- json_sessions %>%
    magrittr::extract2("OrdiaMateria") %>%
    magrittr::extract2("Materia")

  ordem_do_dia_df <- sessions_data %>%
    magrittr::extract2("OrdensDoDia") %>%
    magrittr::extract2("OrdemDoDia") %>%
    magrittr::extract2("SessaoPlenaria") %>%
    purrr::map_df( ~ .) %>%
    tidyr::unnest() %>%
    rename_table_to_underscore()

  ordem_do_dia_df
}

###################################################################

#' @title Recupera o estado e partido de um autor
#' @description Retorna o estado e partido
#' @param uri uri que contém dados sobre o autor
#' @return Estado e partido
#' @export
extract_partido_estado_autor <- function(uri) {
  if (!is.na(uri)) {
    json_autor <- fetch_json_try(uri)

    autor <-
      json_autor %>%
      magrittr::extract2('dados')

    autor_uf <-
      autor %>%
      magrittr::extract2('ufNascimento')

    autor_partido <-
      autor %>%
      magrittr::extract2('ultimoStatus') %>%
      magrittr::extract2('siglaPartido')

    paste0(autor_partido, '/', autor_uf)
  } else {
    ''
  }
}

#' @title Recupera as proposições apensadas
#' @description Retorna os IDs das proposições apensadas a uma determinada proposição
#' @param prop_id ID da proposição
#' @return Ventor contendo os IDs das proposições apensadas
#' @examples
#' fetch_apensadas(2121442)
#' @export
fetch_apensadas <- function(prop_id) {
  api_v1_proposicao_url <- 'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp='
  xml2::read_xml(paste0(api_v1_proposicao_url, prop_id)) %>%
    xml2::xml_find_all('//apensadas/proposicao/codProposicao') %>%
    xml2::xml_text() %>%
    tibble::tibble(apensadas = .)
}

#' @title Recupera os eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' fetch_events(2121442)
#' @export
#' @importFrom utils timestamp
fetch_events <- function(prop_id) {
  events_base_url <-
    'http://www.camara.gov.br/proposicoesWeb/sessoes_e_reunioes?idProposicao='
  bill_events_url <- paste0(events_base_url, prop_id)
  events <- bill_events_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="content"]/table') %>%
    rvest::html_table()
  events_df <- events[[1]]
  names(events_df) <- c('timestamp', 'origem', 'descricao', 'links')
  events_df %>%
    dplyr::select(-links) %>%
    dplyr::mutate(timestamp = lubridate::dmy_hm(timestamp))
}

###################################################################

#' @title Retorna o dataFrame com as audiências públicas do Senado
#' @description Retorna um dataframe contendo as audiências públicas do Senado
#' @param initial_date data inicial no formato yyyy-mm-dd
#' @param end_date data final no formato yyyy-mm-dd
#' @return Dataframe
#' @examples
#' get_audiencias_publicas('2016-05-15', '2016-05-25')
get_audiencias_publicas <- function(initial_date, end_date) {

  pega_audiencias_publicas_do_data_frame <- function(l){
    if(length(l$Tipo) == 1 ) {
      if (l$Tipo == "Audiência Pública Interativa") {
        paste(l$Eventos$Evento$MateriasRelacionadas$Materia$Codigo, collapse = " ,")
      }else {
        ""
      }
    }else {
      if ("Audiência Pública Interativa" %in% l$Tipo) {
        paste(l$Eventos$Evento$MateriasRelacionadas, collapse = " ,")
      }else {
        ""
      }
    }
  }

  agenda_senado <- get_data_frame_agenda_senado(initial_date, end_date) %>%
    dplyr::mutate(id_proposicao = purrr::map_chr(partes_parte, ~ pega_audiencias_publicas_do_data_frame(.)))

  if ("comissoes_comissao_sigla" %in% names(agenda_senado)) {
    agenda_senado %>%
      dplyr::select(data, hora, realizada, sigla = comissoes_comissao_sigla, id_proposicao)
  }else {
    agenda_senado %>%
      mutate(sigla = purrr::map_chr(comissoes_comissao, ~ paste(.$Sigla, collapse = " ,"))) %>%
      dplyr::select(data, hora, realizada, sigla, id_proposicao)
  }
}

#' @title Baixa dados de requerimentos relacionados
#' @description Retorna um dataframe contendo dados sobre os requerimentos relacionados a uma proposição
#' @param id ID de uma proposição
#' @param mark_deferimento valor default true
#' @return Dataframe
#' @export
fetch_related_requerimentos <- function(id, mark_deferimento = TRUE) {
  regexes <-
    tibble::frame_data(
      ~ deferimento,
      ~ regex,
      'indeferido',
      '^Indefiro',
      'deferido',
      '^(Defiro)|(Aprovado)'
    )

  related <-
    rcongresso::fetch_relacionadas(id)$uri %>%
    strsplit('/') %>%
    vapply(last, '') %>%
    unique %>%
    rcongresso::fetch_proposicao()

  requerimentos <-
    related %>%
    dplyr::filter(stringr::str_detect(.$siglaTipo, '^REQ'))

  if (!mark_deferimento)
    return(requerimentos)

  tramitacoes <- fetch_tramitacao(requerimentos$id, 'camara', TRUE)

  related <-
    tramitacoes %>%
    # mark tramitacoes rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by = c(texto_tramitacao = 'regex')) %>%
    dplyr::group_by(prop_id) %>%
    # fill down marks
    tidyr::fill(deferimento) %>%
    # get last mark on each tramitacao
    dplyr::do(tail(., n = 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(prop_id, deferimento) %>%
    # and mark proposicoes based on last tramitacao mark
    dplyr::left_join(related, by = c('prop_id' = 'id'))
}







#' @title Baixa a agenda de audiências públicas na câmara por órgão
#' @description Retorna um dataframe contendo as audiências públicas da camara ou do senado
#' @param initial_date data inicial no formato dd/mm/yyyy
#' @param end_date data final no formato dd/mm/yyyy
#' @param fases_tramitacao_df dataframe da PL preprocessada
#' @return Dataframe com as audiências públicas de um órgão
#' @examples
#' fetch_audiencias_publicas_by_orgao_camara('01/01/2017', '30/10/2018', process_proposicao(fetch_proposicao(2121442, 'camara', 'Lei do Teto Remuneratório', 'Agenda Nacional'), fetch_tramitacao(2121442, 'camara', T), 'camara'))
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#' @importFrom utils tail
#' @importFrom lubridate as_date
fetch_audiencias_publicas_by_orgao_camara <- function(initial_date, end_date, fases_tramitacao_df){
  orgao_atual <-
    fases_tramitacao_df %>%
    dplyr::filter(data_hora >= lubridate::as_date(lubridate::dmy(initial_date)) &
                    data_hora <= lubridate::as_date((lubridate::dmy(end_date))) &
                    sigla_local != 'MESA' &
                    sigla_local != 'PLEN') %>%
    utils::tail(1) %>%
    dplyr::mutate(local =
                    dplyr::if_else(toupper(local) == "PLENÁRIO", "PLEN", dplyr::if_else(local == 'Comissão Especial',
                                                                                        sigla_local,
                                                                                        local))) %>%
    dplyr::distinct() %>%
    dplyr::select(local) %>%
    dplyr::rename(sigla = local)

  if(nrow(orgao_atual) > 0){
    orgao_id <-
      fetch_todos_orgaos() %>%
      dplyr::filter(stringr::str_detect(sigla, orgao_atual$sigla)) %>%
      dplyr::select(orgao_id)


    url <- RCurl::getURL(paste0(
      'http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterPauta?IDOrgao=',
      orgao_id$orgao_id, '&datIni=', initial_date, '&datFim=', end_date))

    eventos_list <- readXML(url)

    df <-
      eventos_list %>%
      jsonlite::toJSON() %>%
      jsonlite::fromJSON()

    if(purrr::is_list(df)){
      df <- df %>%
        purrr::list_modify(".attrs" = NULL) %>%
        tibble::as.tibble() %>%
        t() %>%
        as.data.frame()

      names(df) <- c("comissao","cod_reuniao", "num_reuniao", "data", "hora", "local",
                     "estado", "tipo", "titulo_reuniao", "objeto", "proposicoes")

      df <- df %>%
        dplyr::filter (tipo == 'Audiência Pública') %>%
        dplyr::select(-c(num_reuniao, proposicoes)) %>%
        as.data.frame() %>%
        sapply( function(x) unlist(x)) %>%
        as.data.frame()

      #df <- df %>%
      #  dplyr::mutate(proposicao = stringr::str_extract(tolower(objeto), '"discussão d(o|a) (pl|projeto de lei) .*"'),
      #                tipo_materia = dplyr::case_when(
      #                  stringr::str_detect(tolower(proposicao), 'pl| projeto de lei') ~ 'PL',
      #                  TRUE ~ 'NA'),
      #                numero_aux = stringr::str_extract(tolower(proposicao), "(\\d*.|)\\d* de"),
      #                numero = stringr::str_extract(tolower(numero_aux), "(\\d*.|)\\d*"),
      #                numero = gsub('\\.', '', numero),
      #                ano_aux = stringr::str_extract(tolower(proposicao), "( de |/)\\d*"),
      #                ano = stringr::str_extract(tolower(ano_aux), "\\d{4}|\\d{2}")) %>%
      #  dplyr::select(-ano_aux, -numero_aux, -proposicao,  -tipo)

      df <- df %>%
        dplyr::mutate(requerimento =
                        stringr::str_extract_all(tolower(objeto),
                                                 camara_env$frase_requerimento$requerimento),
                      num_requerimento =
                        dplyr::if_else(
                          stringr::str_extract_all(
                            requerimento, camara_env$extract_requerimento_num$regex) != 'character(0)',
                          stringr::str_extract_all(
                            requerimento, camara_env$extract_requerimento_num$regex) %>% lapply(function(x)(preprocess_requerimentos(x))),
                          list(0))) %>%
        dplyr::select(-requerimento)

      return(df)

    }
  }
  return (tibble::frame_data(~ comissao, ~ cod_reuniao, ~ data, ~ hora, ~ local,
                             ~ estado, ~ tipo_materia, ~ titulo_reuniao, ~ objeto, ~ numero, ~ ano))

}

preprocess_requerimentos <- function(element){
  element <- dplyr::if_else(
    stringr::str_detect(element, stringr::regex('/[0-9]{4}')),
    sub('/[0-9]{2}', '/', element),
    element)

  element <- gsub(" ","", element)

  return (element)
}

readXML <- function(url) {
  out <- tryCatch({
    XML::xmlParse(url) %>%
      XML::xmlToList()
  },
  error=function(cond) {
    message(paste("Request returned Error 503 Service Unavailable. Please try again later."))
    return(NA)
  },
  warning=function(cond) {
    message(paste("Request caused a warning:", url))
    message(cond)
    return(NULL)
  }
  )
  return(out)
}
