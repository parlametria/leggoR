source(here::here("R/utils.R"))
source(here::here("R/agendas.R"))
senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
senado_constants <- senado_env$constants

fetch_json_try <- function(url) {
  count <- 0
  repeat {
    json_data <- NULL
    tryCatch({
      json_data <- jsonlite::fromJSON(url, flatten = T)
    },
    error = function(msg) {
    })
    if (!is.null(json_data) & is.null(json_data$ERROR)) {
      break
    } else {
      print("Erro ao baixar dados, tentando outra vez...")
      count <- count + 1
      print(paste("Tentativas: ", count))
      Sys.sleep(2)
    }
  }
  return(json_data)
}

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

#' @title Renomeia as colunas do dataframe passado para o formato underscore
#' @description Renomeia as colunas do dataframe usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe do Senado
#' @return Dataframe com as colunas renomeadas
#' @export
rename_table_to_underscore <- function(df) {
  new_names = names(df) %>%
    to_underscore()
  
  names(df) <- new_names
  
  df
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

#' @title Retorna um dataframe a partir de uma coluna com listas encadeadas
#' @description Retorna um dataframe a partir de uma coluna com listas encadeadas.
#' @param column Coluna
#' @return Dataframe com as informações provenientes de uma coluna com listas encadeadas.
#' @examples
#' generate_dataframe(column)
#' @export
generate_dataframe <- function (column) {
  as.data.frame(column) %>%
    tidyr::unnest() %>%
    rename_df_columns()
}


#' @title Retorna as emendas de uma proposição no Congresso
#' @description Retorna dataframe com os dados das emendas de uma proposição no Congresso.
#' @param bill_id ID de uma proposição do Congresso
#' @return Dataframe com as informações sobre as emendas de uma proposição no Congresso.
#' @examples
#' fetch_emendas(91341,'senado')
#' @export
fetch_emendas <- function(id, casa) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    emendas <- fetch_emendas_camara(id)
  } else if (casa == 'senado') {
    emendas <- fetch_emendas_senado(id)
  } else {
    print('Parâmetro "casa" não identificado.')
    return()
  }
  
  emendas  <-
    emendas %>%
    dplyr::mutate(prop_id = id, codigo_emenda = as.integer(codigo_emenda)) %>%
    dplyr::select(
      prop_id, codigo_emenda, data_apresentacao, numero, local, autor, casa, tipo_documento, inteiro_teor) 
}

#' @title Retorna as emendas de uma proposição no Senado
#' @description Retorna dataframe com os dados das emendas de uma proposição no Senado.
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as emendas de uma proposição no Senado.
#' @examples
#' fetch_emendas_senado(91341)
fetch_emendas_senado <- function(bill_id) {
  url_base_emendas <-
    "http://legis.senado.leg.br/dadosabertos/materia/emendas/"
  url <- paste0(url_base_emendas, bill_id)
  
  json_emendas <- fetch_json_try(url)
  
  emendas_data <- json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia")
  
  emendas_df <- emendas_data %>%
    magrittr::extract2("Emendas") %>%
    purrr::map_df( ~ .) %>% rename_df_columns()
  
  num_emendas = nrow(emendas_df)
  
  if (num_emendas == 0) {
    emendas_df <-
      tibble::frame_data( ~ codigo_emenda, ~ data_apresentacao, ~ numero, ~ local, ~ autor, ~ partido, ~ casa, ~ tipo_documento, ~ inteiro_teor)

  } else if (num_emendas == 1) {
    texto <- generate_dataframe(emendas_df$textos_emenda) %>%
      dplyr::select(tipo_documento, url_texto)
    
    autoria <- generate_dataframe(emendas_df$autoria_emenda) %>%
      dplyr::mutate(
        partido = paste0(
          identificacao_parlamentar_sigla_partido_parlamentar,
          "/",
          identificacao_parlamentar_uf_parlamentar
        )
      )
    
    emendas_df <- emendas_df %>%
      plyr::rename(
        c(
          "numero_emenda" = "numero",
          "colegiado_apresentacao" = "local"
        )
      ) %>%
      dplyr::mutate(autor = autoria$nome_autor,
                    partido = autoria$partido,
                    tipo_documento = texto$tipo_documento,
                    inteiro_teor = texto$url_texto,
                    casa = 'senado') 
    
    
  } else{
    emendas_df <- emendas_df %>%
      tidyr::unnest() %>%
      plyr::rename(
        c(
          "numero_emenda" = "numero",
          "colegiado_apresentacao" = "local",
          "autoria_emenda_autor_nome_autor" = "autor",
          "textos_emenda_texto_emenda_url_texto" = "inteiro_teor",
          "textos_emenda_texto_emenda_tipo_documento" = "tipo_documento",
          "autoria_emenda_autor_identificacao_parlamentar_sigla_partido_parlamentar" = "partido",
          "autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar" = "uf"
        )
      ) %>%
      dplyr::mutate(
        partido = paste0(partido, "/", uf),
        casa = "senado"
      ) 

  }

  emendas_df %>%
    dplyr::mutate(autor = paste0(autor, " ", partido), 
                  numero = as.integer(numero),
                  tipo_documento = as.character(tipo_documento),
                  inteiro_teor = as.character(inteiro_teor)) %>%
    dplyr::select(-partido)

}

#' @title Retorna as emendas de uma proposição na Camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
#' @param id ID de uma proposição da Camara
#' @param sigla Sigla da proposição
#' @param numero Numero da proposição
#' @param ano Ano da proposição
#' @return Dataframe com as informações sobre as emendas de uma proposição na Camara
#' @examples
#' fetch_emendas_camara(408406)
fetch_emendas_camara <- function(id=NA, sigla="", numero="", ano="") {
  if(is.na(id)) {
    url <- 
      paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal?tipo=', sigla, '&numero=', numero, '&ano=', ano)
  }else {
    prop <- fetch_proposicao(id, 'camara')
    url <- 
      paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal?tipo=', prop$tipo_materia, '&numero=', prop$numero, '&ano=', prop$ano)
  }
 
   eventos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    eventos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    magrittr::extract2('Emendas') %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame()
  
  if(nrow(df) == 0) {
    return(tibble::frame_data( ~ codigo_emenda, ~ data_apresentacao, ~ numero, ~ local, ~ autor, ~ casa, ~ tipo_documento, ~ inteiro_teor))
  }
  
  new_names <- c("cod_proposicao", "descricao")
  names(df) <- new_names
  
  emendas <- purrr::map_df(df$cod_proposicao, fetch_emendas_camara_auxiliar)
  normalizes_names <- c("codigo_emenda", "data_apresentacao", "numero", "local", "autor", "casa", "tipo_documento", "inteiro_teor")
  names(emendas) <- normalizes_names
  
  emendas %>%
    dplyr::mutate(data_apresentacao = as.character(as.Date(data_apresentacao)))
}

#' @title Função auxiliar para o fetch_emendas_camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
fetch_emendas_camara_auxiliar <- function(id) {
  fetch_proposicao(id, "camara", normalized = T, emendas = T) %>%
    dplyr::select(c(prop_id, data_apresentacao, numero, status_proposicao_sigla_orgao, autor_nome, casa, tipo_materia, ementa))
}

build_data_filepath <- function(folder_path,data_prefix,house,bill_id) {
  filename <- paste0(paste(bill_id,data_prefix,house, sep='-'),'.csv')
  filepath <- paste(folder_path, house, filename, sep='/')
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

#' @title Baixa os órgãos na câmara
#' @description Retorna um dataframe contendo os órgãos da câmara
#' @return Dataframe contendo os órgãos da Câmara
#' @importFrom RCurl getURL
fetch_orgaos_camara <- function(){
  url <- RCurl::getURL('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterOrgaos')
  
  orgaos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    orgaos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame()
  
  names(df) <- c("orgao_id", "tipo_orgao_id", "sigla", "descricao")
  
  return(df)
}


#' @title Baixa todas as siglas das comissões atuais do Senado
#' @description Retorna um dataframe contendo as siglas das comissões atuais do Senado
#' @return Dataframe
#' @examples
#' fetch_orgaos_senado()
#' @importFrom RCurl getURL
#' @importFrom dplyr %>%
fetch_orgaos_senado <- function() {
  url <- 'http://legis.senado.leg.br/dadosabertos/dados/'
  
  url_comissoes_permanentes <- RCurl::getURL(paste0(url, 'ComissoesPermanentes.xml'))
  
  url_comissoes_temporarias <- RCurl::getURL(paste0(url, 'ComissoesTemporarias.xml'))
  
  comissoes_permanentes_df <- 
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_permanentes), 
      "//Colegiado")) %>% 
    dplyr::select(sigla = SiglaColegiado)
  
  comissoes_temporarias_df <- 
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_temporarias), 
      "//Colegiado")) %>% 
    dplyr::select(sigla = SiglaColegiado)
  
  url <- 'https://www.congressonacional.leg.br/dados/comissao/lista/'
  
  url_comissoes_mistas <- RCurl::getURL(paste0(url, 'mistas'))
  
  comissoes_mistas_df <- 
    XML::xmlToDataFrame(nodes = XML::getNodeSet(XML::xmlParse(url_comissoes_mistas), 
                                                "//IdentificacaoComissao")) %>% 
    dplyr::select(sigla = SiglaComissao)
  
  df <-
    rbind(comissoes_permanentes_df, comissoes_mistas_df) %>% 
    rbind(comissoes_mistas_df) %>% 
    dplyr::distinct()
  
  df <-
    df %>% dplyr::filter(!stringr::str_detect(sigla, '^CMMPV'))
  
  return(df)
}


