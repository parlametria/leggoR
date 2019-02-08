senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
camara_env <- jsonlite::fromJSON(here::here("R/config/environment_camara.json"))
congresso_env <- jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))
congress_constants <- congresso_env$constants

#' @title Extrai os links quando as proposições podem ter sido modificadas
#' @description Obtém a data e o link para o arquivo em pdf do texto da proposição
#' @param id ID da proposição.
#' @param casa casa
#' @return Dataframe contendo a data da versão e o link para o arquivo pdf
#' @examples 
#' extract_links_proposicao(46249, 'camara')
#' @export
extract_links_proposicao <- function(id, casa) {
  if(tolower(casa) == congress_constants$camara_label) {
    proposicao_df = agoradigital::fetch_proposicao(id, casa, '', '', T, F)
    tramitacao_df = rcongresso::fetch_tramitacao(id, casa)
    return(extract_links_proposicao_camara(proposicao_df, tramitacao_df))
    
  } else if(tolower(casa) == congress_constants$senado_label) {
    return(extract_links_proposicao_senado(id))
  }
}

#' @title Checa se a uma requisição possui redirecionamento
#' @description Recupera a url de redirecionamento de um link (quando houver) na Câmara. Caso não haja, a url retornada será a original.
#' @param content Conteúdo da requisição à URL 
#' @return url de redirecionamento (quando houver) ou url original.
#' @examples
#' url <- "http://www.camara.gov.br/proposicoesWeb/prop_mostrarintegra?codteor=1272135"
#' content <- httr::GET(url) %>% 
#' httr::content()
#' has_redirect_url(content)
has_redirect_url <- function(content) {
  if(length(content) > 1) {
    content = content[1]
  }
  if(class(content) != 'raw'){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' @title Recupera a url de redirecionamento de um link na Câmara
#' @description Recupera a url de redirecionamento de um link (quando houver) na Câmara. Caso não haja, a url retornada será a original.
#' @param url URL do link
#' @return url de redirecionamento (quando houver) ou url original.
#' @examples
#' url <- "http://www.camara.gov.br/proposicoesWeb/prop_mostrarintegra?codteor=1272135"
#' get_redirected_url(url)
get_redirected_url <- function(url) {
  content <- httr::GET(url) %>% 
    httr::content()
  
  if(has_redirect_url(content)) {
    content_list <- content %>% 
      xml2::as_list()
    attribute <- lapply(content_list$html$head, attributes)
    url <- stringr::str_extract(attribute$meta$content, "http.*") %>% 
      stringr::str_remove("&altura=.*")
  } 
  
  return(url)
}

#' @title Extrai os links quando as proposições podem ter sido modificadas
#' @description Obtém a data e o link para o arquivo em pdf do texto da proposição
#' @param df Dataframe da tramitação na Câmara.
#' @return Dataframe contendo a data da versão e o link para o arquivo pdf
#' @examples
#' extract_links_proposicao_camara(fetch_proposicao(46249, 'camara', 'PL do Veneno', 'Meio Ambiente', T, F), rcongresso::fetch_tramitacao(46249))
extract_links_proposicao_camara <- function(proposicao_df, tramitacao_df) {
  tramitacao_df <- 
    tramitacao_df %>% 
    dplyr::filter(!is.na(url)) %>% 
    dplyr::mutate(data_hora = lubridate::ymd_hm(stringr::str_replace(dataHora,'T',' ')),
                  casa = 'camara',
                  id_situacao = as.integer(codTipoTramitacao)) %>%
    dplyr::select(prop_id = id_prop,
                  casa,
                  data_hora,
                  sequencia,
                  texto_tramitacao = despacho,
                  sigla_local = siglaOrgao,
                  id_situacao,
                  descricao_situacao = descricaoSituacao,
                  link_inteiro_teor = url)
  df <- 
    agoradigital::process_proposicao(proposicao_df, tramitacao_df, casa, NULL) %>% 
    dplyr::filter(stringr::str_detect(evento, camara_env$versoes_texto_proposicao$eventos_regex))
  
  if(nrow(df) > 0) {
    df <- df %>%
      dplyr::select(id_proposicao = prop_id,
                    casa,
                    data = data_hora,
                    descricao = texto_tramitacao,
                    link_inteiro_teor) %>%
      dplyr::mutate(descricao = 
                      stringr::str_remove(descricao, 
                                          camara_env$versoes_texto_proposicao$remove_publicacao_regex)) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(link_inteiro_teor = get_redirected_url(link_inteiro_teor))
  } else {
    df <- dplyr::tribble(
      ~ id_proposicao, ~ casa, ~ data, ~ descricao, ~ link_inteiro_teor)
  }
  return(df)
}

#' @title Muda texto dos links de textos de interesse
#' @description  Muda texto dos links e retorna o dataframe contendo informações sobre os links dos textos de interesse (emendas, texto de apresentação e pareceres das comissões)
#' @param df Dataframe dos links
#' @return Dataframe dos links contendo coluna DescricaoTexto
#' @examples
#' mutate_links(textos_df)
mutate_links <- function(df) {
  if(typeof(df$DescricaoTexto) == 'NULL') {
    df <- df %>% 
      dplyr::mutate(DescricaoTexto = DescricaoTipoTexto)
  } else {
    df <- df %>% 
      dplyr::mutate(DescricaoTexto = dplyr::if_else(is.na(DescricaoTexto), 
                                                    DescricaoTipoTexto,
                                                    DescricaoTexto))
  }
  return(df)
}

#' @title Filtra os links de textos de interesse
#' @description Filtra e retorna o dataframe contendo informações sobre os links dos textos de interesse (emendas, texto de apresentação e pareceres das comissões)
#' @param df Dataframe dos links a serem filtrados
#' @return Dataframe dos links filtrados
#' @examples
#' filter_links(textos_df)
filter_links <- function(df) {
  if(nrow(df) ==1 & df$id_proposicao == 41703) {
    return(df)
  } else if(typeof(df$NumeroEmenda) == 'NULL') {
    df <- df %>% 
      dplyr::filter(stringr::str_detect(tolower(DescricaoTexto), 
                                        senado_env$versoes_texto_proposicao$tipos_texto_regex))
  }else {
    df <- df %>% 
      dplyr::filter(!is.na(NumeroEmenda) | 
                      stringr::str_detect(tolower(DescricaoTexto), 
                                          senado_env$versoes_texto_proposicao$tipos_texto_regex))
  }
  return(df)
}

#' @title Extrai os links quando as proposições podem ter sido modificadas
#' @description Obtém a data e o link para o arquivo em pdf do texto da proposição
#' @param id Id da proposição no Senado
#' @return Dataframe contendo a data da versão e o link para o arquivo pdf
#' @examples
#' extract_links_proposicao_senado(127753)
extract_links_proposicao_senado <- function(id) {
  url = paste0('http://legis.senado.leg.br/dadosabertos/materia/textos/', id)
  
  textos_df <- 
    XML::xmlToDataFrame(nodes = XML::getNodeSet(XML::xmlParse(RCurl::getURL(url)),
                                                "//Texto")) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(id_proposicao = id,
                  casa = "senado") %>% 
    mutate_links() %>% 
    filter_links()
  
  if(nrow(textos_df) == 0) {
    return(dplyr::tribble(
      ~ id_votacao, ~ casa, ~ data, ~ descricao, ~ link_inteiro_teor))
    
  } else{
    textos_df <- textos_df %>% 
      dplyr::select(id_proposicao,
                    casa,
                    descricao_tipo_texto = DescricaoTipoTexto,
                    data = DataTexto,
                    descricao = DescricaoTexto, 
                    link_inteiro_teor = UrlTexto) %>% 
      dplyr::select(-descricao_tipo_texto)
    return(textos_df)
  }
}
