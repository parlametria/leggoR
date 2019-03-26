senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
camara_env <- jsonlite::fromJSON(here::here("R/config/environment_camara.json"))

source("R/camara_analyzer.R")

#' @title Extrai os links quando as proposições podem ter sido modificadas
#' @description Obtém a data e o link para o arquivo em pdf do texto da proposição
#' @param id ID da proposição.
#' @param casa casa
#' @return Dataframe contendo a data da versão e o link para o arquivo pdf
#' @examples
#' extract_links_proposicao(46249, 'camara')
#' @export
extract_links_proposicao <- function(id, casa) {
  if(casa == 'camara') {
    proposicao_df = rcongresso::fetch_proposicao_camara(id)
    tramitacao_df = rcongresso::fetch_tramitacao_camara(id)
    df <- extract_links_proposicao_camara(proposicao_df, tramitacao_df)
  } else if(casa == 'senado') {
    df <- extract_links_proposicao_senado(id)
  }
  
  return(df %>% extract_initial_page_from_link())
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
    new_url <- stringr::str_extract(attribute$meta$content, "http.*") %>%
      stringr::str_remove("&altura=.*")
    
    if(length(new_url) > 0) {
      url <- new_url
    }
  }
  
  return(url)
}

#' @title Processa dados de um proposição da câmara.
#' @description Recebido um dataframe com a tramitação, a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/camara.
#' @param tramitacao_df Dataframe com tramitação da proposição
#' @importFrom magrittr %>%
process_proposicao_camara <- function(tramitacao_df) {
  proc_tram_df <- tramitacao_df %>%
    extract_events_in_camara() %>% 
    extract_locais_in_camara() %>%
    refact_date() %>%
    sort_by_date()
  
  return(proc_tram_df)
}

#' @title Extrai os links quando as proposições podem ter sido modificadas
#' @description Obtém a data e o link para o arquivo em pdf do texto da proposição
#' @param df Dataframe da tramitação na Câmara.
#' @return Dataframe contendo a data da versão e o link para o arquivo pdf
#' @examples
#' extract_links_proposicao_camara(rcongresso::fetch_proposicao(46249), rcongresso::fetch_tramitacao(46249, 'camara'))
extract_links_proposicao_camara <- function(proposicao_df, tramitacao_df) {
  casa <- "camara"
  tramitacao_df <- 
    tramitacao_df %>% 
    dplyr::filter(!is.na(url)) %>% 
    dplyr::mutate(data_hora = lubridate::ymd_hm(stringr::str_replace(data_hora,'T',' ')),
                  casa = casa,
                  id_situacao = as.integer(cod_tipo_tramitacao)) %>%
    dplyr::select(prop_id = id_prop,
                  casa,
                  data_hora,
                  sequencia,
                  texto_tramitacao = despacho,
                  sigla_local = sigla_orgao,
                  id_situacao,
                  descricao_situacao,
                  link_inteiro_teor = url)
  df <- 
    process_proposicao_camara(tramitacao_df) %>%  
    dplyr::filter(stringr::str_detect(evento, camara_env$versoes_texto_proposicao$eventos_regex)) %>% 
    dplyr::select(prop_id,
                  casa,
                  data_hora,
                  texto_tramitacao,
                  link_inteiro_teor)
  emendas <- rcongresso::fetch_emendas(proposicao_df$id, casa, proposicao_df$siglaTipo, proposicao_df$numero, proposicao_df$ano) %>% 
    
    dplyr::mutate(prop_id = proposicao_df$id,
                  casa = "camara") %>%
    dplyr::select(prop_id,
                  casa,
                  data_hora = data_apresentacao,
                  texto_tramitacao = inteiro_teor,
                  codigo_emenda)
  
  if(nrow(emendas) > 0) {
    emendas$link_inteiro_teor <- do.call("rbind", lapply(emendas$codigo_emenda, get_emendas_links))
    
    df <- df %>%
      rbind(emendas %>%
              select(-codigo_emenda))
  }
  
  if(nrow(df) > 0) {
    df <- df %>%
      dplyr::select(id_proposicao = prop_id,
                    casa,
                    data = data_hora,
                    descricao = texto_tramitacao,
                    link_inteiro_teor) %>%
      dplyr::mutate(descricao =
                      stringr::str_remove(descricao,
                                          camara_env$versoes_texto_proposicao$remove_publicacao_regex))
    df <- df %>%
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
  if("DescricaoTexto" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(DescricaoTexto = dplyr::if_else(is.na(DescricaoTexto),
                                                    DescricaoTipoTexto,
                                                    DescricaoTexto))
  } else {
    df <- df %>%
      dplyr::mutate(DescricaoTexto = DescricaoTipoTexto)
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
  if(nrow(df) ==1 && df$id_proposicao == 41703) {
    return(df)
  } else if("NumeroEmenda" %in% names(df)) {
    df <- df %>%
      dplyr::filter(!is.na(NumeroEmenda) |
                      stringr::str_detect(tolower(DescricaoTipoTexto),
                                          senado_env$versoes_texto_proposicao$tipos_texto_regex))
  } else {
    df <- df %>%
      dplyr::filter(stringr::str_detect(tolower(DescricaoTipoTexto),
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
      dplyr::select(-descricao_tipo_texto) %>% 
      dplyr::mutate(data = as.POSIXct(data))
    return(textos_df)
  }
}

#' @title Extrai as páginas iniciais de um link
#' @description Obtém a página onde o texto de interesse começa a partir da URL. Caso não haja detecção da página, a default será 1.
#' @param df Dataframe contendo o link para o texto
#' @return Dataframe contendo uma nova nova coluna chamada pagina_inicial com a página extraída da URL.
#' @examples
#' extract_initial_page_from_link(df)
extract_initial_page_from_link <- function(df) {
  df <-
    df %>%
    dplyr::mutate(pagina_inicial =
                    dplyr::case_when(stringr::str_detect(tolower(link_inteiro_teor), 'txpagina=\\d.*') ~
                                       stringr::str_extract(tolower(link_inteiro_teor), 'txpagina=\\d*'),
                                     TRUE ~ '1'),
                  pagina_inicial = stringr::str_extract(pagina_inicial, '\\d.*'))
  return(df)
}

#' @title Extrai informações e link das emendas de uma proposição
#' @description Extrai informações e link das emendas de uma proposição
#' @param proposicao_df Dataframe da proposição
#' @return Dataframe contendo informações sobre as emendas de uma proposição
#' @examples
#' get_emendas_links(577691)
get_emendas_links <- function(id_emenda) {
  return(rcongresso::fetch_proposicao_camara(id_emenda)$urlInteiroTeor)
}
