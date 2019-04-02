senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
camara_env <- jsonlite::fromJSON(here::here("R/config/environment_camara.json"))

source(here::here("R/camara_analyzer.R"))

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
    df <- extract_links_proposicao_camara(id)
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

#' @title Remove a quebra de linha no fim de uma string
#' @description Recebe uma string e remove a quebra de linha no fim de uma string.
#' @param string String a ser processada
#' @return string processada
#' @examples
#' remove_line_break("emenda\r\n")
remove_line_break <- function(string) {
  return(string %>% 
    stringr::str_replace_all("\r\n$", ""))
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
    new_url <- dplyr::case_when(
      stringr::str_detect(attribute$meta$content, "http.*") ~ stringr::str_extract(attribute$meta$content, "http.*") %>% 
        stringr::str_remove("&altura=.*"),
      stringr::str_detect(attribute$meta$content, "/internet.*") ~ paste0("https://www.camara.leg.br", stringr::str_extract(attribute$meta$content, "/internet.*")))
      
    
    if(!is.na(new_url) && length(new_url) > 0) {
      url <- new_url
    }
  }
  return(url)
}

#' @title Extrai os links quando as proposições podem ter sido modificadas
#' @description Obtém a data e o link para o arquivo em pdf do texto da proposição
#' @param df Dataframe da tramitação na Câmara.
#' @return Dataframe contendo a data da versão e o link para o arquivo pdf
#' @examples
#' extract_links_proposicao_camara(2121442)
extract_links_proposicao_camara <- function(id) {
  tipo_sigla <- camara_env$sigla_tipo_relacionadas %>% 
    tibble::as_tibble()
  
  relacionadas <- rcongresso::fetch_relacionadas(id) %>%
    dplyr::filter(siglaTipo %in% tipo_sigla$sigla)
  
  if(nrow(relacionadas) == 0) {
    return(df <- dplyr::tribble(
      ~ id_proposicao, ~ casa, ~ data, ~ tipo_texto, ~ descricao, ~ link_inteiro_teor))
  }
  
  relacionadas <- relacionadas %>% 
    dplyr::select(id_documento = id)

  df <- purrr::map_df(.x = relacionadas$id_documento, ~ rcongresso::fetch_proposicao_camara(.x)) %>% 
    dplyr::filter(!is.na(urlInteiroTeor)) %>% 
    dplyr::select(-id) %>% 
    dplyr::mutate(id_proposicao = id,
                  casa = "camara",
                  data = as.Date(dataApresentacao, format = '%Y-%m-%d'),
                  descricaoTipo = remove_line_break(descricaoTipo),
                  descricao = dplyr::if_else(is.na(ementa) | ementa == '',
                                             descricaoTipo,
                                             ementa) %>% 
                    remove_line_break()) %>%
    dplyr::select(id_proposicao,
                  casa,
                  data,
                  tipo_texto = descricaoTipo,
                  descricao,
                  link_inteiro_teor = urlInteiroTeor) %>% 
    dplyr::distinct()
  
  if(nrow(df) > 0) {
    df <- df %>%
      dplyr::rowwise() %>% 
      dplyr::mutate(link_inteiro_teor = get_redirected_url(link_inteiro_teor))
  } else {
    df <- dplyr::tribble(
      ~ id_proposicao, ~ casa, ~ data, ~ tipo_texto, ~ descricao, ~ link_inteiro_teor)
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
      dplyr::mutate(DescricaoTexto = dplyr::if_else(is.na(DescricaoTexto) || DescricaoTexto == "-",
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
                                                "//Texto"))
  
  if (nrow(textos_df) == 0 ) {
    return(dplyr::tribble(
      ~ id_votacao, ~ casa, ~ data, ~ tipo_texto, ~ descricao, ~ link_inteiro_teor))
  }
  
  textos_df <- textos_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(id_proposicao = id,
                  casa = "senado") %>%
    mutate_links() %>%
    filter_links()
  
  if(nrow(textos_df) == 0) {
    return(dplyr::tribble(
      ~ id_votacao, ~ casa, ~ data, ~ tipo_texto, ~ descricao, ~ link_inteiro_teor))
    
  } else{
    textos_df <- textos_df %>%
      dplyr::select(id_proposicao,
                    casa,
                    data = DataTexto,
                    tipo_texto = DescricaoTipoTexto,
                    descricao = DescricaoTexto,
                    link_inteiro_teor = UrlTexto) %>% 
      dplyr::mutate(data = as.Date(data))
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
