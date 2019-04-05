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
    
    if(!is.na(new_url) && length(new_url) > 0) {
      url <- new_url
    }
  }
  
  return(url)
}

#' @title Recupera as emendas de uma proposição com seus respectivos links para inteiro teor
#' @description Recupera as emendas de uma proposição com seus respectivos links para inteiro teor na Câmara.
#' @param proposicao_df Dataframe da proposição
#' @return dataframe contendo informações das emendas e link para o inteiro teor
#' @examples
#' fetch_emendas(rcongresso::fetch_proposicao_camara(2121442))
fetch_emendas <- function(proposicao_df) {
  casa <- "camara"
  
  emendas <- tryCatch({
    rcongresso::fetch_emendas(proposicao_df$id, 
                                casa, 
                                proposicao_df$siglaTipo, 
                                proposicao_df$numero, 
                                proposicao_df$ano) %>%
      dplyr::filter(tipo_documento %in% camara_env$sigla_tipo_relacionadas$sigla) %>% 
      dplyr::left_join(camara_env$sigla_tipo_relacionadas, 
                       by = c("tipo_documento" = "sigla")) %>%
      dplyr::mutate(id_proposicao = proposicao_df$id,
                    casa = casa,
                    data = as.Date(data_apresentacao, 
                                   format = '%Y-%m-%d'),
                    descricao = 
                      dplyr::if_else(
                        !is.na(inteiro_teor) & inteiro_teor != "",
                        inteiro_teor,
                        tipo)) %>% 
      dplyr::select(id_proposicao,
                    casa,
                    data,
                    tipo_texto = tipo,
                    descricao,
                    codigo_emenda)
    
    }, error = function(e) {
      data <- 
        dplyr::tribble(~ id_proposicao, ~ casa, ~ data, ~ tipo_texto, ~ descricao, ~ link_inteiro_teor)
      
      return(data)
  })
  
  if(nrow(emendas) == 0) {
    return(
      dplyr::tribble(~ id_proposicao, ~ casa, ~ data, ~ tipo_texto, ~ descricao, ~ link_inteiro_teor))
  } 
  
  emendas$link_inteiro_teor <- 
    do.call("rbind",
            lapply(emendas$codigo_emenda, get_emendas_links))
  
  return(emendas %>%
           select(-codigo_emenda))
}

#' @title Processa dados de um proposição da câmara.
#' @description Recebido um dataframe com a tramitação, a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/camara.
#' @param tramitacao_df Dataframe com tramitação da proposição
#' @importFrom magrittr %>%
process_proposicao_camara <- function(tramitacao_df) {
  tramitacao_df <- 
    tramitacao_df %>% 
    dplyr::filter(!is.na(url)) %>% 
    dplyr::mutate(data_hora = as.Date(data_hora, format = '%Y-%m-%dT%H:%M'),
                  casa = "camara",
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
  
  proc_tram_df <- tramitacao_df %>%
    extract_events_in_camara() %>% 
    extract_locais_in_camara() %>%
    sort_by_date()
  
  return(proc_tram_df)
}

#' @title Processa dados de tipo_texto de uma proposição da câmara.
#' @description Recebido um dataframe com a tramitação, a função adiciona uma coluna chamada tipo_texto 
#' a partir do tipo do evento
#' @param tramitacao_df Dataframe com tramitação da proposição
#' @importFrom magrittr %>%
add_tipo_texto <- function(df) {
  return(df %>% 
           dplyr::mutate(tipo_texto = dplyr::case_when(
             stringr::str_detect(evento, 'apresentacao_pl') ~ 'Apresentação de Proposição',
             stringr::str_detect(evento, "parecer_comissao") ~ "Parecer da Comissão"
           )))
}

#' @title Extrai os links quando as proposições podem ter sido modificadas
#' @description Obtém a data e o link para o arquivo em pdf do texto da proposição
#' @param df Dataframe da tramitação na Câmara.
#' @return Dataframe contendo a data da versão e o link para o arquivo pdf
#' @examples
#' extract_links_proposicao_camara(rcongresso::fetch_proposicao_camara(46249),
#' rcongresso::fetch_tramitacao_camara(46249))
extract_links_proposicao_camara <- function(proposicao_df, tramitacao_df) {
  casa <- "camara"
  
  df <- 
    process_proposicao_camara(tramitacao_df) %>%  
    dplyr::filter(
      stringr::str_detect(evento, 
                          camara_env$versoes_texto_proposicao$eventos_regex)) %>% 
    add_tipo_texto() %>% 
    dplyr::select(id_proposicao =prop_id,
                  casa,
                  data = data_hora,
                  tipo_texto,
                  descricao = texto_tramitacao,
                  link_inteiro_teor) %>% 
    dplyr::bind_rows(fetch_emendas(proposicao_df))
  
  if(nrow(df) > 0) {
    df <- df %>%
      dplyr::mutate(
        descricao = 
          stringr::str_remove(descricao,
                              camara_env$versoes_texto_proposicao$remove_publicacao_regex))
    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(link_inteiro_teor = get_redirected_url(link_inteiro_teor))
  } else {
    df <- dplyr::tribble(
      ~ id_proposicao, ~ casa, ~ data, ~tipo_texto, ~ descricao, ~ link_inteiro_teor)
  }
  
  return(df)
}

#' @title Muda texto dos links de textos de interesse
#' @description  Muda texto dos links e retorna o dataframe contendo informações sobre os 
#' links dos textos de interesse (emendas, texto de apresentação e pareceres das comissões)
#' @param df Dataframe dos links
#' @return Dataframe dos links contendo coluna DescricaoTexto
#' @examples
#' mutate_links(textos_df)
mutate_links <- function(df) {
  if("DescricaoTexto" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(
        DescricaoTexto = 
          dplyr::if_else(is.na(DescricaoTexto) || DescricaoTexto == "-",
                         DescricaoTipoTexto,
                         DescricaoTexto))
  } else {
    df <- df %>%
      dplyr::mutate(DescricaoTexto = DescricaoTipoTexto)
  }
  return(df)
}

#' @title Filtra os links de textos de interesse
#' @description Filtra e retorna o dataframe contendo informações 
#' sobre os links dos textos de interesse (emendas, texto de apresentação e pareceres das comissões)
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
  id_pls_526_1999 <- 41703
  
  url = paste0('http://legis.senado.leg.br/dadosabertos/materia/textos/', id)
  
  textos_df <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(XML::xmlParse(RCurl::getURL(url)),
                                                "//Texto"), stringsAsFactors = F)
  
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
  
  # Verifica se o id é o da PLS 526/1999, pois não possui link para o inteiro teor da matéria inicial
  if(id == id_pls_526_1999) {
    texto_inicial <- 
      dplyr::tribble(
        ~ id_proposicao, ~ casa, ~ data, ~ tipo_texto, ~ descricao, ~ link_inteiro_teor,
        id, "senado", as.Date("1999-08-31"), "Avulso inicial da matéria", "Avulso inicial da matéria", 
        "https://github.com/analytics-ufcg/versoes-de-proposicoes/raw/master/data/PL-veneno-avulso-inicial.pdf")
    
    textos_df <- texto_inicial %>% 
      dplyr::bind_rows(
        textos_df %>%
          dplyr::select(id_proposicao,
                        casa,
                        data = DataTexto,
                        tipo_texto = DescricaoTipoTexto,
                        descricao = DescricaoTexto,
                        link_inteiro_teor = UrlTexto) %>% 
          dplyr::mutate(data = as.Date(data)))
    return(textos_df)
  }
  
  if(nrow(textos_df) == 0) {
    return(dplyr::tribble(
      ~ id_proposicao, ~ casa, ~ data, ~ tipo_texto, ~ descricao, ~ link_inteiro_teor))
    
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
#' @description Obtém a página onde o texto de interesse começa a partir da URL. 
#' Caso não haja detecção da página, a default será 1.
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
