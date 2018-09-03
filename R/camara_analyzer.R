source(here::here("R/camara-lib.R"))
#' @title Cria coluna com os relatores na tramitação na Câmara
#' @description Cria uma nova coluna com os relatores na Câmara. O relator é adicionado à coluna no
#' envento pontual em que ele é designado
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "relator" adicionada.
#' @examples
#' tramitacao %>% extract_relator_in_camara()
extract_relator_in_camara <- function(df) {
  df %>%
    dplyr::mutate(relator = dplyr::case_when(
      stringr::str_detect(tolower(despacho), '^designad. relat.r') ~
        stringr::str_extract(despacho, stringr::regex('dep.+', ignore_case=TRUE))))
}



#' @title Recupera o último relator na Câmara
#' @description Recupera o nome do último relator na Câmara
#' @param df Dataframe da tramitação na Câmara
#' @return String do nome do último relator na Câmara
#' @examples
#' tramitacao %>% extract_last_relator_in_camara()
extract_last_relator_in_camara <- function(df) {
  relatores <- extract_relator_in_camara(df)
  relator <-
    relatores %>%
    dplyr::filter(!is.na(relator)) %>%
    dplyr::arrange(desc(data_hora)) %>%
    dplyr::select(relator)
  
  relator$relator[1]
}

#' @title Busca os últimos n eventos da tramitação na Câmara
#' @description Recupera os útimos n eventos da tramitação na Câmara, caso nenhuma quantidade seja informada, assume-se que é 1
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe dos últimos n eventos na Câmara contendo hora e evento.
extract_last_n_events_in_camara <- function(df, num) {
  df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::arrange(data_hora) %>%
    tail(n = num) %>%
    dplyr::select(data_hora, evento)
}

#' @title Recupera relatores de uma proposição
#' @description Recupera todos os relatores de uma proposição, junto com suas informações de parlamentar e comissão
#' @param tramitacao_df Dataframe da tramitação na Câmara
#' @return Dataframe que contém todos os relatores
extract_relatorias_in_camara <- function(tramitacao_df) {
  tramitacao_df %>%
    # extract line when a relator is designated by the code
    dplyr::filter(id_tipo_tramitacao == '320') %>%
    # select columns
    dplyr::select(
      data_hora,
      despacho,
      sigla_orgao
    ) %>%
    tibble::add_column() %>%
    # extract relator's name and partido
    dplyr::mutate(
      nome_parlamentar = stringr::str_match(despacho,'Dep. (.*?) [(]')[,2],
      partido = stringr::str_match(despacho,'[(](.*?)[)]')[,2]
    ) %>%
    # remove despacho column and return
    dplyr::select(-c(despacho))
}

#' @title Renomeia as colunas do dataframe
#' @description Renomeia as colunas do dataframe usando o padrão de letras minúsculas e underscore
#' @param df Dataframe
#' @return Dataframe com as colunas renomeadas.
rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

#' @title Extrai os eventos importantes que aconteceram na Câmara
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram na Câmara
#' @param tramitacao_df Dataframe da tramitação na Câmara
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
extract_events_in_camara <- function(tramitacao_df) {
  tramitacao_df %>% regex_left_match(camara_codes$eventos, "evento")
}

#' @title Recupera o autor de uma proposição na Câmara
#' @description Retorna um dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor
#' @param prop_id ID da proposição
#' @return Dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor.
#' @examples
#' extract_autor_in_camara(2121442)
#' @export
extract_autor_in_camara <- function(prop_id) {
  camara_exp <- 'câmara dos deputados'
  senado_exp <- 'senado federal'
  
  url_base_autores <- 'https://dadosabertos.camara.leg.br/api/v2/proposicoes/'
  url <- paste0(url_base_autores, prop_id, '/autores')
  json_voting <- jsonlite::fromJSON(url, flatten = T)
  
  authors <- json_voting %>%
    magrittr::extract2("dados") %>%
    dplyr::rename(
      autor.uri = uri,
      autor.nome = nome,
      autor.tipo = tipo,
      autor.cod_tipo = codTipo) %>%
    dplyr::mutate(casa_origem = 
                    dplyr::case_when(
                      stringr::str_detect(tolower(autor.nome), camara_exp) | autor.tipo == 'Deputado' ~ 'Câmara dos Deputados',
                      stringr::str_detect(tolower(autor.nome), senado_exp) | autor.tipo == 'Senador' ~ 'Senado Federal',
                      autor.cod_tipo == 40000 ~ 'Senado Federal',
                      autor.cod_tipo == 2 ~ 'Câmara dos Deputados'))
  
  partido_estado <- extract_partido_estado_autor(authors$autor.uri %>% tail(1))
  
  authors %>%
    dplyr::mutate(autor.nome = paste0(autor.nome, " ", partido_estado))
}

#' @title Recupera os locais da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada local
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada local
#' @examples
#'  extract_locais_in_camara(fetch_tramitacao(91341))
extract_locais_in_camara <- function(df) {
  descricoes_plenario <- c('votação', 'pronta para pauta', 'apresentação de proposição', 'sessão deliberativa')
  descricoes_comissoes <- c('recebimento pela')
  
  df %<>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(
          (tolower(despacho) %in% descricoes_plenario & sigla_orgao == 'PLEN' |
             stringr::str_detect(tolower(descricao_tramitacao), '^votação')) ~ 'Plenário',
          (stringr::str_detect(tolower(despacho), '^recebimento pela') |
             tolower(despacho) %in% descricoes_comissoes) & 
            sigla_orgao != 'CCP' &
            !stringr::str_detect(tolower(sigla_orgao), '^s') ~ sigla_orgao,
          tolower(descricao_tramitacao) == 'remessa ao senado federal' ~ 'Câmara')
    ) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(stringr::str_detect(local, "^PL") ~ "Comissão Especial",
                         TRUE ~ local)
    )
  
  if (is.na(df[1, ]$local)) {
    df[1, ]$local = 'CD-MESA-PLEN'
  }
  
  df %>%
    tidyr::fill(local)
}

extract_evento_in_camara <- function(df) {
  camara_codes <- get_environment_camara_json()
  eventos <- camara_codes$eventos
  novo_despacho_regex <- eventos$regex$novo_despacho
  redistribuicao_regex <- eventos$regex$redistribuicao
  redistribuicao_text <- eventos$text$distribuicao %>% tolower()
  df %>%
    dplyr::mutate(evento =
             case_when((str_detect(
               tolower(despacho),
               stringr::regex(redistribuicao_regex, ignore_case = TRUE)
             ) |
               str_detect(
                 tolower(despacho),
                 stringr::regex(novo_despacho_regex, ignore_case = TRUE)
               )) &
               tolower(descricao_tramitacao) == redistribuicao_text ~ "redistribuicao"
             ))
}

#' @title Recupera as casas da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada casa
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada casa
#' @examples
#'  extract_fase_casa_in_camara(fetch_tramitacao(91341))
extract_fase_casa_in_camara <- function(df) {
  descricoes_plenario <- c('votação', 'pronta para pauta', 'apresentação de proposição', 'sessão deliberativa')
  descricoes_comissoes <- c('recebimento pela')
  
  df <- df %>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      casa =
        dplyr::case_when(
          (tolower(despacho) %in% descricoes_plenario & sigla_orgao == 'PLEN' |
             stringr::str_detect(tolower(descricao_tramitacao), '^votação')) ~ 'Plenário',
          (stringr::str_detect(tolower(despacho), '^recebimento pela') | 
             tolower(despacho) %in% descricoes_comissoes) & sigla_orgao != 'CCP' & !stringr::str_detect(tolower(sigla_orgao), '^s') ~ "Comissões")
    )
  
  
  if (is.na(df[1, ]$casa)) {
    df[1, ]$casa <- 'Apresentação'
  }
  
  df %>%
    tidyr::fill(casa)
}

#' @title Recupera a situação em que a proposição se encontra na comissão na Câmara
#' @description Retorna o dataframe da tramitação com a coluna situacao_comissao adicionada (ex. recebimento, encaminhamento)
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitação contendo a coluna situacao_comissao
#' @examples
#'  extract_situacao_comissao(process_proposicao_camara(345311))
extract_situacao_comissao <- function(df) {
  
  situacao_comissao <- camara_codes$situacao_comissao
  situacao_comissao['local'] <- get_regex_comissoes_camara()
  
  df %>%
    regex_left_match(situacao_comissao, "situacao_comissao") %>%
    tidyr::fill(situacao_comissao)
}


#' @title Processa dados de um proposição da câmara.
#' @description Recebido um dataframe com a tramitação, a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/camara.
#' @param tramitacao_df Dataframe com tramitação da proposição
#' @importFrom magrittr %>%
process_proposicao_camara_df <- function(proposicao_df, tramitacao_df) {
  tramitacao_df %>%
    rename_df_columns %>%
    extract_events_in_camara() %>%
    extract_locais_in_camara() %>%
    extract_fase_casa_in_camara() %>%
    extract_situacao_comissao() %>%
    refact_date() %>%
    sort_by_date()
}

#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao_camara(id) %>%
    rename_df_columns
  
  df[,!sapply(df, is.list)]
}

extract_forma_apreciacao_camara <- function(prop_id) {
  base_url <-
    'http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao='
  regex_apreciacao <-
    tibble::frame_data(
      ~ forma_apreciacao,
      ~ regex,
      'Conclusiva',
      'Sujeita à Apreciação Conclusiva pelas Comissões',
      'Plenário',
      'Sujeita à Apreciação do Plenário'
    )
  
  page_df <- data.frame(page_url = paste0(base_url, prop_id), stringsAsFactors = F)
  
  apreciacao_df <- page_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(page_html = list(xml2::read_html(page_url))) %>%
    dplyr::mutate(temp =
                    rvest::html_node(page_html, '#informacoesDeTramitacao') %>%
                    rvest::html_text()) %>%
    fuzzyjoin::regex_left_join(regex_apreciacao, by = c(temp = "regex"))
  
  return(apreciacao_df[1,]$forma_apreciacao)
}


extract_regime_tramitacao_camara <- function(tram_df) {
  regex_regime <-
    tibble::frame_data(
      ~ regime_tramitacao,
      ~ regex,
      'Ordinária',
      'Ordinária',
      'Prioridade',
      'Prioridade',
      'Urgência',
      'Urgência'
    )
  
  prop_id <- tram_df[1,]$id_prop
  
  regime_df <- rcongresso::fetch_proposicao(prop_id) %>%
    fuzzyjoin::regex_left_join(regex_regime, 
                               by = c(statusProposicao.regime = "regex"))
    return(regime_df[1,]$regime_tramitacao)
}
