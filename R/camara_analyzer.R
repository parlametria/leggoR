source(here::here("R/camara-lib.R"))

camara_constants <- jsonlite::fromJSON(here::here("R/config/environment_camara.json"))

#' @title Cria coluna com os relatores na tramitação na Câmara
#' @description Cria uma nova coluna com os relatores na Câmara. O relator é adicionado à coluna no
#' envento pontual em que ele é designado
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "relator" adicionada.
#' @examples
#' fetch_tramitacao(2121442, 'camara', T) %>% extract_relator_in_camara()
extract_relator_in_camara <- function(df) {
  df %>%
    dplyr::mutate(relator = dplyr::case_when(
      stringr::str_detect(tolower(texto_tramitacao), '^designad. relat.r') ~
        stringr::str_extract(texto_tramitacao, stringr::regex('dep.+', ignore_case=TRUE))))
}



#' @title Recupera o último relator na Câmara
#' @description Recupera o nome do último relator na Câmara
#' @param df Dataframe da tramitação na Câmara
#' @return String do nome do último relator na Câmara
#' @examples
#' fetch_tramitacao(2121442, 'camara', T) %>% extract_last_relator_in_camara()
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
      texto_tramitacao,
      sigla_local
    ) %>%
    tibble::add_column() %>%
    # extract relator's name and partido
    dplyr::mutate(
      nome_parlamentar = stringr::str_match(texto_tramitacao,'Dep. (.*?) [(]')[,2],
      partido = stringr::str_match(texto_tramitacao,'[(](.*?)[)]')[,2]
    ) %>%
    # remove texto_tramitacao column and return
    dplyr::select(-c(texto_tramitacao))
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
  
  # authors <- authors %>%
  #   mutate(autor.nome = dplyr::if_else(casa_origem == 'Senado Federal', stringr::str_split(autor.nome,'-')[[2]], autor.nome))
  
  partido_estado <- extract_partido_estado_autor(authors$autor.uri %>% tail(1))
  
  authors %>%
    dplyr::mutate(autor.nome = paste0(autor.nome, " ", partido_estado))
}

#' @title Extrai as fases globais da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada global
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada global
#' @examples
#'  extract_fase_global_in_camara(fetch_tramitacao(2121442, 'camara', T), fetch_proposicao(2121442, 'camara', T))
extract_fase_global_in_camara <- function(tramitacao_df, proposicao_df) { 
  casa_name = if_else(tolower(proposicao_df$casa_origem) == "senado federal", "(Revisão)", "(Origem)")
  
  tramitacao_df %<>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      fase_global =
        dplyr::case_when(
          (stringr::str_detect(tolower(texto_tramitacao), camara_constants$plen_global$plenario) & 
             sigla_local == 'PLEN') ~ paste0("Plenário ", casa_name),
          sigla_local != 'PLEN' &
            (sigla_local %in% camara_constants$comissoes$siglas_comissoes_antigas |
               sigla_local %in% camara_constants$comissoes$siglas_comissoes |
               stringr::str_detect(tolower(sigla_local), '^pl'))  ~ paste0("Comissões ", casa_name)))
  
  tramitacao_df %>%
    tidyr::fill(fase_global)
}

#' @title Recupera o progresso de um PL na Câmara
#' @description Retorna um dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @param df Dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @return Dataframe contendo o id da PL, as fases globais, data de inicio, data de fim
#' @examples
#'  get_progresso_camara(fetch_tramitacao(2121442, 'camara', T), fetch_proposicao(2121442, 'camara', T))
get_progresso_camara <- function(tramitacao_df, proposicao_df) { 
  tramitacao_df <- 
    tramitacao_df %>%
    extract_fase_global_in_camara(proposicao_df)
  
  df <- 
    tramitacao_df %>%
    dplyr::filter(fase_global != 'NA') %>%
    dplyr::mutate(end_data = dplyr::lead(data_hora, default=Sys.time())) %>%
    dplyr::group_by(fase_global, sequence = data.table::rleid(fase_global)) %>%
    dplyr::summarise(data_hora_inicio = min(data_hora),
              data_hora_fim = max(end_data)) %>%
    dplyr::filter(data_hora_fim - data_hora_inicio > 0) %>%
    dplyr::select(-sequence) 
  
  if(nrow(df %>% dplyr::group_by(fase_global) %>% dplyr::filter(n()>1)) > 0) {
    df <- 
      df %>%
      dplyr::group_by(fase_global) %>%
      dplyr::summarise(data_hora_inicio = min(data_hora_inicio),
                data_hora_fim = max(data_hora_fim)) %>%
      dplyr::right_join(camara_constants$fases_global, by = "fase_global")
  } else {
    df <- 
      df %>%
      dplyr::right_join(camara_constants$fases_global, by = "fase_global")
  }
  
  df %>%
    dplyr::mutate(prop_id = proposicao_df$prop_id) %>%
    dplyr::select(prop_id, fase_global, casa, data_hora_inicio, data_hora_fim)
}

#' @title Recupera os locais da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada local
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada local
#' @examples
#'  extract_locais_in_camara(fetch_tramitacao(2121442, 'camara', T))
extract_locais_in_camara <- function(df) {
  descricoes_plenario <- c('votação', 'pronta para pauta', 'apresentação de proposição', 'sessão deliberativa')
  descricoes_comissoes <- c('recebimento pela')
  
  df %<>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(
          (tolower(texto_tramitacao) %in% descricoes_plenario & sigla_local == 'PLEN' |
             stringr::str_detect(tolower(texto_tramitacao), '^votação')) ~ 'Plenário',
          (stringr::str_detect(tolower(texto_tramitacao), '^recebimento pela') |
             tolower(texto_tramitacao) %in% descricoes_comissoes) & 
            sigla_local != 'CCP' &
            !stringr::str_detect(tolower(sigla_local), '^s') ~ sigla_local,
          tolower(texto_tramitacao) == 'remessa ao senado federal' ~ 'Câmara')
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

#' @title Recupera os eventos da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada evento
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada evento
#' @examples
#'  extract_evento_in_camara(fetch_tramitacao(2121442, 'camara', T))
extract_evento_in_camara <- function(df) {
  camara_codes <- get_environment_camara_json()
  eventos <- camara_codes$eventos
  novo_despacho_regex <- eventos$regex$novo_despacho
  redistribuicao_regex <- eventos$regex$redistribuicao
  redistribuicao_text <- eventos$text$distribuicao %>% tolower()
  df %>%
    dplyr::mutate(evento =
             case_when((str_detect(
               tolower(texto_tramitacao),
               stringr::regex(redistribuicao_regex, ignore_case = TRUE)
             ) |
               str_detect(
                 tolower(texto_tramitacao),
                 stringr::regex(novo_despacho_regex, ignore_case = TRUE)
               )) &
               tolower(texto_tramitacao) == redistribuicao_text ~ "redistribuicao"
             ))
}

#' @title Recupera as casas da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada casa
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada casa
#' @examples
#'  extract_fase_casa_in_camara(fetch_tramitacao(2121442, 'camara', T))
extract_fase_casa_in_camara <- function(df) {
  descricoes_plenario <- c('votação', 'pronta para pauta', 'apresentação de proposição', 'sessão deliberativa')
  descricoes_comissoes <- c('recebimento pela')
  
  df <- df %>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      casa =
        dplyr::case_when(
          (tolower(texto_tramitacao) %in% descricoes_plenario & sigla_local == 'PLEN' |
             stringr::str_detect(tolower(texto_tramitacao), '^votação')) ~ 'Plenário',
          (stringr::str_detect(tolower(texto_tramitacao), '^recebimento pela') | 
             tolower(texto_tramitacao) %in% descricoes_comissoes) & sigla_local != 'CCP' & !stringr::str_detect(tolower(sigla_local), '^s') ~ "Comissões")
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
    extract_events_in_camara() %>%
    extract_locais_in_camara() %>%
    #extract_fase_global_in_camara(proposicao_df) %>% 
    #extract_situacao_comissao() %>%
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

#' @title Extrai o regime de apreciação na Câmara
#' @description Obtém o regime de apreciação de um PL
#' @param proposicao_id id da proposicao
#' @return String com a situação do PL.
#' @examples
#' extract_forma_apreciacao_camara(217161)
#' @export
#' @importFrom stats filter
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

#' @title Extrai o regime de tramitação de um PL na Câmara
#' @description Obtém o regime de tramitação de um PL.
#' @param df Dataframe da tramitação na Câmara.
#' @return String com a situação do regime de tramitação da PL.
#' @examples
#' extract_regime_tramitacao_camara(fetch_tramitacao(257161,'camara', TRUE))
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
  
  prop_id <- tram_df[1,]$prop_id
  
  regime_df <- rcongresso::fetch_proposicao(prop_id) %>%
    fuzzyjoin::regex_left_join(regex_regime, 
                               by = c(statusProposicao.regime = "regex"))
    return(regime_df[1,]$regime_tramitacao)
}