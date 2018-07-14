source(here::here("R/congresso-lib.R"))

#' @title Recupera o número, o tipo e ementa de uma proposição na Câmara
#' @description Retorna um dataframe contendo o número, o tipo e a ementa de uma proposição na Câmara através do ID da proposição
#' @param prop_id ID da proposição
#' @return Dataframe com o número, o tipo e a ementa da proposição na Câmara.
#' @examples
#' get_ementas_in_camara(2121442)
#' @export
get_ementas_in_camara <- function(prop_id) {
  rcongresso::fetch_proposicao(prop_id) %>% dplyr::select(ementa, siglaTipo, numero)
}

#' @title Recupera os n últimos despachos na Câmara
#' @description Retorna um dataframe das últimas n tramitações na Câmara contendo a hora, a descrição e o despacho
#' @param df Dataframe da tramitação na Câmara
#' @param qtd  (opcional) Quantidade de eventos a serem recuperados. (Default: qtd = 1)
#' @return Dataframe com as última n tramitações da Câmara.
#' @examples
#' tramitacao %>% last_n_despacho_in_camara()
#' tramitacao %>% last_n_despacho_in_camara(4)
#' @export
last_n_despacho_in_camara <- function(df, qtd=1) {
  df %>%
    dplyr::arrange(data_hora) %>%
    tail(qtd) %>%
    dplyr::select(data_hora, descricao_tramitacao, despacho)
}


get_comissoes_camara <- function() {

  siglas_comissoes_antigas <- '
  CDCMAM
  CAPR
  CCJR
  '

  siglas_comissoes <- '
  CAPADR
  CCTCI
  CCJC
  CCULT
  CDC
  CMULHER
  CIDOSO
  CPD
  CDU
  CDEICS
  CDHM
  CE
  CESPO
  CFT
  CFFC
  CINDRA
  CLP
  CMADS
  CME
  CREDN
  CSPCCO
  CSSF
  CTASP
  CTUR
  CVT
  '

  comissoes_permanentes <- '
    Agricultura, Pecuária, Abastecimento e Desenvolvimento Rural
    Ciência e Tecnologia, Comunicação e Informática
    Constituição e Justiça e de Cidadania
    Cultura
    Defesa do Consumidor
    Defesa dos Direitos da Mulher
    Defesa dos Direitos da Pessoa Idosa
    Defesa dos Direitos das Pessoas com Deficiência
    Desenvolvimento Urbano
    Desenvolvimento Econômico, Indústria, Comércio e Serviços
    Direitos Humanos e Minorias
    Educação
    Esporte
    Finanças e Tributação
    Fiscalização Financeira e Controle
    Integração Nacional, Desenvolvimento Regional e da Amazônia
    Legislação Participativa
    Meio Ambiente e Desenvolvimento Sustentável
    Minas e Energia
    Relações Exteriores e de Defesa Nacional
    Segurança Pública e Combate ao Crime Organizado
    Seguridade Social e Família
    Trabalho, de Administração e Serviço Público
    Turismo
    Viação e Transportes
    '

  comissoes_temporarias <- '
    Comissão Especial
    '

  to_list <- function(name) {
    name %>%
      trimws() %>%
      strsplit('\n') %>%
      sapply(trimws)
  }

  dplyr::tibble(siglas_comissoes_antigas=list(siglas_comissoes_antigas %>% to_list),
                siglas_comissoes=list(siglas_comissoes %>% to_list),
                comissoes_temporarias=list(comissoes_temporarias %>% to_list),
                comissoes_permanentes=list(comissoes_permanentes %>% to_list),)
}

#' @title Recupera as comissões pelas quais a proposição irá passar
#' @description Retorna um dataframe das comissões pelas quais a proposição irá passar, contendo a hora, o id da proposição e
#' as próximas comissões
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com as próximas comissões que a proposição irá passar.
#' @examples
#' tramitacao %>% get_comissoes_in_camara(df)
#' @export
get_comissoes_in_camara <- function(df) {
  reg <-
    unlist(get_comissoes_camara()) %>%
    paste(collapse='|') %>%
    stringr::regex(ignore_case=TRUE)

  fix_names <- function(name) {
    if(!str_detect(name, 'Comissão') & !grepl("^[[:upper:]]+$", name))
      paste("Comissão de", name)
    else name
  }

  detect <- function(str, regex) {
    stringr::str_detect(str, stringr::regex(regex, ignore_case=TRUE))
  }

  df %>%
    dplyr::mutate(
      comissoes = dplyr::case_when(
        (detect(descricao_tramitacao, 'distribuição') &
          (detect(descricao_tramitacao, 'cria..o de comiss.o tempor.ria') |
           detect(despacho, 'especial'))
        ) ~ 'Comissão Especial',
        (detect(descricao_tramitacao, 'distribuição') &
         (detect(despacho, 'às* comiss..s*|despacho à') |
          detect(despacho, 'novo despacho'))
        ) ~ despacho
    )) %>%
    dplyr::filter(!is.na(comissoes)) %>%
    dplyr::mutate(
      proximas_comissoes = stringr::str_extract_all(comissoes, reg) %>% as.list()
    ) %>%
    dplyr::select(data_hora, id_prop, proximas_comissoes) %>%
    dplyr::mutate(proximas_comissoes = map(proximas_comissoes, fix_names) ) %>%
    dplyr::mutate(proximas_comissoes = unique(proximas_comissoes, incomparables = FALSE))
}

#' @title Cria coluna com os relatores na tramitação na Câmara
#' @description Cria uma nova coluna com os relatores na Câmara. O relator é adicionado à coluna no
#' envento pontual em que ele é designado
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "relator" adicionada.
#' @examples
#' tramitacao %>% extract_relator_in_camara()
#' @export
extract_relator_in_camara <- function(df) {
  df %>%
    dplyr::mutate(relator=dplyr::case_when(
      stringr::str_detect(tolower(despacho), '^designad. relat.r') ~
        stringr::str_extract(despacho, stringr::regex('dep.+', ignore_case=TRUE))))
}

#' @title Recupera o último relator na Câmara
#' @description Recupera o nome do último relator na Câmara
#' @param df Dataframe da tramitação na Câmara
#' @return String do nome do último relator na Câmara
#' @examples
#' tramitacao %>% extract_last_relator_in_camara()
#' @export
extract_last_relator_in_camara <- function(df) {
  relatores <- extract_relator_in_camara(df)
  relator <-
    relatores %>%
    dplyr::filter(!is.na(relator)) %>%
    dplyr::arrange(desc(data_hora)) %>%
    dplyr::select(relator)

  relator$relator[1]
}

#' @title Cria coluna com as fases da tramitação na Câmara
#' @description Cria uma nova coluna com as fases na Câmara.
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "fase" adicionada.
#' @examples
#' tramitacao %>% extract_phases_in_camara()
#' @export
extract_phases_in_camara <- function(dataframe) {
  iniciativa <- c(100)
  relatoria <- c(320)
  discussao_deliberacao <- c(322)
  virada_de_casa <- c(128)
  final <- c(502, 251)

  dataframe %<>%
    dplyr::mutate(fase = dplyr::case_when(detect_fase(id_tipo_tramitacao, iniciativa) ~ 'iniciativa',
                                          detect_fase(id_tipo_tramitacao, relatoria) ~ 'relatoria',
                                          detect_fase(id_tipo_tramitacao, discussao_deliberacao) ~ 'discussao_deliberacao',
                                          detect_fase(id_tipo_tramitacao, virada_de_casa) ~ 'virada_de_casa',
                                          detect_fase(id_tipo_tramitacao, final) ~ 'final',
                                          detect_fase(id_situacao, 937) ~ 'final'))
}

#' @title Busca os últimos n eventos da tramitação na Câmara
#' @description Recupera os útimos n eventos da tramitação na Câmara, caso nenhuma quantidade seja informada, assume-se que é 1
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe dos últimos n eventos na Câmara contendo hora e evento.
#' @examples
#' tramitacao %>% extract_last_n_events_in_camara()
#' tramitacao %>% extract_last_n_events_in_camara(3)
#' @export
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
#' @examples
#' tramitacao %>% extract_relatorias_in_camara()
#' @export
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
    add_column() %>%
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
#' @examples
#' df %>% rename_df_columns()
#' @export
rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

#' @title Extrai os eventos importantes que aconteceram na Câmara
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram na Câmara
#' @param tramitacao_df Dataframe da tramitação na Câmara
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
#' @examples
#' df %>% extract_events_in_camara(importants_events)
#' @export
extract_events_in_camara <- function(tramitacao_df) {
  events_df <- frame_data(
    ~ evento, ~ regex,
    'requerimento_audiencia_publica', '^apresentação do requerimento.*requer a realização d.* audiências? públicas?',
    'aprovacao_audiencia_publica', '^aprovado requerimento.*requer a realização d.* audiências? públicas?',
    'aprovacao_parecer', 'aprovado.*parecer',
    'requerimento_redistribuicao', '^apresentação do requerimento de redistribuição',
    'requerimento_apensacao', '^apresentação do requerimento de apensação',
    'requerimento_urgencia', '^apresentação do requerimento de urgência',
    'requerimento_prorrogacao', '^apresentação do requerimento de prorrogação de prazo de comissão temporária')

  special_comissao <- c('120')

  tramitacao_df %>%
    dplyr::mutate(despacho_lower = tolower(despacho)) %>%
    fuzzyjoin::regex_left_join(events_df, by = c(despacho_lower = "regex")) %>%
    dplyr::select(-c(despacho_lower, regex)) %>%
    dplyr::mutate(evento = dplyr::case_when(
      id_tipo_tramitacao == special_comissao ~ 'criacao_comissao_temporaria',
      TRUE ~ evento))
}

#' @title Altera as datas da tramitação para formato mais fácil de tratar
#' @description Formata cada data da coluna para o formato POSIXct
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna de datas refatorada para um formato tratável.
#' @examples
#' df %>% refact_date()
#' @export
refact_date <- function(df) {
  dplyr::mutate(df, data_hora = lubridate::ymd_hm(data_hora))
}

#' @title Ordena o dataframe de acordo com a data
#' @description Ordena o dataframe de acordo com a data
#' @param df Dataframe contendo coluna de datas
#' @return Dataframe ordenado pela data
#' @examples
#' df %>% sort_by_date()
#' @export
sort_by_date <- function(df) {
  dplyr::arrange(df, data_hora, sequencia)
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
    dplyr::mutate(casa_origem = dplyr::case_when(
      stringr::str_detect(tolower(autor.nome), camara_exp) | autor.tipo == 'Deputado' ~ 'Câmara dos Deputados',
      stringr::str_detect(tolower(autor.nome), senado_exp) | autor.tipo == 'Senador' ~ 'Senado Federal'))

  partido_estado <- extract_partido_estado_autor(authors$autor.uri %>% tail(1))

  authors %>%
    dplyr::mutate(autor.nome = paste0(autor.nome, " ", partido_estado))
}

#' @title Recupera o estado e partido de um autor
#' @description Retorna o estado e partido
#' @param uri uri que contém dados sobre o autor
#' @return Estado e partido
#' @examples
#' partido_estado <- extract_partido_estado_autor(autores$autor.uri %>% tail(1))
#' @export
extract_partido_estado_autor <- function(uri) {
  if(!is.na(uri)) {
    json_autor <- jsonlite::fromJSON(uri, flatten = T)

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
  }else {
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
  api_v1_proposicao_url = 'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp='
  xml2::read_xml(paste0(api_v1_proposicao_url, prop_id)) %>%
    xml2::xml_find_all('//apensadas/proposicao/codProposicao') %>%
    xml2::xml_text() %>%
    tibble::tibble(apensadas=.)
}

fetch_proposicao_with_apensamentos <- function(prop_id) {
  rcongresso::fetch_proposicao(prop_id) %>%
    dplyr::mutate(proposicoes_apensadas = paste(fetch_apensadas(prop_id), collapse=' '))
}

#' @title Baixa dados sobre uma proposição
#' @description Retorna um dataframe contendo dados sobre uma proposição
#' @param prop_id Um ou mais IDs de proposições
#' @return Dataframe
#' @examples
#' fetch_proposicao_in_camara(2056568)
#' @export
fetch_proposicao_in_camara <- function(prop_id) {
  base_url <- 'http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao='

  regime_regex <-
    frame_data(~ regime_tramitacao, ~ regex,
               'ordinaria', 'Ordinária',
               'prioridade', 'Prioridade',
               'urgencia', 'Urgência')

  apreciacao_regex <-
    frame_data(~ forma_apreciacao, ~ regex,
               'conclusiva', 'Sujeita à Apreciação Conclusiva pelas Comissões',
               'plenario', 'Sujeita à Apreciação do Plenário')

  rcongresso::fetch_proposicao(prop_id) %>%
    # Adiciona url das páginas das proposições
    dplyr::mutate(page_url=paste0(base_url, prop_id)) %>%

    # Adiciona html das páginas das proposições
    dplyr::rowwise() %>%
    dplyr::mutate(page_html=list(xml2::read_html(page_url))) %>%

    # Padroniza valor sobre regime de tramitação
    fuzzyjoin::regex_left_join(regime_regex, by=c(statusProposicao.regime="regex")) %>%
    select(-'regex') %>%

    # Adiciona coluna sobre forma de apreciação
    dplyr::rowwise() %>%
    dplyr::mutate(temp=
      rvest::html_node(page_html, '#informacoesDeTramitacao') %>%
      rvest::html_text()
    ) %>%
    fuzzyjoin::regex_left_join(apreciacao_regex, by=c(temp="regex")) %>%
    select(-c('temp', 'regex'))
}

#' @title Baixa dados de requerimentos relacionados
#' @description Retorna um dataframe contendo dados sobre os requerimentos relacionados a uma proposição
#' @param prop_id ID de uma proposição
#' @return Dataframe
#' @examples
#' fetch_releated_requerimentos(2056568)
#' @export
fetch_related_requerimentos <- function(id, mark_deferimento=T) {
  regexes <-
    frame_data(~ deferimento, ~ regex,
               'indeferido', '^Indefiro',
               'deferido', '^(Defiro)|(Aprovado)')

  related <-
    rcongresso::fetch_relacionadas(id)$uri %>%
    strsplit('/') %>%
    vapply(last, '') %>%
    unique %>%
    rcongresso::fetch_proposicao()

  requerimentos <-
    related %>%
    dplyr::filter(stringr::str_detect(.$siglaTipo, '^REQ'))

  if(!mark_deferimento) return(requerimentos)

  tramitacoes <-
    requerimentos$id %>%
    rcongresso::fetch_tramitacao()

  related <-
    tramitacoes %>%
    # mark tramitacoes rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by=c(despacho='regex')) %>%
    dplyr::group_by(id_prop) %>%
    # fill down marks
    tidyr::fill(deferimento) %>%
    # get last mark on each tramitacao
    dplyr::do(tail(., n=1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_prop, deferimento) %>%
    # and mark proposicoes based on last tramitacao mark
    dplyr::left_join(related, by=c('id_prop' = 'id'))
}

#' @title Recupera os eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' fetch_events(2121442)
#' @export
fetch_events <- function(prop_id) {
  events_base_url <- 'http://www.camara.gov.br/proposicoesWeb/sessoes_e_reunioes?idProposicao='
  bill_events_url <- paste0(events_base_url,prop_id)
  events <- bill_events_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath='//*[@id="content"]/table') %>%
    rvest::html_table()
  events_df <- events[[1]]
  names(events_df) <- c('timestamp','origem','descricao','links')
  events_df %>%
    dplyr::select(-links) %>%
    dplyr::mutate(timestamp = lubridate::dmy_hm(timestamp))
}

#' @title Recupera os últimos eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' get_latest_events(2121442)
#' @export
get_latest_events <- function(prop_id) {
  fetch_events(prop_id) %>%
    dplyr::filter(timestamp <= lubridate::now())
}

#' @title Recupera os próximos eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' get_next_events(2121442)
#' @export
get_next_events <- function(prop_id) {
  fetch_events(prop_id) %>%
    dplyr::filter(timestamp > lubridate::now())
}

#' @title Recupera os locais da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada local
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada local
#' @examples
#'  extract_locais_in_camara(fetch_tramitacao(91341))
#' @export
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
            tolower(despacho) %in% descricoes_comissoes) & sigla_orgao != 'CCP' ~ sigla_orgao,
          tolower(descricao_tramitacao) == 'remessa ao senado federal' ~ 'Câmara')
    )

  if (is.na(df[1, ]$local)) {
    df[1, ]$local = 'CD-MESA-PLEN'
  }

  df %>%
    tidyr::fill(local)
}

extract_tramitacao <- function(prop_id){
  rcongresso::fetch_tramitacao(prop_id) %>% rename_df_columns
}
