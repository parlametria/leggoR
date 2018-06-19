source(here::here("code/congresso-lib.R"))

#' @title Recupera o número, o tipo e ementa de uma proposição na Câmara
#' @description Retorna um dataframe contendo o número, o tipo e a ementa de uma proposição na Câmara através do ID da proposição
#' @param bill_id ID da proposição
#' @return Dataframe com o número, o tipo e a ementa da proposição na Câmara.
#' @examples
#' get_nome_ementa_Camara(2121442)
#' @export
get_nome_ementa_Camara <- function(bill_id) {
  rcongresso::fetch_proposicao(bill_id) %>% dplyr::select(ementa, siglaTipo, numero)
}

#' @title Recupera os n últimos despachos na Câmara
#' @description Retorna um dataframe das últimas n tramitações na Câmara contendo a hora, a descrição e o despacho 
#' @param df Dataframe da tramitação na Câmara
#' @param qtd  (opcional) Quantidade de eventos a serem recuperados
#' @return Dataframe com as última n tramitações da Câmara.
#' @examples
#' tramitacao %>% tail_descricao_despacho_Camara()
#' tramitacao %>% tail_descricao_despacho_Camara(4)
#' @export
tail_descricao_despacho_Camara <- function(df, qtd=1) {
  df %>% 
    dplyr::arrange(data_hora) %>% 
    tail(qtd) %>% 
    dplyr::select(data_hora, descricao_tramitacao, despacho)
}

#' @title Recupera as comissões pelas quais a proposição irá passar
#' @description Retorna um dataframe aas comissões pelas quais a proposição irá passar, contendo a hora, o id da proposição e
#' as próximas comissões 
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com as próximas comissões que a proposição irá passar.
#' @examples
#' tramitacao %>% get_ditribuicao_comissoes_Camara()
#' @export
get_ditribuicao_comissoes_Camara <- function(df) {
  comissoes_permanentes <- "agricultura, pecu.ria, abastecimento e desenvolvimento rural|ci.ncia e tecnologia, comunica..o e informatica|constitui..o e justi.a e de cidadania|cultura|defesa do consumidor|defesa dos direitos da mulher|defesa dos direitos da pessoa idosa|defesa dos direitos das pessoas com defici.ncia|desenvolvimento urbano|desenvolvimento econ.mico, ind.stria, com.rcio e servi.os|direitos humanos e minorias|educa..o|comiss.o do esporte|finan.as e tributa..o|fiscaliza..o financeira e controle|integra..o nacional, desenvolvimento regional e da amaz.nia|legisla..o participativa|meio ambiente e desenvolvimento sustent.vel|minas e energia|rela..es exteriores e de defesa nacional|seguran.a p.blica e combate ao crime organizado|seguridade social e fam.lia|trabalho, de administra..o e servi.o p.blico|turismo|via..o e transportes"
  
  df %>%
    dplyr::mutate(proximas_comissoes = dplyr::case_when(stringr::str_detect(despacho, regex("às comiss..s|despacho à", ignore_case=TRUE)) ~ stringr::str_extract_all(despacho, regex(comissoes_permanentes, ignore_case=TRUE)),
                                                        stringr::str_detect(descricao_tramitacao, regex("cria..o de comiss.o tempor.ria", ignore_case=TRUE)) ~ list("Especial"),
                                                        TRUE ~ list(NA))) %>%
    dplyr::filter(!is.na(proximas_comissoes)) %>% 
    dplyr::select(c('data_hora', 'id_prop', 'proximas_comissoes'))
}

#' @title Cria coluna com os relatores na tramitação na Câmara
#' @description Cria uma nova coluna com os relatores na Câmara. O relator é adicionado à coluna no 
#' envento pontual em que ele é designado
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "relator" adicionada.
#' @examples
#' tramitacao %>% extract_relator_Camara()
#' @export
extract_relator_Camara <- function(df) {
  df %>% 
    dplyr::mutate(relator = 
                    case_when(stringr::str_detect(tolower(despacho), '^designad. relat.r') ~ 
                                stringr::str_extract(despacho, regex('dep.+', ignore_case=TRUE))))
}

#' @title Recupera o último relator na Câmara
#' @description Recupera o nome do último relator na Câmara
#' @param df Dataframe da tramitação na Câmara
#' @return String do nome do último relator na Câmara
#' @examples
#' tramitacao %>% extract_last_relator_Camara()
#' @export
extract_last_relator_Camara <- function(df) {
  relatores <- extract_relator_Camara(df)
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
#' tramitacao %>% extract_phases_Camara()
#' @export
extract_phases_Camara <- function(dataframe, phase_one, phase_two, phase_three, phase_four, phase_five) {
  dataframe %<>%
    dplyr::mutate(fase = dplyr::case_when(detect_phase(id_tipo_tramitacao, phase_one) ~ 'iniciativa',
                                          detect_phase(id_tipo_tramitacao, phase_two) ~ 'relatoria',
                                          detect_phase(id_tipo_tramitacao, phase_three) ~ 'discussao_deliberacao',
                                          detect_phase(id_tipo_tramitacao, phase_four) ~ 'virada_de_casa',
                                          detect_phase(id_tipo_tramitacao, phase_five) ~ 'final',
                                          detect_phase(id_situacao, 937) ~ 'final'))
}

#' @title Busca os últimos n eventos da tramitação na Câmara
#' @description Recupera os útimos n eventos da tramitação na Cãmara, caso nenhuma quantidade seja informada, assume-se que é 1
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe dos últimos n eventos na Câmara contendo hora e evento.
#' @examples
#' tramitacao %>% extract_n_last_events_Camara()
#' tramitacao %>% extract_n_last_events_Camara(3)
#' @export
extract_n_last_events_Camara <- function(df, num) {
  df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::arrange(data_hora) %>%
    tail(n = num) %>%
    dplyr::select(data_hora, evento)
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
#' @param tramitacao_df Dataframe da tramitação na Cãmara
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
#' @examples
#' df %>% extract_events_Camara(importants_events)
#' @export
extract_events_Camara <- function(tramitacao_df, events_df) {
  tramitacao_df %<>%
    dplyr::mutate(despacho_lower = tolower(despacho)) %>%
    fuzzyjoin::regex_left_join(importants_events, by = c(despacho_lower = "regex")) %>%
    dplyr::select(-c(despacho_lower, regex))
  tramitacao_df %<>%
    dplyr::mutate(evento = dplyr::case_when(id_tipo_tramitacao == special_commission ~ 'criacao_comissao_temporaria', 
                                            TRUE ~ evento))
}

#' @title Altera as datas da tramitação para formato mais fácil de tratar 
#' @description Formata cada data da coluna para o formato POSIXct
#' @param df Dataframe da tramitação na Cãmara
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
#' extract_autor_Camara(2121442)
#' @export
extract_autor_Camara <- function(prop_id) {
  camara_exp <- 'câmara dos deputados'
  senado_exp <- 'senado federal'
  
  url_base_autores <- 'https://dadosabertos.camara.leg.br/api/v2/proposicoes/'
  url <- paste0(url_base_autores, prop_id, '/autores')
  json_voting <- jsonlite::fromJSON(url, flatten = T)
  
  autores <- json_voting %>% 
    magrittr::extract2("dados") %>%
    dplyr::rename(autor.uri = uri,
                  autor.nome = nome,
                  autor.tipo = tipo,
                  autor.cod_tipo = codTipo) %>% 
    dplyr::mutate(casa_origem = dplyr::case_when(
      stringr::str_detect(tolower(autor.nome), camara_exp) | autor.tipo == 'Deputado' ~ 'Câmara dos Deputados',
      stringr::str_detect(tolower(autor.nome), senado_exp) | autor.tipo == 'Senador' ~ 'Senado Federal'))
  
  autores
}

#' @title Recupera as proposições apensadas
#' @description Retorna os IDs das proposições apensadas a uma determinada proposição
#' @param prop_id ID da proposição
#' @return Ventor contendo os IDs das proposições apensadas
#' @examples
#' fetch_apensadas(2121442)
#' @export
fetch_apensadas <- function(prop_id) {
  api_v1_proposicao = 'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp='
  xml2::read_xml(paste0(api_v1_proposicao, prop_id)) %>%
    xml2::xml_find_all('//apensadas/proposicao/codProposicao') %>%
    xml2::xml_text() %>%
    tibble::tibble(apensadas=.)
}

fetch_proposicao_com_apensamentos <- function(prop_id) {
  prop <- rcongresso::fetch_proposicao(prop_id)
  prop$proposicoes_apensadas <- fetch_apensadas(prop_id) %>% paste(collapse = ' ')
  prop
}

fetch_requerimentos_relacionados <- function(id, mark_deferimento=T) {
  regexes <- 
    frame_data(~ deferimento, ~ regex,
               "indeferido", '^Indefiro',
               "deferido", '^(Defiro)|(Aprovado)')
  
  relacionadas <- 
    rcongresso::fetch_relacionadas(id)$uri %>% 
    strsplit('/') %>% 
    vapply(last, '') %>% 
    unique %>% 
    rcongresso::fetch_proposicao()
  
  requerimentos <- 
    relacionadas %>% 
    dplyr::filter(stringr::str_detect(.$siglaTipo, '^REQ'))
  
  if(!mark_deferimento) return(requerimentos)
  
  tramitacoes <- 
    requerimentos$id %>% 
    rcongresso::fetch_tramitacao()
  
  relacionadas <-
    tramitacoes %>% 
    # mark tramitacoes rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by=c(despacho="regex")) %>% 
    dplyr::group_by(id_prop) %>% 
    # fill down marks
    tidyr::fill(deferimento) %>%
    # get last mark on each tramitacao
    dplyr::do(tail(., n=1)) %>%
    dplyr::ungroup %>% 
    dplyr::select(id_prop, deferimento) %>% 
    # and mark proposicoes based on last tramitacao mark
    dplyr::left_join(relacionadas, by=c('id_prop' = 'id'))
}
