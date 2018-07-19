#' @title Renomeia um vetor com o padrão de underscores e minúsculas
#' @description Renomeia cada item do vetor com o padrão: separado por underscore e letras minúsculas
#' @param x Vetor de strings
#' @return Vetor contendo as strings renomeadas.
#' @examples
#' to_underscore(c("testName", "TESTNAME"))
#' @export
to_underscore <- function(x) {
  gsub('([A-Za-z])([A-Z])([a-z])', '\\1_\\2\\3', x) %>%
    gsub('.', '_', ., fixed = TRUE) %>%
    gsub('([a-z])([A-Z])', '\\1_\\2', .) %>%
    tolower()
}

#' @title Verfica se um elemento está contido em um vetor
#' @description Verfica se um elemento está contido em um vetor
#' @param element Elemento que pode estar contido
#' @param set Vetor de elementos
#' @return Valor booleano que indica se o elemento está contido no vetor.
#' @export
detect_fase <- function(element, set) {
  element %in% set
}

#' @title Recupera os detalhes de uma proposição no Senado ou na Câmara
#' @description Retorna dataframe com os dados detalhados da proposição, incluindo número, ementa, tipo e data de apresentação.
#' @param id ID de uma proposição
#' @param casa casa de onde a proposição esta
#' @return Dataframe com as informações detalhadas de uma proposição
#' @examples
#' fetch_proposicao(91341, 'senado')
#' @export
fetch_proposicao <- function(id, casa) {
  if (tolower(casa) == 'camara') {
    fetch_proposicao_camara(id)
  } else {
    fetch_proposicao_senado(id)
  }
}

#' @title Recupera os detalhes de uma proposição no Senado
#' @description Retorna dataframe com os dados detalhados da proposição, incluindo número, ementa, tipo e data de apresentação.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações detalhadas de uma proposição no Senado
#' @examples
#' fetch_proposicao_senado(91341)
#' @export
fetch_proposicao_senado <- function(proposicao_id){
  url_base_proposicao <- "http://legis.senado.leg.br/dadosabertos/materia/"
  da_url <- paste0(url_base_proposicao, proposicao_id)

  page_url_senado <- "https://www25.senado.leg.br/web/atividade/materias/-/materia/"

  json_proposicao <- jsonlite::fromJSON(da_url, flatten = T)
  proposicao_data <- json_proposicao$DetalheMateria$Materia
  proposicao_ids <-
    proposicao_data$IdentificacaoMateria %>%
    tibble::as.tibble()
  proposicao_basic_data <-
    proposicao_data$DadosBasicosMateria %>%
    purrr::flatten() %>%
    tibble::as.tibble()
  proposicao_author <-
    proposicao_data$Autoria$Autor %>%
    tibble::as.tibble()
  proposicao_specific_assunto <-
    proposicao_data$Assunto$AssuntoEspecifico %>%
    tibble::as.tibble() %>%
    dplyr::rename(assunto_especifico = Descricao, codigo_assunto_especifico = Codigo)
  proposicao_general_assunto <-
    proposicao_data$Assunto$AssuntoGeral %>%
    tibble::as.tibble() %>%
    dplyr::rename(assunto_geral = Descricao, codigo_assunto_geral = Codigo)
  proposicao_source <-
    proposicao_data$OrigemMateria %>%
    tibble::as.tibble()
  anexadas <-
    proposicao_data$MateriasAnexadas$MateriaAnexada$IdentificacaoMateria.CodigoMateria
  relacionadas <-
    proposicao_data$MateriasRelacionadas$MateriaRelacionada$IdentificacaoMateria.CodigoMateria

  proposicao_complete <-
    proposicao_basic_data %>%
    tibble::add_column(
      !!! proposicao_ids, !!! proposicao_author, !!! proposicao_specific_assunto,
      !!! proposicao_general_assunto, !!! proposicao_source,
      page_url = paste0(page_url_senado, proposicao_id),
      proposicoes_relacionadas = paste(relacionadas, collapse=' '),
      proposicoes_apensadas = paste(anexadas, collapse=' '))

  proposicao_complete <- proposicao_complete[, !sapply(proposicao_complete, is.list)]

  rename_proposicao_df(proposicao_complete)
}


#' @title Baixa dados sobre uma proposição
#' @description Retorna um dataframe contendo dados sobre uma proposição
#' @param prop_id Um ou mais IDs de proposições
#' @return Dataframe
#' @examples
#' fetch_proposicao_camara(2056568)
#' @export
fetch_proposicao_camara <- function(prop_id) {
  base_url <- 'http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao='

  regex_regime <-
    tibble::frame_data(~ regime_tramitacao, ~ regex,
               'Ordinária', 'Ordinária',
               'Prioridade', 'Prioridade',
               'Urgência', 'Urgência')

  regex_apreciacao <-
    tibble::frame_data(~ forma_apreciacao, ~ regex,
               'Conclusiva', 'Sujeita à Apreciação Conclusiva pelas Comissões',
               'Plenário', 'Sujeita à Apreciação do Plenário')

  rcongresso::fetch_proposicao(prop_id) %>%
    # Adiciona url das páginas das proposições
    dplyr::mutate(page_url=paste0(base_url, prop_id)) %>%
    # Adiciona html das páginas das proposições
    dplyr::rowwise() %>%
    dplyr::mutate(page_html=list(xml2::read_html(page_url))) %>%

    # Padroniza valor sobre regime de tramitação
    fuzzyjoin::regex_left_join(regex_regime, by=c(statusProposicao.regime="regex")) %>%
    dplyr::select(-'regex') %>%

    # Adiciona coluna sobre forma de apreciação
    dplyr::rowwise() %>%
    dplyr::mutate(temp=
             rvest::html_node(page_html, '#informacoesDeTramitacao') %>%
             rvest::html_text()
    ) %>%
    fuzzyjoin::regex_left_join(regex_apreciacao, by=c(temp="regex")) %>%
    dplyr::select(-c('temp', 'regex'))
}
