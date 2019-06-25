source(here::here("R/utils.R"))

#' @title Importa as informações de uma proposição da internet.
#' @description Recebido um id e a casa, a função roda os scripts para
#' importar os dados daquela proposição.
#' @param prop_id Identificador da proposição que pode ser recuperado no site da casa legislativa.
#' @param casa Casa onde o projeto está tramitando
#' @param out_folderpath Caminho da pasta onde os dados devem ser salvos
#' @param apelido Apelido da proposição
#' @param tema Tema da proposição
#' @export
#' @examples
#' import_proposicao(129808, 'senado', 'Cadastro Positivo', 'Agenda Nacional', 'data/')
import_proposicao <- function(prop_id, casa, apelido, tema, out_folderpath=NULL) {
  casa <- tolower(casa)
  if (!(casa %in% c('camara','senado'))) {
    return('Parâmetro "casa" não identificado.')
  }

  prop_df <- fetch_proposicao(prop_id,casa,apelido, tema)
  tram_df <- fetch_tramitacao(prop_id,casa)
  emendas_df <- rcongresso::fetch_emendas(prop_id,casa, prop_df$tipo_materia, prop_df$numero, prop_df$ano)

  if (!is.null(out_folderpath)) {
    if (!is.null(prop_df)) readr::write_csv(prop_df, build_data_filepath(out_folderpath,'proposicao',casa,prop_id))
    if (!is.null(tram_df)) readr::write_csv(tram_df, build_data_filepath(out_folderpath,'tramitacao',casa,prop_id))
    if (!is.null(emendas_df)) readr::write_csv(emendas_df, build_data_filepath(out_folderpath,'emendas',casa,prop_id))
  }

  return(list(proposicao = prop_df, tramitacao = tram_df))
}
#' @title Recupera os detalhes de uma proposição no Senado ou na Câmara
#' @description Retorna dataframe com os dados detalhados da proposição, incluindo número, ementa, tipo e data de apresentação.
#' @param id ID de uma proposição
#' @param casa casa de onde a proposição esta
#' @param apelido Apelido da proposição
#' @param tema Tema da proposição
#' @return Dataframe com as informações detalhadas de uma proposição
#' @examples
#' fetch_proposicao(129808, 'senado', 'Cadastro Positivo', 'Agenda Nacional', F)
#' @export
fetch_proposicao <- function(id, casa, apelido="", tema="") {
  casa <- tolower(casa)
  if (casa == "camara") {
    fetch_proposicao_camara(id, apelido, tema)
  } else if (casa == "senado") {
    fetch_proposicao_senado(id, apelido, tema)
  } else {
    return("Parâmetro 'casa' não identificado.")
  }
}

#' @title Recupera os detalhes de proposições no Senado ou na Câmara
#' @description Retorna dataframe com os dados detalhados das proposições, incluindo número, ementa, tipo e data de apresentação.
#' @param pls_ids Dataframe com id e casa das proposições
#' @return Dataframe com as informações detalhadas das proposições
#' @examples
#' all_pls <- readr::read_csv('data/tabela_geral_ids_casa.csv')
#' fetch_proposicoes(all_pls)
#' @export
fetch_proposicoes <- function(pls_ids) {
  purrr::map2_df(pls_ids$id, pls_ids$casa, ~ fetch_proposicao(.x, .y))
}

#' @title Recupera os detalhes de uma proposição no Senado
#' @description Retorna dataframe com os dados detalhados da proposição, incluindo número, ementa, tipo e data de apresentação.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @param normalized whether or not the output dataframe should be normalized (have the same format and column names for every house)
#' @param apelido apelido da proposição
#' @param tema tema da proposição
#' @return Dataframe com as informações detalhadas de uma proposição no Senado
#' @examples
#' fetch_proposicao_senado(91341, 'Cadastro Positivo', 'Agenda Nacional')
fetch_proposicao_senado <- function(id, apelido, tema) {

  proposicao <-
    rcongresso::fetch_proposicao_senado(id) %>%
    dplyr::transmute(
      prop_id = as.integer(codigo_materia),
      sigla_tipo = sigla_subtipo_materia,
      numero = as.integer(numero_materia),
      ano = as.integer(ano_materia),
      ementa = ementa_materia,
      data_apresentacao = lubridate::ymd_hm(paste(data_apresentacao, "00:00")),
      casa = "senado",
      casa_origem = ifelse(tolower(nome_casa_origem) == "senado federal",
                           "senado",
                           "camara"),
      autor_nome,
      apelido_materia = ifelse(
        "apelido_materia" %in% names(.),
        apelido_materia,
        apelido),
      tema = tema
    )
}

#' @title Baixa dados sobre uma proposição
#' @description Retorna um dataframe contendo dados sobre uma proposição
#' @param prop_id Um ou mais IDs de proposições
#' @param normalized whether or not the output dataframe should be normalized (have the same format and column names for every house)
#' @param apelido Apelido da proposição
#' @param tema Tema da proposição
#' @return Dataframe
#' @examples
#' fetch_proposicao_camara(2056568, "Lei para acabar zona de amortecimento", "Meio Ambiente")
fetch_proposicao_camara <- function(id, apelido, tema) {
  autor_df <- rcongresso::fetch_autor_camara(id)
  if("ultimoStatus.nomeEleitoral" %in% names(autor_df)) {
    autor_df %<>%
      dplyr::rename('nome' = 'ultimoStatus.nomeEleitoral')
  }

  proposicao <- rcongresso::fetch_proposicao_camara(id) %>%
    rename_df_columns() %>%
    dplyr::transmute(prop_id = as.integer(id),
                     sigla_tipo,
                     numero = as.integer(numero),
                     ano = as.integer(ano),
                     ementa = paste(ementa,ementa_detalhada),
                     data_apresentacao = lubridate::ymd_hm(stringr::str_replace(data_apresentacao,'T',' ')),
                     casa = 'camara',
                     casa_origem = ifelse(autor_df %>% head(1) %>%
                                            dplyr::select(codTipo) == 40000,"senado","camara"),
                     autor_nome = paste(unlist(t(autor_df$nome)),collapse="+"),
                     autor_uf = ifelse(length(autor_df) > 1 && autor_df$codTipo == 10000,
                                       get_uf_autores(autor_df),
                                       NA),
                     autor_partido = ifelse(length(autor_df) > 1 && autor_df$codTipo == 10000,
                                            get_partido_autores(autor_df),
                                            NA),
                     apelido_materia = apelido,
                     tema = tema)
  proposicao
}

#' @title Baixa os ids dos documentos a partir dos ids das principais, verificando quais delas são novas
#' @description Retorna um dataframe contendo os novos documentos
#' @param all_pls_ids IDs das proposições principais
#' @param current_docs_ids IDs dos documentos atualmente baixados
#' @return Dataframe
#' @export
find_new_documentos <- function(all_pls_ids, current_docs_ids) {

  pls_principais_ids <- all_pls_ids %>%
    dplyr::filter(casa == "camara") %>%
    dplyr::select(id_principal,
                  casa) %>%
    dplyr::mutate(id_documento = id_principal)

  all_docs_ids <- purrr::map_df(pls_principais_ids$id_principal, ~rcongresso::fetch_ids_relacionadas(.x)) %>%
    dplyr::rename(id_principal = id_prop,
                  id_documento = id_relacionada)  %>%
    dplyr::bind_rows(pls_principais_ids)

  new_docs_ids <- all_docs_ids %>%
    dplyr::anti_join(current_docs_ids, by=c("id_documento","id_principal","casa"))

  return(new_docs_ids)
}

#' @title Baixa autores de documentos, adequando as colunas ao padrão desejado
#' @description Retorna um dataframe contendo autores dos documentos
#' @param docs_ids_df Dataframe com os ids dos documentos a serem baixadas
#' @return Dataframe
#' @export
fetch_autores_documentos <- function(docs_ids_df) {
  autores_docs_camara <- purrr::map2_df(docs_ids_df$id_documento, docs_ids_df$sigla_tipo, ~ fetch_all_autores(.x, .y)) %>%
    dplyr::mutate(casa = 'camara')

  autores_docs_camara
}

#' @title Baixa dados dos documentos, adequando as colunas ao padrão desejado
#' @description Retorna um dataframe contendo dados dos documentos
#' @param docs_ids Dataframe com os IDs dos documentos a serem baixados
#' @return Dataframe
#' @examples
#' \dontrun{
#'   docs_data <- fetch_docs_data(2056568)
#' }
#' @export
fetch_documentos_data <- function(docs_ids) {
  docs_camara <- purrr::map_df(docs_ids$id_documento, ~ fetch_all_documents(.x))
  formatted_docs_df <- tibble::tibble()

  if (nrow(docs_camara) > 0) {
    formatted_docs_df <- merge(docs_camara, docs_ids, by.x="id", by.y = "id_documento") %>%
      dplyr::distinct() %>%
      dplyr::select(id_documento = id,
                    id_principal,
                    casa,
                    sigla_tipo = siglaTipo,
                    numero,
                    ano,
                    data_apresentacao = dataApresentacao,
                    ementa,
                    dplyr::everything())
  }
  return(formatted_docs_df)
}

#' @title Concatena siglas de unidade federativa de cada autor da proposição
#' @description Retorna unidade federativa dos autores
#' @param autor_df Autores da proposição
#' @return character
get_uf_autores <- function(autor_df) {
  autores_uf <- (paste(unlist(t(autor_df$ultimoStatus.siglaUf)),collapse="+"))
  return(autores_uf)
}

#' @title Concatena siglas de partido de cada autor da proposição
#' @description Retorna partido dos autores
#' @param autor_df Autores da proposição
#' @return character
get_partido_autores <- function(autor_df) {
  autores_partido <- (paste(unlist(t(autor_df$ultimoStatus.siglaPartido)),collapse="+"))
  return(autores_partido)
}


#' @export
get_all_leggo_props_ids <- function(leggo_props_df) {
  pls_ids_camara <- leggo_props_df %>%
    dplyr::mutate(casa = "camara") %>%
    dplyr::select(id_principal = id_camara,casa,apelido,tema) %>%
    dplyr::filter(!is.na(id_principal))

  pls_ids_senado <- leggo_props_df %>%
    dplyr::mutate(casa = "senado") %>%
    dplyr::select(id_principal = id_senado,casa,apelido,tema) %>%
    dplyr::filter(!is.na(id_principal))

  pls_ids_all <- dplyr::bind_rows(pls_ids_camara,pls_ids_senado)
  return(pls_ids_all)
}

# update_proposicoes <- function(current_props_df, pls_ids_df) {
#   pls_ids_all <- .get_all_ids(pls_ids_df)
#
#   new_props <- pls_ids_all %>%
#     dplyr::anti_join(current_props_df, by=c("id_principal","casa")) %>%
#     dplyr::rename(id = id_principal)
#
#   if (nrow(new_props > 0)) {
#     new_props <- purrr::map2_df(new_props$id, new_props$casa, ~ agoradigital::fetch_proposicao(.x, .y))
#   }
#
#
#   return(new_props)
#
# }


safe_fetch_proposicao <- purrr::safely(rcongresso::fetch_proposicao_camara,otherwise = tibble::tibble())

#' @title Realiza busca das informações de um documento
#' @description Retorna dados de um documento caso a requisição seja bem-sucedida,
#' caso contrário retorna um Dataframe vazio
#' @param id_documento ID do documento
#' @return Dataframe
fetch_all_documents <- function(id_documento) {
  fetch_prop_output <- safe_fetch_proposicao(id_documento)
  if (!is.null(fetch_prop_output$error)) {
    print(fetch_prop_output$error)
  }
  return(fetch_prop_output$result)
}


safe_fetch_autores <- purrr::safely(rcongresso::fetch_autores_camara,otherwise = tibble::tibble())

#' @title Realiza busca dos autores de um documento
#' @description Retorna autores de um documento caso a requisição seja bem-sucedida,
#' caso contrário retorna um Dataframe vazio
#' @param id_documento ID do documento
#' @return Dataframe
fetch_all_autores <- function(id_documento, sigla_tipo) {
  fetch_prop_output <- safe_fetch_autores(id_documento, sigla_tipo)
  autores_result <- fetch_prop_output$result
  if (!is.null(fetch_prop_output$error)) {
    print(fetch_prop_output$error)
  } else {
    autores_result <- autores_result %>%
      dplyr::mutate(id_documento = id_documento)
  }
  return(autores_result)
}

