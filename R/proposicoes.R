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

#' @title Baixa todos os novos documentos relacionados a uma proposição
#' @description Retorna um dataframe contendo os documentos relacionados a uma proposição
#' @param all_pls_ids IDs das proposições principais
#' @param current_relacionadas_ids IDs dos documentos relacionados
#' @return Dataframe
#' @examples
#' fetch_relacionadas(2056568)
#' @export
fetch_new_relacionadas <- function(all_pls_ids, current_relacionadas_ids) {

  pls_principais_ids <- all_pls_ids %>%
    dplyr::filter(casa == "camara") %>%
    select(id_principal)

  all_relacionadas_ids <- purrr::map_df(pls_principais_ids$id_principal, ~rcongresso::fetch_ids_relacionadas(.x)) %>%
    dplyr::rename(id_principal = id_prop)

  new_relacionadas_ids <- all_relacionadas_ids %>%
    dplyr::anti_join(current_relacionadas_ids, by=c("id_relacionada","id_principal","casa"))

  if (nrow(new_relacionadas_ids > 0)) {
    relacionadas_camara <- purrr::map_df(new_relacionadas_ids$id_relacionada, ~ fetch_all_documents(.x))
  }

  new_relacionadas <- merge(relacionadas_camara, all_relacionadas_ids, by.x="id", by.y = "id_relacionada") %>%
    dplyr::distinct() %>%
    dplyr::select(id_relacionada = id,
                  id_principal,
                  casa,
                  sigla_tipo = siglaTipo,
                  data_apresentacao = dataApresentacao,
                  ementa,
                  dplyr::everything())
  new_relacionadas

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
get_all_ids <- function(pls_ids_df) {
  pls_ids_camara <- pls_ids_df %>%
    dplyr::mutate(casa = "camara") %>%
    dplyr::select(id_principal = id_camara,casa,apelido,tema) %>%
    dplyr::filter(!is.na(id_principal))

  pls_ids_senado <- pls_ids_df %>%
    dplyr::mutate(casa = "senado") %>%
    dplyr::select(id_principal = id_senado,casa,apelido,tema) %>%
    dplyr::filter(!is.na(id_principal))

  pls_ids_all <- dplyr::bind_rows(pls_ids_camara,pls_ids_senado)
  return(pls_ids_all)
}
#
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

fetch_all_documents <- function(id_documento) {
  fetch_prop_output <- safe_fetch_proposicao(id_documento)
  if (!is.null(fetch_prop_output$error)) {
    print(fetch_prop_output$error)
  }
  return(fetch_prop_output$result)
}

