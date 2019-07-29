source(here::here("R/utils.R"))
camara_env <- jsonlite::fromJSON(here::here("R/config/environment_camara.json"))
senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))

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
#' @param casa_prop Casa de origem dos documentos
#' @return Dataframe
#' @export
find_new_documentos <- function(all_pls_ids, current_docs_ids, casa_prop) {

  pls_principais_ids <- all_pls_ids %>%
    dplyr::filter(casa == casa_prop) %>%
    dplyr::select(id_principal,
                  casa) %>%
    dplyr::mutate(id_documento = id_principal)

  all_docs_ids <- purrr::map2_df(pls_principais_ids$id_principal,
                                 pls_principais_ids$casa,
                                 ~rcongresso::fetch_ids_relacionadas(.x, .y)) %>%
    dplyr::rename(id_principal = id_prop,
                  id_documento = id_relacionada)  %>%
    dplyr::mutate(id_principal = as.double(id_principal),
                  id_documento = as.double(id_documento)) %>%
    dplyr::bind_rows(pls_principais_ids)

  new_docs_ids <- all_docs_ids %>%
    dplyr::anti_join(current_docs_ids, by=c("id_documento","id_principal","casa"))

  return(new_docs_ids)
}

#' @title Baixa autores de documentos, adequando as colunas ao padrão desejado
#' @description Retorna um dataframe contendo autores dos documentos
#' @param docs_data_df Dataframe com os dados dos documentos a serem baixadas
#' @return Dataframe
#' @export
fetch_autores_documentos <- function(docs_data_df) {
  casa_prop <- docs_data_df$casa[1]
  autores_docs <- purrr::pmap_df(list(docs_data_df$id_documento, docs_data_df$casa,
                                      docs_data_df$sigla_tipo), function(a,b,c) fetch_autores_documento(a,b,c)) %>%
  dplyr::mutate(casa = casa_prop) %>%
  rename_table_to_underscore()

  formatted_atores_df <- tibble::tibble()
  if (nrow(autores_docs) > 0) {
    if (casa_prop == 'camara') {
      formatted_atores_df <- autores_docs %>%
        dplyr::distinct() %>%
        dplyr::select(id_autor,
                      nome,
                      tipo_autor = tipo,
                      uri_autor = uri,
                      id_documento,
                      casa,
                      cod_tipo_autor = cod_tipo,
                      dplyr::everything())
    } else if (casa_prop == 'senado') {
      formatted_atores_df <- autores_docs %>%
        dplyr::distinct() %>%
        dplyr::select(id_autor = id_parlamentar,
                      nome,
                      tipo_autor = descricao_tipo_autor,
                      uri_autor = url_pagina,
                      id_documento,
                      casa,
                      partido = sigla_partido,
                      uf = uf_parlamentar,
                      dplyr::everything())
    } else {
      warning('Casa inválida')
    }
  }

  formatted_atores_df
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
  docs <- purrr::map2_df(docs_ids$id_documento, docs_ids$casa, ~ fetch_documento(.x, .y)) %>%
    rename_table_to_underscore()
  formatted_docs_df <- tibble::tibble()
  casa <- docs_ids$casa[1]
  if (nrow(docs) > 0) {
    if (casa == 'camara') {
      formatted_docs_df <- merge(docs_ids, docs, by.x="id_documento", by.y = "id") %>%
        dplyr::distinct() %>%
        dplyr::select(id_documento,
                      id_principal,
                      casa,
                      sigla_tipo,
                      numero,
                      ano,
                      data_apresentacao,
                      ementa,
                      descricao_tipo_documento = descricao_tipo,
                      cod_tipo_documento = cod_tipo,
                      uri_documento = uri,
                      dplyr::everything())
    } else if (casa == 'senado') {
      formatted_docs_df <- merge(docs_ids, docs, by.x="id_documento", by.y = "codigo_materia") %>%
        dplyr::distinct() %>%
        dplyr::select(id_documento,
                      id_principal,
                      casa,
                      sigla_tipo = sigla_subtipo_materia,
                      numero = numero_materia,
                      ano = ano_materia,
                      data_apresentacao,
                      ementa = ementa_materia,
                      dplyr::everything())

    } else {
      warning('Casa inválida')
    }

  }
  return(formatted_docs_df)
}

#' @title Baixa dados dos documentos, através de um scrap
#' @description Retorna um dataframe contendo dados dos documentos
#' @param pls_ids Dataframe com os IDs das proposições cujos documentos 
#' iremos baixar, formato (id_principal, casa)
#' @return Dataframe
#' @export
fetch_documento_relacionados_senado <- function(pls_ids) {
  docs <- 
    purrr::map2_df(pls_ids$id_principal, pls_ids$casa, ~ rcongresso::scrap_senado_congresso_documentos(.x, T, .y)) %>% 
  dplyr::mutate(id_documento = dplyr::row_number())
  return(docs)
}

#' @title Extrai dos autores de documentos o partido, estado e nome
#' @description Recebi uma lista de autores e retorna um dataframe com
#' nome, partido e estado dos autores
#' @param autor_raw lista com autores
#' @param id_doc id do documento
#' @return Dataframe
extract_autor_relacionadas_senado <- function(autor_raw, id_doc) {
  stringr::str_split(autor_raw,",") %>% 
  purrr::pluck(1) %>% 
  purrr::map_df(.aux_extract_autor_relacionadas_senado) %>% 
  dplyr::mutate(id_documento = id_doc) %>% 
  dplyr::distinct()
}

#' @title Recebi uma string com o autor e quebra ela em nome, partido e estado
#' @description Recebi uma string com o autor e quebra ela em nome, partido e estado
#' @param autores_raw_element Autor
#' @return Dataframe
.aux_extract_autor_relacionadas_senado <- function(autores_raw_element) {
  clean_autor_raw = trimws(autores_raw_element)
  nome_autor = ifelse(grepl('\\(',clean_autor_raw),stringr::str_extract(clean_autor_raw,"(.*?)(?=\\()"),clean_autor_raw)
  partido = stringr::str_extract(clean_autor_raw,"(?<=\\()(.*?)(?=\\/)")
  uf = stringr::str_extract(clean_autor_raw,"(?<=\\/)(.*?)(?=\\))")
  
  tibble::tibble(nome_autor = nome_autor, partido = partido, uf = uf)
}

#' @title Baixa dados dos autores dos documentos
#' @description Retorna um dataframe contendo dados dos autores dos documentos
#' @param relacionadas_docs Dataframe com os documetos oriundos do scrap
#' @return Dataframe
#' @export
fetch_autores_relacionadas_senado <- function(relacionadas_docs) {
  autores_raw <- 
    relacionadas_docs %>%
    dplyr::rename(autor_raw = autor) %>% 
    dplyr::filter(!is.na(autor_raw)) %>%
    dplyr::mutate(autor_raw =
        dplyr::if_else(stringr::str_detect(autor_raw,"Comissão de Constituição, Justiça e Cidadania"),
                       stringr::str_replace_all(autor_raw, "Comissão de Constituição, Justiça e Cidadania",
                                            "Comissão de Constituição Justiça e Cidadania"), autor_raw)) %>% 
    dplyr::mutate(autor_raw =
        dplyr::if_else(stringr::str_detect(autor_raw, "Comissão Mista da Medida Provisória .*"),
                                   stringr::str_replace_all(autor_raw, "Comissão Mista da Medida Provisória .*",
                                                            "Comissão Mista"), autor_raw)) %>% 
    dplyr::select(id_principal, id_documento, casa, autor_raw)
  
  autores_metadata <- 
    purrr::map2_df(autores_raw$autor_raw, 
                                     autores_raw$id_documento,
                                     ~extract_autor_relacionadas_senado(.x, .y))
  
  autores <- 
    autores_raw %>% 
    dplyr::inner_join(autores_metadata, by="id_documento") %>% 
    dplyr::select(-autor_raw)
}

#' @title Agrupa os tipos dos documentos
#' @description Retorna um dataframe contendo dados dos documentos
#' com uma coluna a mais (tipo)
#' @param docs_data Dataframe com os todos os dados dos documentos
#' @return Dataframe
#' @export
add_tipo_evento_documento <- function(docs_data, documentos_scrap = F) {
  casa_prop <- docs_data$casa[1]
  docs <- tibble::tibble()
  if(documentos_scrap) {
    docs <- docs_data %>%
      fuzzyjoin::regex_left_join(senado_env$tipos_documentos_scrap, by = c(identificacao = "regex"), ignore_case = T) %>%
      dplyr::select(-regex) %>%
      dplyr::mutate(tipo = dplyr::if_else(is.na(tipo), "Outros", tipo))
  }else {
    if (casa_prop == 'camara') {
      docs <- docs_data %>%
        fuzzyjoin::regex_left_join(camara_env$tipos_documentos, by = c(descricao_tipo_documento = "regex"), ignore_case = T) %>%
        dplyr::select(-regex) %>%
        dplyr::mutate(tipo = dplyr::if_else(is.na(tipo), "Outros", tipo))
      
    } else if (casa_prop == 'senado') {
      docs <- docs_data %>%
        fuzzyjoin::regex_left_join(senado_env$tipos_documentos, by = c(sigla_tipo = "regex"), ignore_case = T) %>%
        dplyr::select(-regex) %>%
        dplyr::mutate(tipo = dplyr::if_else(is.na(tipo), "Outros", tipo))
      
    } else {
      warning('Casa inválida')
    } 
  }

  return(docs)

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

safe_fetch_proposicao <- purrr::safely(rcongresso::fetch_proposicao,otherwise = tibble::tibble())

#' @title Realiza busca das informações de um documento
#' @description Retorna dados de um documento caso a requisição seja bem-sucedida,
#' caso contrário retorna um Dataframe vazio
#' @param id_documento ID do documento
#' @param casa casa onde o documento foi apresentado
#' @return Dataframe com dados do documento
fetch_documento <- function(id_documento, casa) {
  fetch_prop_output <- safe_fetch_proposicao(id_documento, casa)
  if (!is.null(fetch_prop_output$error)) {
    print(fetch_prop_output$error)
  }
  return(fetch_prop_output$result)
}


safe_fetch_autores <- purrr::safely(rcongresso::fetch_autores,otherwise = tibble::tibble())

#' @title Realiza busca dos autores de um documento
#' @description Retorna autores de um documento caso a requisição seja bem-sucedida,
#' caso contrário retorna um Dataframe vazio
#' @param id_documento ID do documento
#' @param casa casa onde o documento foi apresentado
#' @param sigla_tipo Sigla do tipo do documento
#' @return Dataframe contendo dados dos autores do documento
fetch_autores_documento <- function(id_documento, casa, sigla_tipo) {
  fetch_prop_output <- safe_fetch_autores(id_documento, casa, sigla_tipo)
  autores_result <- fetch_prop_output$result
  if (!is.null(fetch_prop_output$error)) {
    print(fetch_prop_output$error)
  } else {
    autores_result <- autores_result %>%
      dplyr::mutate(id_documento = id_documento)
  }
  return(autores_result)
}

