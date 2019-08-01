camara_env <- jsonlite::fromJSON(here::here("R/config/environment_camara.json"))
senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))

#' @title Cria tabela com atores de documentos com seus respectivos tipos de documentos
#' @description Retorna um dataframe contendo informações com os autores dos documentos e seus tipos
#' @param documentos_df Dataframe dos documentos
#' @param autores_df Dataframe com autores dos documentos
#' @return Dataframe
#' @export
create_tabela_atores_camara <- function(documentos_df, autores_df) {

  if (!(agoradigital::check_dataframe(documentos_df)) ||
      (!agoradigital::check_dataframe(autores_df))) {
    return(tibble::tibble())
  }

  autores_docs <- merge(documentos_df, autores_df, by = c("id_documento", "casa")) %>%
    dplyr::select(id_principal,
                  casa,
                  id_documento,
                  id_autor,
                  nome_autor = nome,
                  sigla_tipo,
                  partido,
                  uf,
                  sigla_local = status_proposicao_sigla_orgao,
                  descricao_tipo_documento)

  atores_df <- autores_docs %>%
    agoradigital::add_tipo_evento_documento() %>%
    dplyr::rename(tipo_generico = tipo) %>%
    dplyr::group_by(id_ext = id_principal,
                    casa,
                    id_autor,
                    nome_autor,
                    partido,
                    uf,
                    tipo_generico,
                    sigla_local) %>%
    dplyr::summarise(qtd_de_documentos = dplyr::n()) %>%
    dplyr::arrange(id_ext, -qtd_de_documentos) %>%
    dplyr::ungroup()

  atores_df <- .detect_sigla_local(atores_df, camara_env)

  return(atores_df)
}

#' @title Cria tabela com atores de documentos com seus respectivos tipos de documentos
#' @description Retorna um dataframe contendo informações com os autores dos documentos e seus tipos
#' @param documentos_df Dataframe dos documentos
#' @param autores_df Dataframe com autores dos documentos
#' @return Dataframe
#' @export
create_tabela_atores_senado <- function(documentos_df, autores_df) {

  if (!(agoradigital::check_dataframe(documentos_df)) ||
      (!agoradigital::check_dataframe(autores_df))) {
    return(tibble::tibble())
  }

  autores_docs <- merge(documentos_df, autores_df, by = c("id_documento", "casa")) %>%
    dplyr::select(id_principal,
                  casa,
                  id_documento,
                  id_autor,
                  nome_autor = nome,
                  sigla_tipo,
                  partido,
                  uf,
                  sigla_local)

  atores_df <- autores_docs %>%
    agoradigital::add_tipo_evento_documento() %>%
    dplyr::rename(tipo_generico = tipo) %>%
    dplyr::group_by(id_ext = id_principal,
                    casa,
                    id_autor,
                    nome_autor,
                    partido,
                    uf,
                    tipo_generico,
                    sigla_local) %>%
    dplyr::summarise(qtd_de_documentos = dplyr::n()) %>%
    dplyr::arrange(id_ext, -qtd_de_documentos) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(!is.na(id_autor)) %>% 
    dplyr::mutate(sigla_local = NA,
                  is_important = FALSE)

  atores_df <- .detect_sigla_local(atores_df, senado_env)

  return(atores_df)
}

#' @title Detecta comissoes importantes da Camara e Senado
#' @description Retorna um dataframe contendo informacoes de importancia de comissoes
#' @param atores_df Dataframe dos atores
#' @param casa_env Camara ou Senado
#' @return Dataframe
.detect_sigla_local <- function(atores_df, casa_env) {
  atores_df <- atores_df %>%
    dplyr::mutate(is_important = dplyr::if_else(is.na(sigla_local),FALSE,
                                                dplyr::if_else((sigla_local %in% c(casa_env$comissoes_nomes$siglas_comissoes) |
                                                  stringr::str_detect(tolower(sigla_local), 'pl') |
                                                  stringr::str_detect(tolower(sigla_local), 'pec') |
                                                  stringr::str_detect(tolower(sigla_local), 'mpv')),TRUE,FALSE)))

  return(atores_df)
}
