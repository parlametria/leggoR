camara_env <- jsonlite::fromJSON(here::here("R/config/environment_camara.json"))
senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))

#' @title Cria tabela com atuacao os pesos dos documentos
#' @description Os pesos são calculados através da conta 1/n
#' onde n é a quantidade de autores distintos para um documento
#' @param autores_docs Dataframe com autores dos documentos
#' @return Dataframe
get_peso_documentos <- function(autores_docs) {
  if (!(agoradigital::check_dataframe(autores_docs))) {
    return(tibble::tibble())
  }

  autores_docs %>%
    dplyr::group_by(id_principal, casa, id_documento) %>%
    dplyr::summarise(peso_documento = 1/dplyr::n_distinct(id_autor))
}

#' @title Cria tabela com atuacao de documentos com seus respectivos tipos de documentos
#' @description Retorna um dataframe contendo informações com os autores dos documentos e seus tipos
#' @param documentos_df Dataframe dos documentos
#' @param autores_df Dataframe com autores dos documentos
#' @param limiar limiar para filtrar os documentos
#' @param data_inicio Data limite inferior para documentos de interesse
#' @param data_fim Data limite superior para documentos de interesse
#' @return Dataframe
#' @export
create_tabela_atuacao_camara <- function(documentos_df, autores_df, data_inicio = NULL, data_fim = NULL, limiar = 0.1) {

  if (!(agoradigital::check_dataframe(documentos_df)) ||
      (!agoradigital::check_dataframe(autores_df))) {
    return(tibble::tibble())
  }

  if(!is.null(data_inicio)) {
    documentos_df <- documentos_df %>% dplyr::filter(data_apresentacao >= data_inicio)
  }

  if(!is.null(data_fim)) {
    documentos_df <- documentos_df %>% dplyr::filter(data_apresentacao < data_fim)
  }

  autores_docs <- merge(documentos_df, autores_df, by = c("id_documento", "id_principal", "casa")) %>%
    dplyr::select(id_principal,
                  casa,
                  id_documento,
                  id_autor,
                  nome_autor = nome,
                  tipo_autor,
                  sigla_tipo,
                  partido,
                  uf,
                  sigla_local = status_proposicao_sigla_orgao,
                  descricao_tipo_documento)

  peso_documentos <-
    get_peso_documentos(autores_docs)

  atuacao_df <-
    autores_docs %>%
    dplyr::left_join(peso_documentos, by = c('id_principal', 'casa', 'id_documento')) %>%
    dplyr::filter(peso_documento >= limiar) %>%
    agoradigital::add_tipo_evento_documento() %>%
    dplyr::rename(tipo_generico = tipo) %>%
    dplyr::group_by(id_ext = id_principal,
                    casa,
                    id_autor,
                    tipo_generico,
                    sigla_local, 
                    tipo_acao) %>%
    dplyr::summarise(peso_total_documentos = sum(peso_documento),
                     num_documentos = dplyr::n_distinct(id_documento),
                     partido = dplyr::first(partido),
                     uf = dplyr::first(uf),
                     nome_autor = dplyr::first(nome_autor),
                     tipo_autor = dplyr::first(tipo_autor)) %>%
    dplyr::arrange(id_ext, -peso_total_documentos) %>%
    dplyr::ungroup()

  atuacao_df <- .detect_sigla_local(atuacao_df, camara_env)

  return(atuacao_df)
}

#' @title Cria tabela com atuacao de documentos com seus respectivos tipos de documentos
#' @description Retorna um dataframe contendo informações com os autores dos documentos e seus tipos
#' @param documentos_df Dataframe dos documentos
#' @param autores_df Dataframe com autores dos documentos
#' @param data_inicio Data limite inferior para documentos de interesse
#' @param data_fim Data limite superior para documentos de interesse
#' @param limiar Limiar para filtrar os documentos
#' @return Dataframe
#' @export
create_tabela_atuacao_senado <- function(documentos_df, autores_df, data_inicio = NULL, data_fim = NULL, limiar = 0.1) {
  atuacao_df <- tibble::tibble()

  if (!(agoradigital::check_dataframe(documentos_df)) ||
      (!agoradigital::check_dataframe(autores_df))) {
    return(tibble::tibble())
  }

  if(!is.null(data_inicio)) {
    documentos_df <- documentos_df %>% dplyr::filter(data_texto >= data_inicio)
  }

  if(!is.null(data_fim)) {
    documentos_df <- documentos_df %>% dplyr::filter(data_texto < data_fim)
  }

  autores_docs <-
    merge(documentos_df, autores_df %>% dplyr::filter(!is.na(id_autor)), by = c("id_principal", "id_documento", "casa")) %>%
    dplyr::mutate(identificacao = descricao_texto) %>%
    dplyr::mutate(identificacao = stringr::str_trim(identificacao)) %>%
    dplyr::mutate(
      identificacao_comissao_nome_comissao = dplyr::if_else(
        str_detect(
          identificacao_comissao_nome_comissao,
          "Mesa Diretora do Congresso Nacional"
        ) | is.na(identificacao_comissao_nome_comissao),
        "Outros",
        identificacao_comissao_nome_comissao
      )
    )

  senado_comissoes <-
    senado_env$comissoes_nomes %>%
    tibble::as_tibble() %>%
    dplyr::select(-comissoes_temporarias) %>%
    dplyr::mutate(comissoes_permanentes = paste0("Comissão ", comissoes_permanentes)) %>%
    rbind(list("Plenário", "Plen(á|a)rio")) %>%
    rbind(list("Comissão Especial", "Especial")) %>%
    rbind(list("Outros", "Outros"))

  autores_docs <-
    fuzzyjoin::regex_left_join(autores_docs, senado_comissoes, by=c("identificacao_comissao_nome_comissao" = "comissoes_permanentes")) %>%
    dplyr::select(-c(identificacao_comissao_nome_comissao, comissoes_permanentes)) %>%
    dplyr::rename(sigla_local = siglas_comissoes)

  peso_documentos <-
    get_peso_documentos(autores_docs)

  if (nrow(autores_docs) > 0) {
    atuacao_df <-
      autores_docs %>%
      dplyr::left_join(peso_documentos, by = c('id_principal', 'casa', 'id_documento')) %>%
      dplyr::filter(peso_documento >= limiar) %>%
      dplyr::mutate(id_principal = as.numeric(id_principal)) %>%
      agoradigital::add_tipo_evento_documento() %>%
      dplyr::rename(tipo_generico = tipo) %>%
      dplyr::group_by(id_ext = id_principal,
                      casa,
                      id_autor,
                      tipo_generico,
                      tipo_acao,
                      sigla_local) %>%
      dplyr::summarise(peso_total_documentos = sum(peso_documento),
                       num_documentos = dplyr::n_distinct(id_documento),
                       partido = dplyr::first(partido),
                       uf = dplyr::first(uf),
                       nome_autor = dplyr::first(nome_autor),
                       tipo_autor = dplyr::first(tipo_autor)) %>%
      dplyr::arrange(id_ext, -peso_total_documentos) %>%
      dplyr::ungroup()

    atuacao_df <-
      .detect_sigla_local(atuacao_df, senado_env)
  }

  return(atuacao_df)
}

#' @title Detecta comissoes importantes da Camara e Senado
#' @description Retorna um dataframe contendo informacoes de importancia de comissoes
#' @param atuacao_df Dataframe dos atuacao
#' @param casa_env Camara ou Senado
#' @return Dataframe
.detect_sigla_local <- function(atuacao_df, casa_env) {
  atuacao_df <- atuacao_df %>%
    dplyr::mutate(is_important = dplyr::if_else(is.na(sigla_local),FALSE,
                                                dplyr::if_else((sigla_local %in% c(casa_env$comissoes_nomes$siglas_comissoes) |
                                                                  stringr::str_detect(tolower(sigla_local), 'pl') |
                                                                  stringr::str_detect(tolower(sigla_local), 'pec') |
                                                                  stringr::str_detect(tolower(sigla_local), 'mpv')),TRUE,FALSE)))

  return(atuacao_df)
}

#' @title Remove atuação dos deputados na fase de comissão mista em medidas provisórias
#' @description Já que a atuação dos deputados na fase de comissão mista já é recuperada pela API do senado, esta
#' função remove da atuação dos deputados as ocorrências de apresentação de documentos na comissão mista retornadas
#' pela API da Câmara, com o objetivo de evitar duplicações na atuação.
#' @param atuacao_df_camara Dataframe com atuação dos deputados em documentos
#' @return Dataframe com atuação dos deputados com exceção dos documentos assinados na fase de comissão mista para MPVs
#' @export
remove_atuacao_camara_comissao_mista <- function(atuacao_df_camara) {
  atuacao_df_camara_alt <- atuacao_df_camara %>%
    filter(!(casa == "camara" & stringr::str_detect(sigla_local, "MPV")))

  return(atuacao_df_camara_alt)
}
