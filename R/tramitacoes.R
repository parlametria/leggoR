#' @title Busca a movimentação da proposição
#' @description Retorna dataframe com os dados da movimentação da proposição, incluindo tramitação, prazos, despachos e situação
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param prop_id ID de uma proposição do Senado
#' @return Dataframe normalizado com as informações sobre a movimentação de uma proposição no Senado
#' @examples
#' fetch_tramitacao_senado(91341)
fetch_tramitacao_senado <- function(prop_id, isMPV = FALSE) {
    tramitacao <- 
      rcongresso::fetch_tramitacao_senado(prop_id) %>%
      dplyr::mutate(data_hora = lubridate::ymd_hm(paste(data_hora, "00:00")),
                    prop_id = as.integer(codigo_materia),
                    sequencia = as.integer(sequencia),
                    id_situacao = as.integer(situacao_codigo_situacao),
                    casa = "senado",
                    link_inteiro_teor = "") %>%
      dplyr::select(prop_id,
                    casa,
                    data_hora,
                    sequencia,
                    texto_tramitacao,
                    sigla_local = origem_tramitacao_local_sigla_local,
                    id_situacao,
                    descricao_situacao = situacao_descricao_situacao,
                    link_inteiro_teor,
                    origem_tramitacao_local_nome_casa_local,
                    destino_tramitacao_local_nome_casa_local)
  
  if(!isMPV) {
    tramitacao <-
      tramitacao %>% 
      dplyr::select(-c(origem_tramitacao_local_nome_casa_local, destino_tramitacao_local_nome_casa_local))
  }
    
    return (tramitacao)
}

#' @title Baixa os dados da tramitação de um Projeto de Lei
#' @description Retorna dataframe com os dados da tramitação de uma proposição no Congresso
#' @param id ID de uma proposição na sua respectiva casa
#' @param casa Casa onde a proposição está tramitando
#' @return Dataframe normalizado com os dados da tramitação de uma proposição no Congresso
#' @examples
#' fetch_tramitacao(91341, "senado")
#' @export
fetch_tramitacao <- function(id, casa, isMPV = FALSE) {
  casa <- tolower(casa)
  if (casa == "camara") {
    fetch_tramitacao_camara(id)
  } else if (casa == "senado") {
    fetch_tramitacao_senado(id, isMPV)
  } else {
    return("Parâmetro 'casa' não identificado.")
  }
}

#' @title Baixa os dados da tramitação da Câmara
#' @description Retorna dataframe com os dados da tramitação de uma proposição da Camara
#' @param bill_id ID de uma proposição na Camara
#' @param data_inicio Data inicio no formato AAAA-MM-DD
#' @param data_fim Data final no formato AAAA-MM-DD
#' @return Dataframe com os dados da tramitação de uma proposição da Camara
#' @examples
#' fetch_tramitacao_camara(2121442)
fetch_tramitacao_camara <- function(bill_id, data_inicio = NA, data_fim = NA) {
  rcongresso::fetch_tramitacao_camara(bill_id, data_inicio, data_fim) %>%
    dplyr::mutate(data_hora = lubridate::ymd_hm(stringr::str_replace(data_hora,"T"," ")),
                  casa = "camara",
                  id_situacao = as.integer(cod_tipo_tramitacao)) %>%
    dplyr::select(prop_id = id_prop,
                  casa,
                  data_hora,
                  sequencia,
                  texto_tramitacao = despacho,
                  sigla_local = sigla_orgao,
                  id_situacao,
                  descricao_situacao,
                  link_inteiro_teor = url)
}

#' @title Baixa dados das tramitações das proposições
#' @description Escreve as tramitações dos ids passados
#' @param export_path Path para ser escritos as tramitações
#' @param id_documento Id do documento para ser baixado
#' @param id_principal Id da proposição principal
write_docs <- function(export_path, id_documento, id_principal) {
  path <- paste0(export_path, "/", id_principal, "_camara")
  ifelse(!dir.exists(file.path(path)), dir.create(file.path(path)), FALSE)
  destfile <- paste0(path, "/", id_documento, ".csv")
  if(file.exists(destfile)){
    tram_antigo <- 
      readr::read_csv(destfile, col_types = readr::cols(
        prop_id = readr::col_double(),
        casa = readr::col_character(),
        data_hora = readr::col_datetime(format = ""),
        sequencia = readr::col_double(),
        texto_tramitacao = readr::col_character(),
        sigla_local = readr::col_character(),
        id_situacao = readr::col_double(),
        descricao_situacao = readr::col_character(),
        link_inteiro_teor = readr::col_character(),
        id_principal = readr::col_double()
      ))
    tram_novo <- 
      fetch_tramitacao_camara(id_documento, lubridate::today() - 14, lubridate::today()) %>% 
      dplyr::mutate(id_principal = id_documento)
    tram <-
      dplyr::bind_rows(tram_antigo, tram_novo) %>% 
      unique()
    readr::write_csv(tram, destfile, append = T)
  } else {
    tram <- 
      fetch_tramitacao_camara(id_documento) %>% 
      dplyr::mutate(id_principal = id_documento)
    readr::write_csv(tram, destfile)
  }

}

#' @title Write_docs seguro
safe_write_docs <- purrr::safely(
  write_docs,
  otherwise = tibble::tibble())

#' @title Baixa dados das tramitações das proposições
#' @description Escreve as tramitações dos ids passados
#' @param export_path Path para ser escritos as tramitações
#' @param docs_ids Dataframe com os IDs dos documentos a serem baixados
#' @export
fetch_tramitacao_data <- function(export_path, docs_ids) {
    purrr::pmap(list(export_path, docs_ids$id_documento, docs_ids$id_principal), function(a, b, c) safe_write_docs(a, b, c))
}

#' @title Baixa os dados da tramitação de vários Projetos de Lei
#' @description Retorna dataframe com os dados da tramitação de proposições no Congresso
#' @param id ID de uma proposição na sua respectiva casa
#' @param casa Casa onde a proposição está tramitando
#' @return Dataframe com os dados da tramitação de proposições no Congresso
#' @examples
#' all_pls <- readr::read_csv('data/tabela_geral_ids_casa.csv')
#' fetch_tramitacoes(all_pls)
#' @export
fetch_tramitacoes <- function(pls_ids) {
    purrr::map2_df(pls_ids$id, pls_ids$casa, ~ fetch_tramitacao(.x, .y))
}
