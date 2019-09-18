#' @title Busca a movimentação da proposição
#' @description Retorna dataframe com os dados da movimentação da proposição, incluindo tramitação, prazos, despachos e situação
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param prop_id ID de uma proposição do Senado
#' @param data_ref Data de referencia a partir da qual a tramitação será retornada
#' @return Dataframe normalizado com as informações sobre a movimentação de uma proposição no Senado
#' @examples
#' fetch_tramitacao_senado(91341)
fetch_tramitacao_senado <- function(prop_id, data_ref = NA) {
    tramitacao <- 
      rcongresso::fetch_tramitacao_senado(prop_id, data_ref) %>%
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
  tramitacao <- rcongresso::fetch_tramitacao_camara(bill_id, data_inicio, data_fim) 
  if (nrow(tramitacao) == 0) {
    tibble::tribble(~prop_id, ~casa, ~data_hora, ~sequencia, ~texto_tramitacao, ~sigla_local,
                   ~id_situacao, ~descricao_situacao, ~link_inteiro_teor)
  }else {
    tramitacao %>% 
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
}

#' @title Baixa dados das tramitações das proposições
#' @description Escreve as tramitações dos ids passados
#' @param casa senado ou camara
#' @param export_path Path para ser escritos as tramitações
#' @param id_documento Id do documento para ser baixado
#' @param id_principal Id da proposição principal
#' @param dias_atras Quantos dias serão diminuidos da data de hoje para
#' retornar a tramitação
write_docs <- function(casa, export_path, id_documento, id_principal, dias_atras) {
  path <- paste0(export_path, "/", id_principal, "_", casa)
  ifelse(!dir.exists(file.path(path)), dir.create(file.path(path)), FALSE)
  if (casa == 'camara') {
    destfile <- paste0(path, "/", id_documento, ".csv")
  }else {
    destfile <- paste0(path, "/", id_principal, ".csv")
  }
  
  if(file.exists(destfile)){
    tram <- aux_write_docs_tram_antiga(destfile, casa, id_documento, dias_atras)
  } else {
    tram <- aux_write_docs_tram_nova(casa, id_documento)
  }
  
  readr::write_csv(tram, destfile)
}

#' @title Baixa dados das tramitações novas das proposições
#' @description Escreve as tramitações dos ids passados
#' @param casa senado ou camara
#' @param id_documento Id do documento para ser baixado
aux_write_docs_tram_nova <- function(casa, id_documento) {
  if (casa == 'camara') {
    tram <- 
      fetch_tramitacao_camara(id_documento)
  } else {
    tram <- 
      fetch_tramitacao_senado(id_documento) 
  }
  
  tram %>% 
    dplyr::mutate(id_principal = id_documento)
}

#' @title Baixa dados das tramitações antigas das proposições
#' @description Escreve as tramitações dos ids passados, fazendo
#' um join das proposições antigas com as novas
#' @param casa senado ou camara
#' @param destfile Path onde está a tramitação antiga
#' @param id_documento Id do documento para ser baixado
#' @param dias_atras Quantos dias serão diminuidos da data de hoje para
#' retornar a tramitação
aux_write_docs_tram_antiga <- function(destfile, casa, id_documento, dias_atras) {
  tram_antigo <-
    readr::read_csv(destfile, col_types = readr::cols(
      .default = readr::col_character(),
      prop_id = readr::col_double(),
      data_hora = readr::col_datetime(format = ""),
      sequencia = readr::col_double(),
      id_situacao = readr::col_double(),
      id_principal = readr::col_double()
    ))
  
  if (casa == 'camara') {
    tram_novo <-
      fetch_tramitacao_camara(id_documento, lubridate::today() - dias_atras, lubridate::today()) %>%
      dplyr::mutate(id_principal = id_documento)
  } else {
    tram_novo <- 
      fetch_tramitacao_senado(id_documento, format(lubridate::today() - dias_atras, "%Y%m%d")) %>% 
      dplyr::mutate(id_principal = id_documento,
                    link_inteiro_teor = dplyr::if_else(link_inteiro_teor == NA, '', link_inteiro_teor))
  }
  
  dplyr::bind_rows(tram_antigo, tram_novo) %>%
    unique() 
}

#' @title Write_docs seguro
safe_write_docs <- purrr::safely(
  write_docs,
  otherwise = tibble::tibble())

#' @title Baixa dados das tramitações das proposições
#' @description Escreve as tramitações dos ids passados
#' @param casa senado ou camara
#' @param export_path Path para ser escritos as tramitações
#' @param docs_ids Dataframe com os IDs dos documentos a serem baixados
#' @param dias_atras Quantos dias serão diminuidos da data de hoje para
#' retornar a tramitação
#' @export
fetch_tramitacao_data <- function(casa, export_path, docs_ids, dias_atras) {
  purrr::pmap(list(casa, export_path, docs_ids$id_documento, docs_ids$id_principal, dias_atras), function(a, b, c, d, e) safe_write_docs(a, b, c, d, e))
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
