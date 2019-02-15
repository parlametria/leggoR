source(here::here("R/utils.R"))

#' @title Renomeia as colunas do dataframe dos detalhes da proposição no Senado
#' @description Renomeia as colunas do dataframe dos detalhes da proposição no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe dos detalhes da proposição no Senado
#' @return Dataframe com as colunas renomeadas
#' @export
rename_proposicao_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace("identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

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
    print('Parâmetro "casa" não identificado.')
  }
  
  prop_df <- fetch_proposicao(prop_id,casa,apelido, tema, TRUE)
  tram_df <- fetch_tramitacao(prop_id,casa, TRUE)
  emendas_df <- fetch_emendas(prop_id,casa)
  
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
#' @param normalized Se os dados vão ser normalizados
#' @param emendas Se vai ser usando na função fetch_emendas
#' @return Dataframe com as informações detalhadas de uma proposição
#' @examples
#' fetch_proposicao(129808, 'senado', 'Cadastro Positivo', 'Agenda Nacional', T, F)
#' @export
fetch_proposicao <- function(id, casa, apelido="", tema="", normalized=TRUE, emendas=FALSE) {
  casa <- tolower(casa)
  if (casa == "camara") {
    fetch_proposicao_camara(id, normalized, apelido, tema, emendas)
  } else if (casa == "senado") {
    fetch_proposicao_senado(id, normalized, apelido, tema)
  } else {
    print("Parâmetro 'casa' não identificado.")
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
  purrr::map2_df(pls_ids$id, pls_ids$casa, ~ fetch_proposicao(.x, .y, TRUE))
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
#' fetch_proposicao_senado(129808, T, 'Cadastro Positivo', 'Agenda Nacional')
fetch_proposicao_senado <- function(proposicao_id, normalized=TRUE, apelido, tema) {
  url_base_proposicao <-
    "http://legis.senado.leg.br/dadosabertos/materia/"
  da_url <- paste0(url_base_proposicao, proposicao_id)
  
  page_url_senado <-
    "https://www25.senado.leg.br/web/atividade/materias/-/materia/"
  
  parse_autor = function(row) {
    return(paste(
      paste(
        row[["NomeAutor"]],
        row[["IdentificacaoParlamentar.SiglaPartidoParlamentar"]]),
      row[["UfAutor"]], sep = "/"))
  }
  
  json_proposicao <- fetch_json_try(da_url)
  proposicao_data <- json_proposicao$DetalheMateria$Materia
  proposicao_ids <-
    proposicao_data$IdentificacaoMateria %>%
    tibble::as.tibble()
  proposicao_basic_data <-
    proposicao_data$DadosBasicosMateria %>%
    purrr::flatten() %>%
    tibble::as.tibble()
  proposicao_author <-
    proposicao_data$Autoria[[1]] %>%
    tibble::as.tibble() %>%
    purrrlyr::by_row(parse_autor) %>%
    .[[".out"]] %>%
    paste(collapse = ", ")
  proposicao_specific_assunto <-
    proposicao_data$Assunto$AssuntoEspecifico %>%
    tibble::as.tibble() 
  if (nrow(proposicao_specific_assunto) == 0) {
    proposicao_specific_assunto <- 
      tibble::tribble(~ codigo_assunto_especifico, ~ assunto_especifico,
                      0, "Não especificado")
  }else {
    proposicao_specific_assunto <- 
      proposicao_specific_assunto %>%
      dplyr::rename(assunto_especifico = Descricao, codigo_assunto_especifico = Codigo)
  }
  proposicao_general_assunto <-
    proposicao_data$Assunto$AssuntoGeral %>%
    tibble::as.tibble()
  if (nrow(proposicao_general_assunto) == 0) {
    proposicao_general_assunto <- 
      tibble::tribble(~ codigo_assunto_geral, ~ assunto_geral,
                      0, "Não especificado")
  }else {
    proposicao_general_assunto <- 
      proposicao_general_assunto %>%
      dplyr::rename(assunto_geral = Descricao, codigo_assunto_geral = Codigo)
  }
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
      !!!proposicao_ids,
      !!!proposicao_specific_assunto,
      !!!proposicao_general_assunto,
      !!!proposicao_source,
      autor_nome = proposicao_author,
      page_url = paste0(page_url_senado, proposicao_id),
      proposicoes_relacionadas = paste(relacionadas, collapse = " "),
      proposicoes_apensadas = paste(anexadas, collapse = " ")
    )
  
  proposicao_complete <-
    proposicao_complete[,!sapply(proposicao_complete, is.list)]
  
  proposicao_complete <- rename_proposicao_df(proposicao_complete)
  
  if (normalized) {
    proposicao_complete <-
      proposicao_complete %>%
      dplyr::mutate(
        prop_id = as.integer(codigo_materia),
        numero = as.integer(numero_materia),
        ano = as.integer(ano_materia),
        data_apresentacao = lubridate::ymd_hm(paste(data_apresentacao, "00:00")),
        casa = "senado",
        apelido_materia = ifelse(
          "apelido_materia" %in% names(.),
          apelido_materia,
          apelido),
        tema = tema,
        indexacao_materia = ifelse(
          "indexacao_materia" %in% names(.),
          indexacao_materia,
          NA)
      ) %>%
      dplyr::select(prop_id,
                    casa,
                    tipo_materia = sigla_subtipo_materia,
                    numero,
                    ano,
                    data_apresentacao,
                    ementa = ementa_materia,
                    palavras_chave = indexacao_materia,
                    casa_origem = nome_casa_origem,
                    autor_nome,
                    apelido_materia,
                    tema)
  }
  
  proposicao_complete
}

#' @title Baixa dados sobre uma proposição
#' @description Retorna um dataframe contendo dados sobre uma proposição
#' @param prop_id Um ou mais IDs de proposições
#' @param normalized whether or not the output dataframe should be normalized (have the same format and column names for every house)
#' @param apelido Apelido da proposição
#' @param tema Tema da proposição
#' @param emendas se vai ser usado na função fetch_emendas
#' @return Dataframe
#' @examples
#' fetch_proposicao_camara(2056568, T, "Lei para acabar zona de amortecimento", "Meio Ambiente", F)
fetch_proposicao_camara <- function(prop_id, normalized=TRUE, apelido, tema, emendas=FALSE) {
  prop_camara <- rcongresso::fetch_proposicao(prop_id) %>%
    rename_df_columns()
  
  if (normalized) {
    autor_df <- extract_autor_in_camara(prop_id)
    
    prop_camara <- prop_camara %>%
      dplyr::mutate(prop_id = as.integer(id),
                    numero = as.integer(numero),
                    ano = as.integer(ano),
                    ementa = paste(ementa,ementa_detalhada),
                    data_apresentacao = lubridate::ymd_hm(stringr::str_replace(data_apresentacao,'T',' ')),
                    casa = 'camara',
                    casa_origem = autor_df[1,]$casa_origem,
                    autor_nome = autor_df[1,]$autor.nome,
                    apelido_materia = apelido,
                    tema = tema) 
    if (emendas) {
      prop_camara <-
        prop_camara %>%
        dplyr::select(prop_id,
                      casa,
                      tipo_materia = sigla_tipo,
                      numero,
                      ano,
                      data_apresentacao,
                      ementa,
                      palavras_chave = keywords,
                      autor_nome,
                      casa_origem,
                      apelido_materia,
                      tema,
                      status_proposicao_sigla_orgao)
    }else {
      prop_camara <-
        prop_camara %>%
        dplyr::select(prop_id,
                      casa,
                      tipo_materia = sigla_tipo,
                      numero,
                      ano,
                      data_apresentacao,
                      ementa,
                      palavras_chave = keywords,
                      autor_nome,
                      casa_origem,
                      apelido_materia,
                      tema)
    }
    
  }
  
  prop_camara
}