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
      ## indexacao_materia = ifelse(
      ##   "indexacao_materia" %in% names(.),
      ##   indexacao_materia,
      ##   NA),
      ## palavras_chave = indexacao_materia,
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
                     casa_origem = ifelse(autor_df %>% head(1) %>% dplyr::select(codTipo) == 40000,"senado","camara"),
                     autor_nome = paste(unlist(t(autor_df$nome)),collapse="+"),
                     autor_uf = ifelse(autor_df %>%
                                        autor_df$codTipo ==  40000,
                                       "senado-u",
                                       (paste(unlist(t(autor_df$ultimoStatus.siglaUf)),collapse="+"))),
                     autor_partido = ifelse(autor_df %>%
                                              autor_df$codTipo ==  40000,
                                            "senado-p",
                                            paste(unlist(t(autor_df$ultimoStatus.siglaPartido)),collapse="+")),
                     apelido_materia = apelido,
                     tema = tema)
  proposicao
}

