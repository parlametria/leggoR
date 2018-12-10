source(here::here("R/utils.R"))
senado_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
senado_constants <- senado_env$constants

fetch_json_try <- function(url) {
  count <- 0
  repeat {
    json_data <- NULL
    tryCatch({
      json_data <- jsonlite::fromJSON(url, flatten = T)
    },
    error = function(msg) {
    })
    if (!is.null(json_data) & is.null(json_data$ERROR)) {
      break
    } else {
      print("Erro ao baixar dados, tentando outra vez...")
      count <- count + 1
      print(paste("Tentativas: ", count))
      Sys.sleep(2)
    }
  }
  return(json_data)
}

#' @title Busca votações de uma proposição no Senado
#' @description Retorna dataframe com os dados das votações de uma proposição no Senado.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as votações de uma proposição no Senado
#' @examples
#' fetch_votacoes(91341)
#' @export
fetch_votacoes <- function(proposicao_id) {
  url_base_votacoes <-
    paste0(senado_env$endpoints_api$url_base, "votacoes/")
  
  url <- paste0(url_base_votacoes, proposicao_id)
  json_votacoes <- fetch_json_try(url)
  votacoes_data <-
    json_votacoes %>%
    magrittr::extract2("VotacaoMateria") %>%
    magrittr::extract2("Materia")
  votacoes_ids <-
    votacoes_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as.tibble() %>%
    unique()
  votacoes_df <-
    votacoes_data %>%
    magrittr::extract2("Votacoes") %>%
    purrr::map_df( ~ .) %>%
    tidyr::unnest()
  
  votacoes_df <-
    votacoes_df %>%
    tibble::add_column(!!!votacoes_ids)
  
  votacoes_df <- votacoes_df[,!sapply(votacoes_df, is.list)]
  rename_votacoes_df(votacoes_df)
}

#' @title Busca a movimentação da proposição
#' @description Retorna dataframe com os dados da movimentação da proposição, incluindo tramitação, prazos, despachos e situação
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @param normalized whether or not the output dataframe should be normalized (have the same format and column names for every house)
#' @return Dataframe com as informações sobre a movimentação de uma proposição no Senado
#' @examples
#' fetch_tramitacao_senado(91341)
fetch_tramitacao_senado <- function(proposicao_id, normalized=FALSE) {
  url <-
    paste0(senado_env$endpoints_api$url_base,
           "movimentacoes/",
           proposicao_id)
  
  json_tramitacao <- fetch_json_try(url)
  
  tramitacao_data <-
    json_tramitacao %>%
    magrittr::extract2("MovimentacaoMateria") %>%
    magrittr::extract2("Materia")
  tramitacao_ids <-
    tramitacao_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as.tibble()
  tramitacao_actual_situation <-
    tramitacao_data %>%
    magrittr::extract2("SituacaoAtual") %>%
    magrittr::extract2("Autuacoes") %>%
    magrittr::extract2("Autuacao") %>%
    magrittr::extract2("Situacao") %>%
    tibble::as.tibble()
  proposicao_tramitacoes_df <-
    tramitacao_data %>%
    magrittr::extract2("Tramitacoes") %>%
    magrittr::extract2("Tramitacao") %>%
    tibble::as.tibble() %>%
    tibble::add_column(!!!tramitacao_ids)
  
  proposicao_tramitacoes_df <-
    proposicao_tramitacoes_df[, !sapply(proposicao_tramitacoes_df, is.list)]
  
  proposicao_tramitacoes_df <-
    rename_tramitacao_df(proposicao_tramitacoes_df) %>%
    dplyr::rename(data_hora = data_tramitacao, sequencia = numero_ordem_tramitacao)
  
  if (normalized) {
    proposicao_tramitacoes_df <- proposicao_tramitacoes_df %>%
      dplyr::mutate(data_hora = lubridate::ymd_hm(paste(data_hora, "00:00")),
                    prop_id = as.integer(codigo_materia),
                    sequencia = as.integer(sequencia),
                    id_situacao = as.integer(situacao_codigo_situacao),
                    casa = "senado") %>%
      dplyr::select(prop_id,
                    casa,
                    data_hora,
                    sequencia,
                    texto_tramitacao,
                    sigla_local = origem_tramitacao_local_sigla_local,
                    id_situacao,
                    descricao_situacao = situacao_descricao_situacao)
  }
  
  proposicao_tramitacoes_df
}

#' @title Deferimento de requerimentos.
#' @description Verifica deferimento ou não para uma lista de IDs de requerimentos.
#' @param proposicao_id ID de um ou vários requerimentos
#' @return Dataframe com IDs dos requerimentos e informação sobre deferimento.
#' @examples
#' fetch_deferimento(c("102343", "109173", "115853"))
#' @importFrom utils tail
#' @export
fetch_deferimento <- function(proposicao_id) {
  deferimento_regexes <- senado_env$deferimento
  regexes <-
    tibble::frame_data(
      ~ deferimento,
      ~ regex,
      "indeferido",
      deferimento_regexes$regex$indeferido,
      "deferido",
      deferimento_regexes$regex$deferido
    )
  
  fetch_one_deferimento <- function(proposicao_id) {
    json <-
      paste0(senado_env$endpoints_api$url_base,
             "movimentacoes/",
             proposicao_id) %>%
      jsonlite::fromJSON()
    
    resultados <-
      json$MovimentacaoMateria$Materia$OrdensDoDia$OrdemDoDia$DescricaoResultado
    # handle NULL
    if (is.null(resultados))
      resultados <- c('')
    
    resultados %>%
      tibble::as.tibble() %>%
      dplyr::mutate(proposicao_id = proposicao_id) %>%
      fuzzyjoin::regex_left_join(regexes, by = c(value = "regex")) %>%
      tidyr::fill(deferimento) %>%
      tail(., n = 1) %>%
      dplyr::select(proposicao_id, deferimento)
  }
  
  proposicao_id %>%
    unlist %>%
    unique %>%
    lapply(fetch_one_deferimento) %>%
    plyr::rbind.fill()
}

#' @title Renomeia as colunas do dataframe passado para o formato underscore
#' @description Renomeia as colunas do dataframe usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe do Senado
#' @return Dataframe com as colunas renomeadas
#' @export
rename_table_to_underscore <- function(df) {
  new_names = names(df) %>%
    to_underscore()
  
  names(df) <- new_names
  
  df
}

#' @title Renomeia as colunas do dataframe de votação no Senado
#' @description Renomeia as colunas do dataframe de votação no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe da votação no Senado
#' @return Dataframe com as colunas renomeadas
#' @export
rename_votacoes_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace(
      "sessao_plenaria_|tramitacao_identificacao_tramitacao_|identificacao_parlamentar_",
      ""
    )
  
  names(df) <- new_names
  
  df
}

#' @title Renomeia as colunas do dataframe de movimentação no Senado
#' @description Renomeia as colunas do dataframe de movimentação no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe da votação no Senado
#' @return Dataframe com as colunas renomeadas
#' @export
rename_tramitacao_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace(
      "identificacao_tramitacao_|
      identificacao_tramitacao_origem_tramitacao_local_|
      identificacao_tramitacao_destino_tramitacao_local_|
      identificacao_tramitacao_situacao_",
      ""
    )
  
  names(df) <- new_names
  
  df
}

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

#' @title Retorna a composição da comissão 
#' @description Retorna lista com os dados dos membros de uma comissão
#' @param sigla sigla da comissão
#' @return Lista com os dados dos membros de uma comissão
#' @examples
#' fetch_composicao_comissao("CCJ",'senado')
#' @export
fetch_composicao_comissao <- function(sigla, casa) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    warning("Function fetch_composicao_comissao_camara not implemented yet.")
    return(NULL)
  } else if (casa == 'senado') {
    fetch_composicao_comissoes_senado(sigla)
  } else {
    print('Parâmetro "casa" não identificado.')
  }
}

#' @title Retorna a composição da comissão do senado
#' @description Retorna uma lista com dois dataframes, um contendo os membros das comissões
#' e o outro contendo quem são os presidentes
#' @param sigla Sigla da comissão do Senado
#' @return List com dois dataframes
fetch_composicao_comissoes_senado <- function(sigla) {
  url <- paste0('http://legis.senado.leg.br/dadosabertos/comissao/', sigla)
  json_sessions <- jsonlite::fromJSON(url, flatten = T)
  
  colegiado <-
    json_sessions %>%
    magrittr::extract2('DetalheComissao') %>%
    magrittr::extract2('COLEGIADO') %>%
    magrittr::extract2('COLEGIADO_ROW') 
  
  colegiado[sapply(colegiado, is.null)] <- NULL
  comissao <-
    colegiado %>% 
    tibble::as.tibble() 
  
  cargos <- 
    comissao %>%
    magrittr::extract2('CARGOS') %>%
    magrittr::extract2('CARGOS_ROW') %>%
    tibble::as.tibble()
  
  membros <- 
    comissao %>%
    magrittr::extract2('MEMBROS_BLOCO') %>%
    magrittr::extract2('MEMBROS_BLOCO_ROW')
  tibble::as.tibble() 
  if('PARTIDOS_BLOCO.PARTIDOS_BLOCO_ROW' %in% names(membros)) {
    membros <- 
      membros %>%
      dplyr::select(-PARTIDOS_BLOCO.PARTIDOS_BLOCO_ROW) %>% 
      tidyr::unnest()
  }
  membros <-
    membros %>%
    tidyr::unnest()
  
  membros %>%
    dplyr::left_join(cargos, by = 'HTTP') %>%
    dplyr::select(-c("@num.y", "PARLAMENTAR.y"))
}

#' @title Retorna as sessões deliberativas de uma proposição no Senado
#' @description Retorna dataframe com os dados das sessões deliberativas de uma proposição no Senado.
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as sessões deliberativas de uma proposição no Senado
#' @examples
#' fetch_sessions(91341)
#' @export
fetch_sessions <- function(bill_id) {
  url_base_sessions <-
    "http://legis.senado.leg.br/dadosabertos/materia/ordia/"
  url <- paste0(url_base_sessions, bill_id)
  
  json_sessions <- jsonlite::fromJSON(url, flatten = T)
  
  sessions_data <- json_sessions %>%
    magrittr::extract2("OrdiaMateria") %>%
    magrittr::extract2("Materia")
  
  ordem_do_dia_df <- sessions_data %>%
    magrittr::extract2("OrdensDoDia") %>%
    magrittr::extract2("OrdemDoDia") %>%
    magrittr::extract2("SessaoPlenaria") %>%
    purrr::map_df( ~ .) %>%
    tidyr::unnest() %>%
    rename_table_to_underscore()
  
  ordem_do_dia_df
}

#' @title Retorna um dataframe a partir de uma coluna com listas encadeadas
#' @description Retorna um dataframe a partir de uma coluna com listas encadeadas.
#' @param column Coluna
#' @return Dataframe com as informações provenientes de uma coluna com listas encadeadas.
#' @examples
#' generate_dataframe(column)
#' @export
generate_dataframe <- function (column) {
  as.data.frame(column) %>%
    tidyr::unnest() %>%
    rename_df_columns()
}


#' @title Retorna as emendas de uma proposição no Congresso
#' @description Retorna dataframe com os dados das emendas de uma proposição no Congresso.
#' @param bill_id ID de uma proposição do Congresso
#' @return Dataframe com as informações sobre as emendas de uma proposição no Congresso.
#' @examples
#' fetch_emendas(91341,'senado')
#' @export
fetch_emendas <- function(id, casa) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    emendas <- fetch_emendas_camara(id)
  } else if (casa == 'senado') {
    emendas <- fetch_emendas_senado(id)
  } else {
    print('Parâmetro "casa" não identificado.')
    return()
  }
  
  emendas  <-
    emendas %>%
    dplyr::mutate(prop_id = id, codigo_emenda = as.integer(codigo_emenda)) %>%
    dplyr::select(
      prop_id, codigo_emenda, data_apresentacao, numero, local, autor, casa, tipo_documento, inteiro_teor) 
}

#' @title Retorna as emendas de uma proposição no Senado
#' @description Retorna dataframe com os dados das emendas de uma proposição no Senado.
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as emendas de uma proposição no Senado.
#' @examples
#' fetch_emendas_senado(91341)
fetch_emendas_senado <- function(bill_id) {
  url_base_emendas <-
    "http://legis.senado.leg.br/dadosabertos/materia/emendas/"
  url <- paste0(url_base_emendas, bill_id)
  
  json_emendas <- fetch_json_try(url)
  
  emendas_data <- json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia")
  
  emendas_df <- emendas_data %>%
    magrittr::extract2("Emendas") %>%
    purrr::map_df( ~ .) %>% rename_df_columns()
  
  num_emendas = nrow(emendas_df)
  
  if (num_emendas == 0) {
    emendas_df <-
      tibble::frame_data( ~ codigo_emenda, ~ data_apresentacao, ~ numero, ~ local, ~ autor, ~ partido, ~ casa, ~ tipo_documento, ~ inteiro_teor)

  } else if (num_emendas == 1) {
    texto <- generate_dataframe(emendas_df$textos_emenda) %>%
      dplyr::select(tipo_documento, url_texto)
    
    autoria <- generate_dataframe(emendas_df$autoria_emenda) %>%
      dplyr::mutate(
        partido = paste0(
          identificacao_parlamentar_sigla_partido_parlamentar,
          "/",
          identificacao_parlamentar_uf_parlamentar
        )
      )
    
    emendas_df <- emendas_df %>%
      plyr::rename(
        c(
          "numero_emenda" = "numero",
          "colegiado_apresentacao" = "local"
        )
      ) %>%
      dplyr::mutate(autor = autoria$nome_autor,
                    partido = autoria$partido,
                    tipo_documento = texto$tipo_documento,
                    inteiro_teor = texto$url_texto,
                    casa = 'senado') 
    
    
  } else{
    emendas_df <- emendas_df %>%
      tidyr::unnest() %>%
      plyr::rename(
        c(
          "numero_emenda" = "numero",
          "colegiado_apresentacao" = "local",
          "autoria_emenda_autor_nome_autor" = "autor",
          "textos_emenda_texto_emenda_url_texto" = "inteiro_teor",
          "textos_emenda_texto_emenda_tipo_documento" = "tipo_documento",
          "autoria_emenda_autor_identificacao_parlamentar_sigla_partido_parlamentar" = "partido",
          "autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar" = "uf"
        )
      ) %>%
      dplyr::mutate(
        partido = paste0(partido, "/", uf),
        casa = "senado"
      ) 

  }

  emendas_df %>%
    dplyr::mutate(autor = paste0(autor, " ", partido)) %>%
    dplyr::select(-partido)

}

#' @title Retorna as emendas de uma proposição na Camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
#' @param id ID de uma proposição da Camara
#' @param sigla Sigla da proposição
#' @param numero Numero da proposição
#' @param ano Ano da proposição
#' @return Dataframe com as informações sobre as emendas de uma proposição na Camara
#' @examples
#' fetch_emendas_camara(408406)
fetch_emendas_camara <- function(id=NA, sigla="", numero="", ano="") {
  if(is.na(id)) {
    url <- 
      paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal?tipo=', sigla, '&numero=', numero, '&ano=', ano)
  }else {
    prop <- fetch_proposicao(id, 'camara')
    url <- 
      paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal?tipo=', prop$tipo_materia, '&numero=', prop$numero, '&ano=', prop$ano)
  }
 
   eventos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    eventos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    magrittr::extract2('Emendas') %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame()
  
  if(nrow(df) == 0) {
    return(tibble::frame_data( ~ codigo_emenda, ~ data_apresentacao, ~ numero, ~ local, ~ autor, ~ casa, ~ tipo_documento, ~ inteiro_teor))
  }
  
  new_names <- c("cod_proposicao", "descricao")
  names(df) <- new_names
  
  emendas <- purrr::map_df(df$cod_proposicao, fetch_emendas_camara_auxiliar)
  normalizes_names <- c("codigo_emenda", "data_apresentacao", "numero", "local", "autor", "casa", "tipo_documento", "inteiro_teor")
  names(emendas) <- normalizes_names
  
  emendas
}

#' @title Função auxiliar para o fetch_emendas_camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
fetch_emendas_camara_auxiliar <- function(id) {
  fetch_proposicao(id, "camara", normalized = T, emendas = T) %>%
    dplyr::select(c(prop_id, data_apresentacao, numero, status_proposicao_sigla_orgao, autor_nome, casa, tipo_materia, ementa))
}

#' @title Baixa os dados da tramitação de um Projeto de Lei
#' @description Retorna dataframe com os dados da tramitação de uma proposição no Congresso
#' @param id ID de uma proposição na sua respectiva casa
#' @param casa Casa onde a proposição está tramitando
#' @param normalized whether or not the output dataframe should be normalized (have the same format and column names for every house)
#' @return Dataframe com os dados da tramitação de uma proposição no Congresso
#' @examples
#' fetch_tramitacao(91341,'senado')
#' @export
fetch_tramitacao <- function(id, casa, normalized=FALSE) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    fetch_tramitacao_camara(id, normalized)
  } else if (casa == 'senado') {
    fetch_tramitacao_senado(id, normalized)
  } else {
    print('Parâmetro "casa" não identificado.')
  }
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
  purrr::map2_df(pls_ids$id, pls_ids$casa, ~ fetch_tramitacao(.x, .y, TRUE))
}

#' @title Baixa os dados da tramitação da Câmara
#' @description Retorna dataframe com os dados da tramitação de uma proposição da Camara
#' @param bill_id ID de uma proposição na Camara
#' @param normalized Parametro para normalizar os dados
#' @return Dataframe com os dados da tramitação de uma proposição da Camara
#' @examples
#' fetch_tramitacao_camara(2121442, TRUE)
fetch_tramitacao_camara <- function(bill_id, normalized=FALSE) {
  tram_camara <- rcongresso::fetch_tramitacao(bill_id) %>%
    rename_df_columns
  
  if (normalized) {
    tram_camara <- tram_camara %>%
      dplyr::mutate(data_hora = lubridate::ymd_hm(stringr::str_replace(data_hora,'T',' ')),
                    casa = 'camara',
                    id_situacao = as.integer(id_tipo_tramitacao)) %>%
      dplyr::select(prop_id = id_prop,
                    casa,
                    data_hora,
                    sequencia,
                    texto_tramitacao = despacho,
                    sigla_local = sigla_orgao,
                    id_situacao,
                    descricao_situacao)
  }
  
  tram_camara
}

build_data_filepath <- function(folder_path,data_prefix,house,bill_id) {
  filename <- paste0(paste(bill_id,data_prefix,house, sep='-'),'.csv')
  filepath <- paste(folder_path, house, filename, sep='/')
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

###################################################################

#' @title Recupera o estado e partido de um autor
#' @description Retorna o estado e partido
#' @param uri uri que contém dados sobre o autor
#' @return Estado e partido
#' @export
extract_partido_estado_autor <- function(uri) {
  if (!is.na(uri)) {
    json_autor <- fetch_json_try(uri)
    
    autor <-
      json_autor %>%
      magrittr::extract2('dados')
    
    autor_uf <-
      autor %>%
      magrittr::extract2('ufNascimento')
    
    autor_partido <-
      autor %>%
      magrittr::extract2('ultimoStatus') %>%
      magrittr::extract2('siglaPartido')
    
    paste0(autor_partido, '/', autor_uf)
  } else {
    ''
  }
}

#' @title Recupera as proposições apensadas
#' @description Retorna os IDs das proposições apensadas a uma determinada proposição
#' @param prop_id ID da proposição
#' @return Ventor contendo os IDs das proposições apensadas
#' @examples
#' fetch_apensadas(2121442)
#' @export
fetch_apensadas <- function(prop_id) {
  api_v1_proposicao_url <- 'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp='
  xml2::read_xml(paste0(api_v1_proposicao_url, prop_id)) %>%
    xml2::xml_find_all('//apensadas/proposicao/codProposicao') %>%
    xml2::xml_text() %>%
    tibble::tibble(apensadas = .)
}

#' @title Recupera os eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' fetch_events(2121442)
#' @export
#' @importFrom utils timestamp
fetch_events <- function(prop_id) {
  events_base_url <-
    'http://www.camara.gov.br/proposicoesWeb/sessoes_e_reunioes?idProposicao='
  bill_events_url <- paste0(events_base_url, prop_id)
  events <- bill_events_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="content"]/table') %>%
    rvest::html_table()
  events_df <- events[[1]]
  names(events_df) <- c('timestamp', 'origem', 'descricao', 'links')
  events_df %>%
    dplyr::select(-links) %>%
    dplyr::mutate(timestamp = lubridate::dmy_hm(timestamp))
}

###################################################################

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

#' @title Baixa a pauta de uma reunião
#' @description Retorna um dataframe contendo dados sobre a pauta, função auxiliar usanda na
#' fetch_agenda_camara()
#' @param id id do evento
#' @param hora_inicio hora que começou o evento
#' @param hora_fim hora que finalizou o evento
#' @return Dataframe
#' @examples
#' fetch_pauta_camara('53277', '2018-07-03T10:00', '2018-07-03T12:37')
#' @importFrom dplyr mutate
#' @importFrom tibble as.tibble
fetch_pauta_camara <- function(id, hora_inicio, hora_fim, sigla_orgao, nome_orgao) {
  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/eventos/", id, "/pauta")
  json_proposicao <- fetch_json_try(url)
  
  json_proposicao$dados %>%
    tibble::as.tibble() %>%
    dplyr::mutate(hora_inicio = hora_inicio,
                  hora_fim = hora_fim,
                  sigla_orgao = sigla_orgao,
                  nome_orgao = nome_orgao)
}

#' @title Baixa a agenda da câmara
#' @description Retorna um dataframe contendo toda a agenda das sessões/reuniões deliberativas da Câmara
#' @param initial_date data inicial no formato yyyy-mm-dd
#' @param end_date data final no formato yyyy-mm-dd
#' @return Dataframe
#' @examples
#' fetch_agenda_camara('2018-07-03', '2018-07-10')
#' @importFrom dplyr filter
#' @importFrom dplyr do
#' @importFrom dplyr rowwise
#' @importFrom tidyr unnest
#' @importFrom tibble as.tibble
fetch_agenda_camara <- function(initial_date, end_date) {
  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/eventos?dataInicio=", initial_date, "&dataFim=", end_date, "&ordem=ASC&ordenarPor=dataHoraInicio")
  json_proposicao <- fetch_json_try(url)
  
  descricoes_inuteis <- c('Seminário', 'Diligência', 'Sessão Não Deliberativa de Debates', 'Reunião de Instalação e Eleição', 'Outro Evento', 'Mesa Redonda', 'Sessão Não Deliberativa Solene')
  agenda <-
    json_proposicao$dados %>%
    tibble::as.tibble() %>%
    dplyr::filter(descricaoSituacao != 'Cancelada' &
                    !(descricaoTipo %in% descricoes_inuteis)) %>%
    tidyr::unnest()
  
  agenda %>%
    dplyr::rowwise() %>%
    dplyr::do(fetch_pauta_camara(
      .$id, .$dataHoraInicio, .$dataHoraFim, .$sigla, .$nome) %>%
        tibble::as.tibble()) %>%
    unique()
  
}

#' @title Baixa a agenda do senado
#' @description Retorna uma lista contendo 3 dataframes: agenda, materias e oradores,
#' todos os dfs possuem a coluna codigo_sessao
#' @param initial_date data inicial no formato yyyy-mm-dd
#' @return list
#' @examples
#' fetch_agenda_senado('2018-07-03')
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
fetch_agenda_senado <- function(initial_date) {
  url <- paste0("http://legis.senado.leg.br/dadosabertos/plenario/agenda/mes/", gsub('-','', initial_date))
  json_proposicao <- fetch_json_try(url)
  if (is.null(json_proposicao$AgendaPlenario)) {
    return(list(agenda = tibble::as.tibble(), materias = tibble::as.tibble(), oradores = tibble::as.tibble()))
  }
  
  agenda <-
    json_proposicao$AgendaPlenario$Sessoes$Sessao %>%
    rename_table_to_underscore() %>%
    tibble::as.tibble()
  
  descricoes_inuteis <- c('SESSÃO SOLENE', 'SESSÃO NÃO DELIBERATIVA', 'NÃO HAVERÁ SESSÃO', 'SESSÃO ESPECIAL')
  
  agenda <-
    agenda %>%
    dplyr::filter(!(tipo_sessao %in% descricoes_inuteis))
  
  materia <- tibble::tibble()
  if('materias_materia' %in% names(agenda)) {
    materia <- purrr::map_df(agenda$materias_materia, dplyr::bind_rows, .id = "codigo_sessao")
    
    materia_not_null <-
      agenda %>%
      dplyr::filter(materias_materia != "NULL")
    
    num_de_materias <-
      materia %>%
      dplyr::group_by(codigo_sessao) %>%
      dplyr::summarise(id = 0)
    
    num_de_materias$id <- materia_not_null$codigo_sessao
    
    materia <-
      merge(materia, num_de_materias) %>%
      dplyr::select(-codigo_sessao) %>%
      dplyr::rename("codigo_sessao" = id) %>%
      rename_table_to_underscore()
  }
  
  oradores <- tibble::tibble()
  if('oradores_tipo_orador_orador_sessao_orador' %in% names(agenda)) {
    oradores <- purrr::map_df(agenda$oradores_tipo_orador_orador_sessao_orador, dplyr::bind_rows, .id = "codigo_sessao")
    
    oradores_not_null <-
      agenda %>%
      dplyr::filter(oradores_tipo_orador_orador_sessao_orador != "NULL")
    
    num_de_oradores <-
      oradores %>%
      dplyr::group_by(codigo_sessao) %>%
      dplyr::summarise(id = 0)
    
    num_de_oradores$id <- oradores_not_null$codigo_sessao
    
    oradores <-
      merge(oradores, num_de_oradores) %>%
      dplyr::select(-codigo_sessao) %>%
      dplyr::rename("codigo_sessao" = id) %>%
      rename_table_to_underscore()
  }
  
  agenda <- list(agenda = agenda, materias = materia, oradores = oradores)
}

#' @title Retorna a agenda de uma comissão no Senado
#' @description Função auxiliar que retorna um dataframe contendo a agenda de
#' uma comissão do Senado
#' @param url Url dos dados abertos do senado para uma comissão
#' @return Dataframe
#' @examples
#' auxiliar_agenda_senado_comissoes('http://legis.senado.leg.br/dadosabertos/agenda/20160515/20160525/detalhe?colegiado=CDH')
auxiliar_agenda_senado_comissoes <- function(url) {
  tipos_inuteis <- c('Outros eventos', 'Reunião')
  json_proposicao <- fetch_json_try(url)
  agenda2 <-
    json_proposicao$Reunioes$Reuniao %>%
    tibble::as.tibble() %>%
    rename_table_to_underscore() %>%
    dplyr::filter(situacao != 'Cancelada') %>%
    dplyr::filter(!(tipo %in% tipos_inuteis))
  if ("partes_parte_itens_item" %in% names(agenda2)) {
    agenda2 <-
      agenda2 %>%
      dplyr::filter(partes_parte_itens_item != "NULL") %>%
      dplyr::mutate(index = as.character(dplyr::row_number())) %>%
      dplyr::select(-codigo)
    
    partes_parte <- purrr::map_df(agenda2$partes_parte_itens_item, dplyr::bind_rows, .id = "index")
    dplyr::left_join(partes_parte, agenda2, by = "index")
  }else {
    tibble::tibble()
  }
}

#' @title Retorna o dataFrame com as audiências públicas do Senado
#' @description Retorna um dataframe contendo as audiências públicas do Senado
#' @param initial_date data inicial no formato yyyy-mm-dd
#' @param end_date data final no formato yyyy-mm-dd
#' @return Dataframe
#' @examples
#' get_audiencias_publicas('2016-05-15', '2016-05-25')
get_audiencias_publicas <- function(initial_date, end_date) {
  
  pega_audiencias_publicas_do_data_frame <- function(l){
    if(length(l$Tipo) == 1 ) {
      if (l$Tipo == "Audiência Pública Interativa") {
        paste(l$Eventos$Evento$MateriasRelacionadas$Materia$Codigo, collapse = " ,")
      }else {
        ""
      }
    }else {
      if ("Audiência Pública Interativa" %in% l$Tipo) {
        paste(l$Eventos$Evento$MateriasRelacionadas, collapse = " ,")
      }else {
        ""
      }
    }
  }
  
  agenda_senado <- get_data_frame_agenda_senado(initial_date, end_date) %>% 
    dplyr::mutate(id_proposicao = purrr::map_chr(partes_parte, ~ pega_audiencias_publicas_do_data_frame(.)))
  
  if ("comissoes_comissao_sigla" %in% names(agenda_senado)) {
    agenda_senado %>%
      dplyr::select(data, hora, realizada, sigla = comissoes_comissao_sigla, id_proposicao)
  }else {
    agenda_senado %>% 
      mutate(sigla = purrr::map_chr(comissoes_comissao, ~ paste(.$Sigla, collapse = " ,"))) %>%
      dplyr::select(data, hora, realizada, sigla, id_proposicao)
  }
}

#' @title Retorna o dataFrame da agenda do Senado
#' @description Retorna um dataframe contendo a agenda do senado
#' @param initial_date data inicial no formato yyyy-mm-dd
#' @param end_date data final no formato yyyy-mm-dd
#' @return Dataframe
#' @examples
#' get_data_frame_agenda_senado('2016-05-15', '2016-05-25')
get_data_frame_agenda_senado <- function(initial_date, end_date) {
  url <-
    paste0("http://legis.senado.leg.br/dadosabertos/agenda/", gsub('-','', initial_date), "/", gsub('-','', end_date), "/detalhe")
  json_proposicao <- fetch_json_try(url)
  
  json_proposicao$Reunioes$Reuniao %>%
    tibble::as.tibble() %>%
    rename_table_to_underscore() %>%
    dplyr::filter(situacao != 'Cancelada')
}


#' @title Retorna a agenda das comissões no Senado
#' @description Retorna um dataframe contendo a agenda do senado normalizada
#' @param initial_date data inicial no formato yyyy-mm-dd
#' @param end_date data final no formato yyyy-mm-dd
#' @return Dataframe
#' @examples
#' fetch_agenda_senado_comissoes('2016-05-15', '2016-05-25')
fetch_agenda_senado_comissoes <- function(initial_date, end_date) {
  tipos_inuteis <- c('Outros eventos', 'Reunião', 'Reunião de Subcomissão')
  
  agenda <-
    get_data_frame_agenda_senado(initial_date, end_date) %>%
    dplyr::filter(!(tipo %in% tipos_inuteis)) %>%
    unique()
  
  if (nrow(agenda) != 0) {
    if ("partes_parte" %in% names(agenda)) {
      agenda <-
        agenda %>%
        dplyr::mutate(id_proposicao = purrr::map(partes_parte, ~ pega_id_proposicao(.))) %>%
        dplyr::mutate(nome = purrr::map(partes_parte, ~ pega_nome(.))) %>%
        dplyr::filter(id_proposicao != "") 
      
      if (nrow(agenda) != 0) {
        agenda <-
          agenda %>% 
          dplyr::rowwise() %>%
          dplyr::mutate(local = strsplit(titulo_da_reuniao, ",")[[1]][[1]]) %>%
          dplyr::select(c(data, nome, id_proposicao, local)) %>%
          dplyr::mutate(id_proposicao = strsplit(as.character(id_proposicao), ",")) %>%
          dplyr::mutate(nome = strsplit(as.character(nome), ",")) %>%
          tidyr::unnest() %>%
          dplyr::select(c(data, nome, id_proposicao, local))
      }else {
        return(tibble::frame_data(~ data, ~ sigla, ~ id_proposicao, ~ local))
      }
      
    }else {
      agenda <-
        agenda %>%
        dplyr::mutate(id_proposicao = purrr::map(partes_parte_itens_item, ~ .$Codigo)) %>%
        dplyr::mutate(nome = purrr::map(partes_parte_itens_item, ~ .$Nome)) %>%
        dplyr::filter(partes_parte_tipo == "Deliberativa") 
      
      if (nrow(agenda) != 0) {
        agenda <-
          agenda %>%
          dplyr::select(data, id_proposicao, nome, titulo_da_reuniao) %>%
          tidyr::unnest() %>%
          dplyr::rowwise() %>%
          dplyr::mutate(local = strsplit(titulo_da_reuniao, ",")[[1]][[1]]) %>%
          dplyr::select(c(data, nome, id_proposicao, local)) 
      }else {
        return(tibble::frame_data(~ data, ~ sigla, ~ id_proposicao, ~ local))
      }
    }
    
    new_names <- c("data", "sigla", "id_proposicao", "local")
    names(agenda) <- new_names
    
    agenda %>%
      dplyr::mutate(data = lubridate::dmy(data)) %>%
      dplyr::arrange(data)
    
  }else {
    tibble::frame_data(~ data, ~ sigla, ~ id_proposicao, ~ local)
  }
  
}

#' @title Extrai o nome da proposição
#' @description Recebe uma lista derivada da agenda do Senado e retorna o nome das proposições
#' que estarão em pauta
#' @param l lista que contém o id
#' @return char
pega_nome <- function(l){
  if(length(l$Tipo) == 1) {
    if (l$Tipo == "Deliberativa") {
      paste(l$Itens$Item$Nome, collapse = ",")
    }else {
      ""
    }
  }else {
    if ("Deliberativa" %in% l$Tipo) {
      # df <- l %>% tibble::as.tibble() %>% dplyr::filter(Tipo == "Deliberativa")
      # l <- df$Itens.Item[[1]]
      if(!is.null(l$Itens.Item)) {
        paste(l$Nome, collapse = ",")
      }else {
        ""
      }
    }else {
      ""
    }
  }
}

#' @title Extrai o id da proposição
#' @description Recebe uma lista derivada da agenda do Senado e retorna o id das proposições
#' que estarão em pauta
#' @param l lista que contém o id
#' @return char
pega_id_proposicao <- function(l){
  if(length(l$Tipo) == 1 ) {
    if (l$Tipo == "Deliberativa") {
      paste(l$Itens$Item$Codigo, collapse = ",")
    }else {
      ""
    }
  }else {
    if ("Deliberativa" %in% l$Tipo) {
      if(!is.null(l$Itens.Item)) {
        paste(l$Itens.Item$Codigo, collapse = ",")
      }else {
        ""
      }
    }else {
      ""
    }
  }
}

#' @title Normaliza as agendas da câmara ou do senado
#' @description Retorna um dataframe contendo a agenda da camara ou do senado normalizada
#' @param agenda dataframe com agenda
#' @param house camara ou senado
#' @return Dataframe
#' @examples
#' normalize_agendas(fetch_agenda_camara('2018-09-03', '2018-09-07'), 'camara')
normalize_agendas <- function(agenda, house) {
  if (tolower(house) == 'senado') {
    if (nrow(agenda$materias) == 0) {return(agenda$materias)}
    materias <- agenda$materias
    agenda <- agenda$agenda
    agenda <-
      merge(agenda, materias) %>%
      dplyr::mutate(sigla = paste0(sigla_materia, " ", numero_materia, "/", ano_materia))
    
    if (!("local_sessao" %in% names(agenda))) {
      agenda <-
        agenda %>%
        dplyr::mutate(local_sessao = NA)
    }
    
    agenda <-
      agenda %>%
      dplyr::select(c(data, sigla, codigo_materia, local_sessao))
    
  }else {
    if (nrow(agenda) == 0) {return(tibble::frame_data(~ data, ~ sigla, ~ id_proposicao, ~ local))}
    agenda <-
      agenda %>%
      dplyr::mutate(sigla = paste0(proposicao_.siglaTipo, " ", proposicao_.numero, "/", proposicao_.ano)) %>%
      dplyr::select(c(hora_inicio, sigla, proposicao_.id, nome_orgao))
  }
  
  new_names <- c("data", "sigla", "id_proposicao", "local")
  names(agenda) <- new_names
  
  agenda %>% dplyr::arrange(data)
}

#' @title Baixa a agenda da câmara ou do senado
#' @description Retorna um dataframe contendo a agenda da camara ou do senado
#' @param initial_date data inicial no formato yyyy-mm-dd
#' @param end_date data final no formato yyyy-mm-dd
#' @param house camara ou senado
#' @param orgao plenario ou comissoes
#' @return Dataframe
#' @examples
#' fetch_agenda('2018-07-03', '2018-07-10', 'camara', 'comissoes')
#' @export
fetch_agenda <- function(initial_date, end_date, house, orgao) {
  try(if (as.Date(end_date) < as.Date(initial_date))
    stop("A data inicial é depois da final!"))
  
  if (tolower(orgao) == "plenario") {
    if (house == "camara") {
      normalize_agendas(
        fetch_agenda_camara(
          initial_date = initial_date, end_date = end_date), house)
    } else {
      normalize_agendas(fetch_agenda_senado(initial_date), house)
    }
  }else {
    if(house == 'camara') {
      initial_date <- strsplit(initial_date, '-')
      end_date <- strsplit(end_date, '-')
      fetch_agenda_comissoes_camara(
        paste0(initial_date[[1]][[3]],'/', initial_date[[1]][[2]], '/', initial_date[[1]][[1]]),
        paste0(end_date[[1]][[3]],'/', end_date[[1]][[2]], '/', end_date[[1]][[1]]))
    }else {
      fetch_agenda_senado_comissoes(initial_date, end_date)
    }
  }
}

#' @title Baixa a agenda da camara e do senado
#' @description Retorna um dataframe contendo a agenda geral da camara e do
#' senado
#' @param initial_date data inicial no formato yyyy-mm-dd
#' @param end_date data final no formato yyyy-mm-dd
#' @return Dataframe
#' @examples
#' fetch_agenda_geral('2018-07-03', '2018-07-10')
#' @export
fetch_agenda_geral <- function(initial_date, end_date) {
  try(if(as.Date(end_date) < as.Date(initial_date)) stop("A data inicial é depois da final!"))
  print(initial_date)
  print(end_date)
  
  agenda_plenario_camara <- normalize_agendas(fetch_agenda_camara(initial_date = initial_date, end_date = end_date), "camara") %>%
    dplyr::mutate(casa = "camara")
  
  if (nrow(agenda_plenario_camara) != 0) {
    agenda_plenario_camara <- 
      agenda_plenario_camara %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = stringr::str_split(data,'T')[[1]][1]) %>%
      dplyr::ungroup()
  }
  agenda_plenario_senado <- 
    normalize_agendas(fetch_agenda_senado(initial_date), "senado") %>%
    dplyr::mutate(casa = "senado")
  agenda_comissoes_senado <- fetch_agenda_senado_comissoes(initial_date, end_date) %>%
    dplyr::mutate(data = as.character(data)) %>%
    dplyr::mutate(casa = "senado")
  
  initial_date <- strsplit(as.character(initial_date), '-')
  end_date <- strsplit(as.character(end_date), '-')
  agenda_comissoes_camara <- 
    fetch_agenda_comissoes_camara(
      paste0(initial_date[[1]][[3]],'/', initial_date[[1]][[2]], '/', initial_date[[1]][[1]]), 
      paste0(end_date[[1]][[3]],'/', end_date[[1]][[2]], '/', end_date[[1]][[1]])) %>%
    dplyr::mutate(data = as.character(data)) %>%
    dplyr::mutate(casa = "camara")
  
  rbind(
    agenda_plenario_camara,
    agenda_plenario_senado,
    agenda_comissoes_senado,
    agenda_comissoes_camara
  ) %>%
    dplyr::arrange(data) %>% 
    dplyr::mutate_all(as.character) %>%
    dplyr::rename(id_ext = id_proposicao)
}

#' @title Baixa dados de requerimentos relacionados
#' @description Retorna um dataframe contendo dados sobre os requerimentos relacionados a uma proposição
#' @param id ID de uma proposição
#' @param mark_deferimento valor default true
#' @return Dataframe
#' @export
fetch_related_requerimentos <- function(id, mark_deferimento = TRUE) {
  regexes <-
    tibble::frame_data(
      ~ deferimento,
      ~ regex,
      'indeferido',
      '^Indefiro',
      'deferido',
      '^(Defiro)|(Aprovado)'
    )
  
  related <-
    rcongresso::fetch_relacionadas(id)$uri %>%
    strsplit('/') %>%
    vapply(last, '') %>%
    unique %>%
    rcongresso::fetch_proposicao()
  
  requerimentos <-
    related %>%
    dplyr::filter(stringr::str_detect(.$siglaTipo, '^REQ'))
  
  if (!mark_deferimento)
    return(requerimentos)
  
  tramitacoes <- fetch_tramitacao(requerimentos$id, 'camara', TRUE)
  
  related <-
    tramitacoes %>%
    # mark tramitacoes rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by = c(texto_tramitacao = 'regex')) %>%
    dplyr::group_by(prop_id) %>%
    # fill down marks
    tidyr::fill(deferimento) %>%
    # get last mark on each tramitacao
    dplyr::do(tail(., n = 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(prop_id, deferimento) %>%
    # and mark proposicoes based on last tramitacao mark
    dplyr::left_join(related, by = c('prop_id' = 'id'))
}

#' @title Baixa dados da agenda de um orgão da Camara
#' @description Retorna um dataframe contendo dados sobre a agenda de um orgão da camara
#' @param orgao_id ID do orgão
#' @param initial_date data inicial no formato dd/mm/yyyy
#' @param end_date data final no formato dd/mm/yyyy
#' @return Dataframe
#' @importFrom RCurl getURL
fetch_agendas_comissoes_camara_auxiliar <- function(orgao_id, initial_date, end_date){
  
  url <-
    RCurl::getURL(paste0(
      'http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterPauta?IDOrgao=',
      orgao_id, '&datIni=', initial_date, '&datFim=', end_date))
  
  eventos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    eventos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON()
  
  if(purrr::is_list(df)){
    df <-
      df %>%
      purrr::list_modify(".attrs" = NULL) %>%
      tibble::as.tibble() %>%
      t() %>%
      as.data.frame()
    
    names(df) <- c("comissao","cod_reuniao", "num_reuniao", "data", "hora", "local",
                   "estado", "tipo", "titulo_reuniao", "objeto", "proposicoes")
    
    proposicoes <- df$proposicoes
    df <-
      df %>%
      dplyr::select(-c(num_reuniao, objeto, proposicoes)) %>%
      lapply(unlist) %>%
      as.data.frame() %>%
      tibble::add_column(proposicoes)
    
    df <-
      df %>%
      as.data.frame() %>%
      dplyr::filter(trimws(estado) != 'Cancelada') %>%
      tidyr::unnest()
    
    if(nrow(df) != 0) {
      df <-
        df %>%
        dplyr::mutate(sigla = purrr::map(proposicoes, ~ .x[['sigla']]),
                      id_proposicao = purrr::map(proposicoes, ~ .x[['idProposicao']]))
    }
    
  }else{
    
    df <- tibble::frame_data(~ comissao, ~ cod_reuniao, ~ num_reuniao, ~ data, ~ hora, ~ local,
                             ~ estado, ~ tipo, ~ titulo_reuniao, ~ objeto, ~ proposicoes)
  }
  
  return(df)
}

#' @title Baixa dados da agenda de todos os orgãos da Camara
#' @description Retorna um dataframe contendo dados sobre a agenda da camara
#' @param initial_date data inicial no formato dd/mm/yyyy
#' @param end_date data final no formato dd/mm/yyyy
#' @return Dataframe
#' @examples
#' fetch_agenda_comissoes_camara('12/05/2018', '26/05/2018')
fetch_agenda_comissoes_camara <- function(initial_date, end_date) {
  orgaos <-
    fetch_orgaos_camara()
  
  agenda <- purrr::map_df(orgaos$orgao_id, fetch_agendas_comissoes_camara_auxiliar, initial_date, end_date)
  if (nrow(agenda) == 0) {
    tibble::frame_data(~ data, ~ sigla, ~ id_proposicao, ~ local)
  }else {
    agenda %>%
      dplyr::select(data, sigla, id_proposicao, local = comissao) %>%
      dplyr::mutate(data = as.Date(data, "%d/%m/%Y")) %>%
      dplyr::arrange(data)
  }
}

#' @title Baixa os órgãos na câmara
#' @description Retorna um dataframe contendo os órgãos da câmara
#' @return Dataframe contendo os órgãos da Câmara
#' @importFrom RCurl getURL
fetch_orgaos_camara <- function(){
  url <- RCurl::getURL('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterOrgaos')
  
  orgaos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    orgaos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame()
  
  names(df) <- c("orgao_id", "tipo_orgao_id", "sigla", "descricao")
  
  return(df)
}


#' @title Baixa dados da agenda por semana
#' @description Retorna um dataframe contendo dados sobre a agenda 
#' @param initial_date data inicial no formato dd/mm/yyyy
#' @param end_date data final no formato dd/mm/yyyy
#' @return Dataframe
#' @examples
#' junta_agendas('2018-11-05', '2018-11-12')
#' @export
junta_agendas <- function(initial_date, end_date) {
  semanas <- 
    seq(lubridate::floor_date(as.Date(initial_date), unit="week") + 1, as.Date(end_date), by = "week") %>%
    tibble::as.tibble() %>%
    dplyr::mutate(fim_semana = as.Date(cut(value, "week")) + 4)
  
  materia <- purrr::map2_df(semanas$value, semanas$fim_semana, ~ fetch_agenda_geral(.x, .y)) 
}