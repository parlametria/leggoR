url_base <- "http://legis.senado.leg.br/dadosabertos/materia/"

#' @title Busca votações de uma proposição no Senado
#' @description Retorna dataframe com os dados das votações de uma proposição no Senado.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as votações de uma proposição no Senado
#' @examples
#' fetch_votacoes(91341)
#' @export
fetch_votacoes <- function(proposicao_id){
  url_base_votacoes <- paste0(url_base, "votacoes/")
  
  url <- paste0(url_base_votacoes, proposicao_id)
  json_votacoes <- jsonlite::fromJSON(url, flatten = T)
  votacoes_data <-
    json_votacoes %>%
    magrittr::extract2("VotacaoMateria") %>%
    magrittr::extract2("Materia")
  votacoes_ids <-
    votacoes_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as.tibble()
  votacoes_df <-
    votacoes_data %>%
    magrittr::extract2("Votacoes") %>%
    purrr::map_df(~ .) %>%
    tidyr::unnest()
  
  votacoes_df <-
    votacoes_df %>%
    tibble::add_column(!!! votacoes_ids)
  
  votacoes_df <- votacoes_df[, !sapply(votacoes_df, is.list)]
  rename_votacoes_df(votacoes_df)
}

#' @title Busca a movimentação da proposição
#' @description Retorna dataframe com os dados da movimentação da proposição, incluindo tramitação, prazos, despachos e situação
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre a movimentação de uma proposição no Senado
#' @examples
#' fetch_tramitacao(91341)
#' @export
fetch_tramitacao <- function(proposicao_id){
  
  url <- paste0(url_base, "movimentacoes/", proposicao_id)
  json_tramitacao <- jsonlite::fromJSON(url, flatten = T)
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
    tibble::add_column(!!! tramitacao_ids)
  
  proposicao_tramitacoes_df <- proposicao_tramitacoes_df[, !sapply(proposicao_tramitacoes_df, is.list)]
  
  rename_tramitacao_df(proposicao_tramitacoes_df)
}

#' @title Deferimento de requerimentos.
#' @description Verifica deferimento ou não para uma lista de IDs de requerimentos.
#' @param proposicao_id ID de um ou vários requerimentos
#' @return Dataframe com IDs dos requerimentos e informação sobre deferimento.
#' @examples
#' fetch_deferimento(c("102343", "109173", "115853"))
#' fetch_proposicao("81668")$proposicoes_relacionadas %>% fetch_deferimento
#' @export
fetch_deferimento <- function(proposicao_id) {
  
  regexes <-
    tibble::frame_data(~ deferimento, ~ regex,
                       "indeferido", '^Indefiro',
                       "deferido", '^(Defiro)|(Aprovado)')
  
  fetch_one_deferimento <- function(proposicao_id) {
    json <-
      paste0(url_base, "movimentacoes/", proposicao_id) %>%
      jsonlite::fromJSON()
    
    resultados <- json$MovimentacaoMateria$Materia$OrdensDoDia$OrdemDoDia$DescricaoResultado
    # handle NULL
    if (is.null(resultados)) resultados <- c('')
    
    resultados %>%
      tibble::as.tibble() %>%
      dplyr::mutate(proposicao_id=proposicao_id) %>%
      fuzzyjoin::regex_left_join(regexes, by=c(value="regex")) %>%
      tidyr::fill(deferimento) %>%
      tail(., n=1) %>%
      dplyr::select(proposicao_id, deferimento)
  }
  
  proposicao_id %>%
    unlist %>%
    unique %>%
    lapply(fetch_one_deferimento) %>%
    plyr::rbind.fill()
}

#' @title Recupera o histórico de relatorias de uma proposição no Senado
#' @description Retorna dataframe com o histórico de relatorias detalhado de uma proposição no Senado, incluindo a data
#' de designação e destituição, o relator e seu partido e a comissão.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações detalhadas do histórico de relatorias de uma proposição no Senado
#' @examples
#' fetch_relatorias(91341)
#' @export
fetch_relatorias <- function(proposicao_id) {
  
  url_relatorias <- paste0(url_base,"relatorias/")
  
  url <- paste0(url_relatorias, proposicao_id)
  json_relatorias <- jsonlite::fromJSON(url, flatten = T)
  
  #extract relatores objects
  relatorias_data <-
    json_relatorias %>%
    magrittr::extract2("RelatoriaMateria") %>%
    magrittr::extract2("Materia") %>%
    magrittr::extract2("HistoricoRelatoria")
  
  relatorias_df <-
    relatorias_data %>%
    magrittr::extract2("Relator") %>%
    as.data.frame() %>%
    purrr::map_df(~ .) %>%
    tidyr::unnest()
  
  #select columns
  relatorias_df <-
    relatorias_df %>%
    tibble::add_column()
  
  relatorias_df <- relatorias_df[, !sapply(relatorias_df, is.list)]
  rename_relatorias_df(relatorias_df)
}

#' @title Recupera a relatoria atual no Senado
#' @description Retorna dataframe com a relatoria atual no senado, contendo a data de designação, o relator e seu partido e a comissão
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações da relatoria atual no Senado
#' @examples
#' fetch_current_relatoria(91341)
#' @export
fetch_current_relatoria <- function(proposicao_id) {
  
  url_relatorias <- paste0(url_base, "relatorias/")
  
  url <- paste0(url_relatorias, proposicao_id)
  json_relatorias <- jsonlite::fromJSON(url, flatten = T)
  
  #extract relatores objects
  relatorias_data <-
    json_relatorias %>%
    magrittr::extract2("RelatoriaMateria") %>%
    magrittr::extract2("Materia")
  
  current_relatoria_df <-
    relatorias_data %>%
    magrittr::extract2("HistoricoRelatoria") %>%
    magrittr::extract2("Relator") %>%
    as.data.frame() %>%
    purrr::map_df(~ .) %>%
    tidyr::unnest()
  
  #fixing bug when api repeats relatorias
  current_relatoria_df <- current_relatoria_df[1,]
  
  #verify if relator atual exists
  if(ncol(current_relatoria_df) == 0){
    return(current_relatoria_df)
  }
  
  
  #select columns
  current_relatoria_df <-
    current_relatoria_df %>%
    tibble::add_column()
  
  current_relatoria_df <- current_relatoria_df[, !sapply(current_relatoria_df, is.list)]
  rename_table_to_underscore(current_relatoria_df)
}

#' @title Renomeia as colunas do dataframe do histórico de relatorias no Senado
#' @description Renomeia as colunas do dataframe do histórico de relatorias no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe do histórico de relatorias
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_relatorias_df()
#' @export
rename_relatorias_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace("identificacao_parlamentar_|identificacao_comissao_", "")
  
  names(df) <- new_names
  
  df
}

#' @title Renomeia as colunas do dataframe passado para o formato underscore
#' @description Renomeia as colunas do dataframe usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe do Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_table_to_underscore()
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
#' @examples
#' df %>% rename_votacoes_df()
#' @export
rename_votacoes_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace("sessao_plenaria_|tramitacao_identificacao_tramitacao_|identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

#' @title Renomeia as colunas do dataframe de movimentação no Senado
#' @description Renomeia as colunas do dataframe de movimentação no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe da votação no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_tramitacao_df()
#' @export
rename_tramitacao_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace("identificacao_tramitacao_|
                identificacao_tramitacao_origem_tramitacao_local_|
                identificacao_tramitacao_destino_tramitacao_local_|
                identificacao_tramitacao_situacao_", "")
  
  names(df) <- new_names
  
  df
}

#' @title Renomeia as colunas do dataframe dos detalhes da proposição no Senado
#' @description Renomeia as colunas do dataframe dos detalhes da proposição no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe dos detalhes da proposição no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_proposicao_df()
#' @export
rename_proposicao_df <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace("identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

#' @title Extrai o regime de apreciação do Senado
#' @description Verifica o regime de apreciação de um dataframe. Se apresentar as
#' palavras '(em|a) decisão terminativa' é retornado 'conclusivo' como resposta, caso contrário
#' é retornado 'plenário'.
#' @param df Dataframe da tramitação no Senado.
#' @return String com a situação da pl.
#' @examples
#' extract_apreciacao_Senado(93418)
#' @export
extract_apreciacao_Senado <- function(proposicao_id) {
  url <- paste0(url_base, "movimentacoes/", proposicao_id)
  json_tramitacao <- jsonlite::fromJSON(url, flatten = T)
  tramitacao_data <-
    json_tramitacao %>%
    magrittr::extract2("MovimentacaoMateria") %>%
    magrittr::extract2("Materia") %>%
    magrittr::extract2("Despachos") %>%
    magrittr::extract2("Despacho")
  
  if(!is.null(tramitacao_data)){
    if(!is.list(tramitacao_data$ComissoesDespacho.ComissaoDespacho)){
      tramitacao_data <-
        tramitacao_data %>%
        magrittr::extract2("ComissoesDespacho") %>%
        magrittr::extract2("ComissaoDespacho") %>%
        tibble::as.tibble()
    } else {
      tramitacao_data <-
        tramitacao_data %>%
        tidyr::unnest(ComissoesDespacho.ComissaoDespacho)
    }
    tramitacao_data <-
      tramitacao_data %>%
      dplyr::filter(IndicadorDespachoTerminativo == "Sim")
    dplyr::if_else(nrow(tramitacao_data) != 0, "Conclusiva", "Plenário")
  } else {
    "Plenário"
  }
}

#' @title Retorna as sessões deliberativas de uma proposição no Senado
#' @description Retorna dataframe com os dados das sessões deliberativas de uma proposição no Senado.
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as sessões deliberativas de uma proposição no Senado
#' @examples
#' fetch_sessions(91341)
#' @export
fetch_sessions <- function(bill_id){
  url_base_sessions <- "http://legis.senado.leg.br/dadosabertos/materia/ordia/"
  url <- paste0(url_base_sessions, bill_id)
  
  json_sessions <- jsonlite::fromJSON(url, flatten = T)
  
  sessions_data <- json_sessions %>%
    magrittr::extract2("OrdiaMateria") %>%
    magrittr::extract2("Materia")
  
  ordem_do_dia_df <- sessions_data %>%
    magrittr::extract2("OrdensDoDia") %>%
    purrr::map_df(~ .) %>%
    tidyr::unnest() %>%
    rename_table_to_underscore()
  
  ordem_do_dia_df
}

#' @title Retorna as emendas de uma proposição no Senado
#' @description Retorna dataframe com os dados das emendas de uma proposição no Senado.
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as emendas de uma proposição no Senado.
#' @examples
#' fetch_emendas(91341)
#' @export
fetch_emendas <- function(bill_id){
  url_base_emendas <- "http://legis.senado.leg.br/dadosabertos/materia/emendas/"
  url <- paste0(url_base_emendas, bill_id)
  
  json_emendas <- jsonlite::fromJSON(url, flatten = T)
  
  emendas_data <- json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia")
  
  emendas_df <- emendas_data %>%
    magrittr::extract2("Emendas") %>%
    purrr::map_df(~ .) 
  
  if(nrow(emendas_df) == 0){
    emendas_df <-  frame_data(~ codigo, ~numero, ~ local, ~ autor, ~partido)
    
  } else if(nrow(emendas_df) == 1) {
    emendas_df <- emendas_df %>%
      rename_table_to_underscore() 
    
    autoria <- as.data.frame(emendas_df$autoria_emenda) %>%
      rename_table_to_underscore() %>% 
      dplyr::mutate(partido = paste0(identificacao_parlamentar_sigla_partido_parlamentar,
                                     "/",
                                     identificacao_parlamentar_uf_parlamentar))
    emendas_df <- emendas_df %>%
      rename_table_to_underscore() %>%
      plyr::rename(c("codigo_emenda" = "codigo", "numero_emenda" = "numero", "colegiado_apresentacao" = "local")) %>%
      dplyr::mutate(autor = autoria$nome_autor,
                    partido = autoria$partido) %>%
      select(codigo, numero, local, autor, partido)
    
  }else{
    emendas_df <- emendas_df %>%
      tidyr::unnest() %>%
      rename_table_to_underscore() %>%
      dplyr::select(codigo_emenda, numero_emenda, colegiado_apresentacao, autoria_emenda_autor_nome_autor,
                    autoria_emenda_autor_identificacao_parlamentar_sigla_partido_parlamentar,
                    autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar) %>%
      dplyr::mutate(partido = paste0(autoria_emenda_autor_identificacao_parlamentar_sigla_partido_parlamentar,
                                     "/",
                                     autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar)) %>%
      select(-autoria_emenda_autor_identificacao_parlamentar_sigla_partido_parlamentar, 
             -autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar) %>%
      plyr::rename(c("codigo_emenda" = "codigo", "numero_emenda" = "numero", "colegiado_apresentacao" = "local", 
                     "autoria_emenda_autor_nome_autor" = "autor"))
  }
  
  emendas_df
  
}
#' @title Importa as informações de uma proposição da internet.
#' @description Recebido um id a função roda os scripts para 
#' importar os dados daquela proposição.
#' @param bill_id Identificador da proposição que pode ser recuperado no site da casa legislativa.
#' @examples
#' import_proposicao(91341)
#' @export
import_proposicao <- function(bill_id){
  #Voting data
  voting <- fetch_votacoes(bill_id)
  voting %>% 
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-votacoes-senado.csv"))
  
  #Passage Data
  passage <- fetch_tramitacao(bill_id)
  passage %>%
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-tramitacao-senado.csv"))
  
  #Votacao Data
  bill_data <- fetch_proposicao(bill_id, 'senado')
  bill_data %>%
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-proposicao-senado.csv"))
  
  #Relatorias Data
  relatorias <- fetch_relatorias(bill_id)
  relatorias %>%
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-relatorias-senado.csv"))
  
  #Relatorias data
  relatorias <- fetch_relatorias(bill_id)
  relatorias %>%
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-relatorias-senado.csv"))
  
  #Current Relatoria data
  current_relatoria <- fetch_current_relatoria(bill_id)
  current_relatoria %>%
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-current-relatoria-senado.csv"))
  
  #Last Relatoria
  last_relatoria <- fetch_last_relatoria(bill_id)
  last_relatoria %>%
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-last-relatoria-senado.csv"))
  
  #Ordem do Dia data
  sessions_data <- fetch_sessions(bill_id)
  sessions_data %>% 
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-sessions-senado.csv"))
  
  #Emendas data
  emendas_data <- fetch_emendas(bill_id)
  emendas_data %>%
    readr::write_csv(paste0(here::here("data/Senado/"), bill_id, "-emendas-senado.csv"))
}

###################################################################

#' @title Recupera o estado e partido de um autor
#' @description Retorna o estado e partido
#' @param uri uri que contém dados sobre o autor
#' @return Estado e partido
#' @examples
#' partido_estado <- extract_partido_estado_autor(autores$autor.uri %>% tail(1))
#' @export
extract_partido_estado_autor <- function(uri) {
  if(!is.na(uri)) {
    json_autor <- jsonlite::fromJSON(uri, flatten = T)
    
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
  }else {
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
  api_v1_proposicao_url = 'http://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp='
  xml2::read_xml(paste0(api_v1_proposicao_url, prop_id)) %>%
    xml2::xml_find_all('//apensadas/proposicao/codProposicao') %>%
    xml2::xml_text() %>%
    tibble::tibble(apensadas=.)
}

#' @title Recupera os eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' fetch_events(2121442)
#' @export
fetch_events <- function(prop_id) {
  events_base_url <- 'http://www.camara.gov.br/proposicoesWeb/sessoes_e_reunioes?idProposicao='
  bill_events_url <- paste0(events_base_url,prop_id)
  events <- bill_events_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath='//*[@id="content"]/table') %>%
    rvest::html_table()
  events_df <- events[[1]]
  names(events_df) <- c('timestamp','origem','descricao','links')
  events_df %>%
    dplyr::select(-links) %>%
    dplyr::mutate(timestamp = lubridate::dmy_hm(timestamp))
}

extract_tramitacao <- function(prop_id) {
  rcongresso::fetch_tramitacao(prop_id) %>% rename_df_columns
}

###################################################################

#' @title Renomeia um vetor com o padrão de underscores e minúsculas
#' @description Renomeia cada item do vetor com o padrão: separado por underscore e letras minúsculas
#' @param x Vetor de strings
#' @return Vetor contendo as strings renomeadas.
#' @examples
#' to_underscore(c("testName", "TESTNAME"))
#' @export
to_underscore <- function(x) {
  gsub('([A-Za-z])([A-Z])([a-z])', '\\1_\\2\\3', x) %>%
    gsub('.', '_', ., fixed = TRUE) %>%
    gsub('([a-z])([A-Z])', '\\1_\\2', .) %>%
    tolower()
}

#' @title Recupera os detalhes de uma proposição no Senado ou na Câmara
#' @description Retorna dataframe com os dados detalhados da proposição, incluindo número, ementa, tipo e data de apresentação.
#' @param id ID de uma proposição
#' @param casa casa de onde a proposição esta
#' @return Dataframe com as informações detalhadas de uma proposição
#' @examples
#' fetch_proposicao(91341, 'senado')
#' @export
fetch_proposicao <- function(id, casa) {
  if (tolower(casa) == 'camara') {
    fetch_proposicao_camara(id)
  } else {
    fetch_proposicao_senado(id)
  }
}

#' @title Recupera os detalhes de uma proposição no Senado
#' @description Retorna dataframe com os dados detalhados da proposição, incluindo número, ementa, tipo e data de apresentação.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações detalhadas de uma proposição no Senado
#' @examples
#' fetch_proposicao_senado(91341)
#' @export
fetch_proposicao_senado <- function(proposicao_id){
  url_base_proposicao <- "http://legis.senado.leg.br/dadosabertos/materia/"
  da_url <- paste0(url_base_proposicao, proposicao_id)
  
  page_url_senado <- "https://www25.senado.leg.br/web/atividade/materias/-/materia/"
  
  json_proposicao <- jsonlite::fromJSON(da_url, flatten = T)
  proposicao_data <- json_proposicao$DetalheMateria$Materia
  proposicao_ids <-
    proposicao_data$IdentificacaoMateria %>%
    tibble::as.tibble()
  proposicao_basic_data <-
    proposicao_data$DadosBasicosMateria %>%
    purrr::flatten() %>%
    tibble::as.tibble()
  proposicao_author <-
    proposicao_data$Autoria$Autor %>%
    tibble::as.tibble()
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
      !!! proposicao_ids, !!! proposicao_author, !!! proposicao_specific_assunto,
      !!! proposicao_general_assunto, !!! proposicao_source,
      page_url = paste0(page_url_senado, proposicao_id),
      proposicoes_relacionadas = paste(relacionadas, collapse=' '),
      proposicoes_apensadas = paste(anexadas, collapse=' '))
  
  proposicao_complete <- proposicao_complete[, !sapply(proposicao_complete, is.list)]
  
  rename_proposicao_df(proposicao_complete)
}


#' @title Baixa dados sobre uma proposição
#' @description Retorna um dataframe contendo dados sobre uma proposição
#' @param prop_id Um ou mais IDs de proposições
#' @return Dataframe
#' @examples
#' fetch_proposicao_camara(2056568)
#' @export
fetch_proposicao_camara <- function(prop_id) {
  base_url <- 'http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao='
  
  regex_regime <-
    tibble::frame_data(~ regime_tramitacao, ~ regex,
                       'Ordinária', 'Ordinária',
                       'Prioridade', 'Prioridade',
                       'Urgência', 'Urgência')
  
  regex_apreciacao <-
    tibble::frame_data(~ forma_apreciacao, ~ regex,
                       'Conclusiva', 'Sujeita à Apreciação Conclusiva pelas Comissões',
                       'Plenário', 'Sujeita à Apreciação do Plenário')
  
  rcongresso::fetch_proposicao(prop_id) %>%
    # Adiciona url das páginas das proposições
    dplyr::mutate(page_url=paste0(base_url, prop_id)) %>%
    # Adiciona html das páginas das proposições
    dplyr::rowwise() %>%
    dplyr::mutate(page_html=list(xml2::read_html(page_url))) %>%
    
    # Padroniza valor sobre regime de tramitação
    fuzzyjoin::regex_left_join(regex_regime, by=c(statusProposicao.regime="regex")) %>%
    dplyr::select(-'regex') %>%
    
    # Adiciona coluna sobre forma de apreciação
    dplyr::rowwise() %>%
    dplyr::mutate(temp=
                    rvest::html_node(page_html, '#informacoesDeTramitacao') %>%
                    rvest::html_text()
    ) %>%
    fuzzyjoin::regex_left_join(regex_apreciacao, by=c(temp="regex")) %>%
    dplyr::select(-c('temp', 'regex'))
}