source(here::here("code/congresso-lib.R"))

url_base_tramitacao <- "http://legis.senado.leg.br/dadosabertos/materia/movimentacoes/"

#' @title Busca votações de uma proposição no Senado
#' @description Retorna dataframe com os dados das votações de uma proposição no Senado.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as votações de uma proposição no Senado
#' @examples
#' fetch_votacoes(91341)
#' @export
fetch_votacoes <- function(proposicao_id){
    url_base_votacoes <- "http://legis.senado.leg.br/dadosabertos/materia/votacoes/"

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

    url <- paste0(url_base_tramitacao, proposicao_id)
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

#' @title Recupera os detalhes de uma proposição no Senado
#' @description Retorna dataframe com os dados detalhados da proposição, incluindo número, ementa, tipo e data de apresentação.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações detalhadas de uma proposição no Senado
#' @examples
#' fetch_proposicao(91341)
#' @export
fetch_proposicao <- function(proposicao_id){
  url_base_proposicao <- "http://legis.senado.leg.br/dadosabertos/materia/"

  url <- paste0(url_base_proposicao, proposicao_id)
  json_proposicao <- jsonlite::fromJSON(url, flatten = T)
  proposicao_data <-
    json_proposicao$DetalheMateria$Materia
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
      proposicoes_relacionadas = paste(relacionadas, collapse=' '),
      proposicoes_apensadas = paste(anexadas, collapse=' '))
  
  proposicao_complete <- proposicao_complete[, !sapply(proposicao_complete, is.list)]

  rename_proposicao_df(proposicao_complete)
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
    frame_data(~ deferimento, ~ regex,
               "indeferido", '^Indefiro',
               "deferido", '^(Defiro)|(Aprovado)')

  fetch_one_deferimento <- function(proposicao_id) {
    json <-
      paste0(url_base_tramitacao, proposicao_id) %>%
      jsonlite::fromJSON()

    resultados <- json$MovimentacaoMateria$Materia$OrdensDoDia$OrdemDoDia$DescricaoResultado
    # handle NULL
    if (is.null(resultados)) resultados <- c('')

    resultados %>%
      tibble::as.tibble() %>%
      mutate(proposicao_id=proposicao_id) %>%
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

  url_relatorias <- "http://legis.senado.leg.br/dadosabertos/materia/relatorias/"

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

  url_relatorias <- "http://legis.senado.leg.br/dadosabertos/materia/relatorias/"

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
  rename_relatoria(current_relatoria_df)
}

#' @title Recupera a última relatoria de uma proposição no Senado
#' @description Retorna dataframe com a última relatoria de uma proposição no Senado, incluindo a data
#' de designação e destituição, o relator e seu partido e a comissão.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações da última relatoria de uma proposição no Senado
#' @examples
#' fetch_relatorias(91341)
#' @export
fetch_last_relatoria <- function(proposicao_id) {
  relatoria <- fetch_relatorias(proposicao_id)
  relatoria <- relatoria[1,]

  relatoria

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

#' @title Renomeia as colunas do dataframe de relatoria atual no Senado
#' @description Renomeia as colunas do dataframe de relatoria atual no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe da relatoria atual no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_relatoria()
#' @export
rename_relatoria <- function(df) {
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

#' @title Recupera o numero e a ementa de uma proposição no Senado
#' @description Retona um dataframe de uma proposição no senado contendo o número, a ementa e o tipo da proposição
#' @param df ID da proposição no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' get_nome_ementa_Senado(91341)
#' @export
get_nome_ementa_Senado <- function(proposicao_id) {

  proposicao <- fetch_proposicao(proposicao_id)
  proposicao %>%
    dplyr::select(ementa_materia, sigla_subtipo_materia, numero_materia) %>%
    unique
}

#' @title Recupera os n últimos despachos no Senado
#' @description Retorna um dataframe das últimas n tramitações no Senado contendo a data, a descrição e o despacho
#' @param df Dataframe da tramitação no Senado
#' @param qtd  (opcional) Quantidade de eventos a serem recuperados
#' @return Dataframe com as últimas n tramitações no Senado.
#' @examples
#' tramitacao %>% tail_descricao_despacho_Senado()
#' tramitacao %>% tail_descricao_despacho_Senado(4)
#' @export
tail_descricao_despacho_Senado <- function(df, qtd=1) {

  df %>%
    dplyr::arrange(data_tramitacao) %>%
    tail(qtd) %>%
      dplyr::select(data_tramitacao, situacao_descricao_situacao, texto_tramitacao)
}

#' @title Cria coluna com as fases da tramitação no Senado
#' @description Cria uma nova coluna com as fases no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com a coluna "fase" adicionada.
#' @examples
#' tramitacao %>% extract_fase_Senado()
#' @export
extract_fase_Senado <- function(dataframe, phase_one, phase_two, phase_three, phase_four) {

  dataframe %>%
    dplyr::mutate(fase = dplyr::case_when( grepl(phase_one, texto_tramitacao) ~ 'iniciativa',
                             detect_fase(situacao_codigo_situacao, phase_two) ~ 'relatoria',
                             detect_fase(situacao_codigo_situacao, phase_three) ~ 'discussao_deliberacao',
                             detect_fase(situacao_codigo_situacao, phase_four) ~ 'virada_de_casa'))
}

#' @title Extrai os eventos importantes que aconteceram no Senado
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram no Senado
#' @param tramitacao_df Dataframe da tramitação no Senado
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
#' @examples
#' df %>% extract_evento_Senado(importants_events)
#' @export
extract_evento_Senado <- function(tramitacao_df, phases_df) {

  dplyr::left_join(tramitacao_df, phases_df, by = "situacao_codigo_situacao")
}

#' @title Recupera os n últimos eventos importantes que aconteceram no Senado
#' @description Retona dataframe contendo os n últimos eventos importantes que aconteceram no Senado
#' @param tramitacao_df Dataframe da tramitação no Senado
#' @param num Quantidade de eventos a serem recuperados
#' @return Dataframe contendo os n últimos eventos importantes que aconteceram no Senado
#' @examples
#' df %>% extract_n_last_eventos_Senado(4)
#' @export
extract_n_last_eventos_Senado <- function(df, num) {

  df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::arrange(data_tramitacao) %>%
    tail(n = num) %>%
    dplyr::select(data_tramitacao, evento)
}

#' @title Recupera as comissões do Senado
#' @description Retorna dataframe contendo o código da proposição e as comissões
#' @param tramitacao_df Dataframe da tramitação no Senado
#' @return Dataframe contendo o código da proposição e as comissões
#' @examples
#' df %>% extract_comissoes_Senado()
#' @export
extract_comissoes_Senado <- function(df) {

  prefix <- '(Comiss..s*)* '

  comissoes_permanentes_especiais <- '
    Especial
    de Assuntos Econômicos
    de Assuntos Sociais
    de Constituição, Justiça e Cidadania
    de Ciência, Tecnologia, Inovação, Comunicação e Informática
    de Direitos Humanos e Legislação Participativa
    Diretora
    de Desenvolvimento Regional e Turismo
    de Educação, Cultura e Esporte
    de Serviços de Infraestrutura
    de Meio Ambiente
    de Agricultura e Reforma Agrária
    de Relações Exteriores e Defesa Nacional
    Senado do Futuro
    de Transparência, Governança, Fiscalização e Controle e Defesa do Consumidor
    Mista de Controle das Atividades de Inteligência
    Mista de Consolidação da Legislação Federal
    Mista do Congresso Nacional de Assuntos Relacionados à Comunidade dos Países de Língua Portuguesa
    Permanente Mista de Combate à Violência contra a Mulher
    Mista Permanente sobre Mudanças Climáticas
    Mista de Planos, Orçamentos Públicos e Fiscalização
    Mista Representativa do Congresso Nacional no Fórum Interparlamentar das Américas
    ' %>%
    # Constrói expressão regular adicionando `prefix` ao começo de cada linha
    # e concatenando todas as linhas com `|`.
    strsplit('\n') %>%
    magrittr::extract2(1) %>%
    sapply(trimws) %>%
    magrittr::extract(. != '') %>%
    paste0(prefix, .) %>%
    paste(collapse='|') %>%
    regex(ignore_case=TRUE)

  # Faz com que os nomes comecem com 'Comissão'.
  fix_names <- function(name) {
    name %>%
      stringr::str_replace('Comissões', 'Comissão') %>%
      sapply(
        function(name) {
          if(!str_detect(name, 'Comissão')) paste0('Comissão', name)
          else name
        },
        USE.NAMES=FALSE)
  }

  df <-
    df %>%
    dplyr::mutate(
      comissoes =
        dplyr::case_when(
        stringr::str_detect(tolower(texto_tramitacao), 'às* comiss..s*') ~
          stringr::str_extract(texto_tramitacao, regex('comiss..s*.+', ignore_case=TRUE)))
    ) %>%
    dplyr::filter(!is.na(comissoes)) %>%
    dplyr::arrange(data_tramitacao) %>%
    dplyr::select(codigo_materia, comissoes, data_tramitacao) %>%
    rowwise() %>%
    mutate(
      comissoes = stringr::str_extract_all(comissoes, comissoes_permanentes_especiais)
    ) %>%
    unique() %>%
    mutate(comissoes = sapply(comissoes, fix_names))
  
  df[1,]
}
