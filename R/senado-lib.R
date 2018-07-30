source(here::here("R/congresso-lib.R"))
senado_codes <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
senado_constants <- senado_codes$constants

#' @title Busca votações de uma proposição no Senado
#' @description Retorna dataframe com os dados das votações de uma proposição no Senado.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as votações de uma proposição no Senado
#' @examples
#' fetch_votacoes(91341)
#' @export
fetch_votacoes <- function(proposicao_id){
   url_base_votacoes <- paste0(senado_codes$endpoints_api$url_base, "votacoes/")

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

    url <- paste0(senado_codes$endpoints_api$url_base, "movimentacoes/", proposicao_id)
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

  deferimento_regexes <- senado_codes$deferimento
  regexes <-
    tibble::frame_data(~ deferimento, ~ regex,
               "indeferido", deferimento_regexes$indeferido,
               "deferido", deferimento_regexes$deferido)

  fetch_one_deferimento <- function(proposicao_id) {
    json <-
      paste0(senado_codes$endpoints_api$url_base, "movimentacoes/", proposicao_id) %>%
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

  url_relatorias <- paste0(senado_codes$endpoints_api$url_base,"relatorias/")

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

  url_relatorias <- paste0(senado_codes$endpoints_api$url_base, "relatorias/")

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

  proposicao <- fetch_proposicao(proposicao_id, 'senado')
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

#' @title Cria coluna com a fase global da tramitação no Senado
#' @description Cria uma nova coluna com a fase global no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com a coluna "global" adicionada.
#' @examples
#' tramitacao %>% extract_fase_global()
#' @export
extract_fase_global <- function(data_tramitacao, bill_id) {
  fase_global_constants <- senado_codes$fase_global
  data_prop <- read_csv(paste0(here::here("data/Senado/"), bill_id,"-proposicao-senado.csv"))
  casa_origem <- if_else(data_prop$nome_casa_origem == "Senado Federal", fase_global_constants$origem_senado, fase_global_constants$revisao_camara)

  virada_de_casa <-
    data_tramitacao %>%
    filter(local == 'Mesa - Câmara') %>%
    arrange(data_tramitacao) %>%
    select(data_tramitacao)

  if(nrow(virada_de_casa) == 0) {
    data_tramitacao %>%
      mutate(global = paste0(casa_origem))
  }else {
    casa_atual <- if_else(casa_origem == " - Origem (Senado)", fase_global_constants$revisao_camara, fase_global_constants$origem_senado)
    data_tramitacao %>%
      mutate(global = if_else(data_tramitacao < virada_de_casa[1, ][[1]], casa_origem, casa_atual))
  }
}

#' @title Cria coluna com as fases da tramitação no Senado
#' @description Cria uma nova coluna com as fases no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com a coluna "fase" adicionada.
#' @examples
#' tramitacao %>% extract_fase_Senado()
#' @export
extract_fase_Senado <- function(dataframe, recebimento_phase, phase_one, phase_two, phase_three, encaminhamento_phase, phase_four) {

  fases <- senado_codes$fase_subfase_comissoes
  dataframe %>%
    dplyr::mutate(
      fase =
        dplyr::case_when(stringr::str_detect(tolower(texto_tramitacao), fase$regex) ~ fases$recebimento,
        detect_fase(situacao_codigo_situacao, phase_two) ~ fases$analise,
        detect_fase(situacao_codigo_situacao, phase_three) ~ fases$discussao,
        detect_fase(situacao_codigo_situacao, encaminhamento_phase) ~ fases$encaminhamento))
}

#' @title Recupera os locais do Senado
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada local
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe da tramitacao contendo mais uma coluna chamada local
#' @examples
#'  extract_locais(fetch_tramitacao(91341))
#' @export
extract_locais <- function(df) {
  
  comissao_json <- senado_codes$fase_comissao
  df <-
    df %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(
          situacao_descricao_situacao %in% constants$regex_plenario ~
            constants$plenario,
          (stringr::str_detect(tolower(texto_tramitacao), constants$regex_recebimento_comissoes) |
             situacao_descricao_situacao %in% constants$regex_comissoes_vector) ~
            origem_tramitacao_local_sigla_local,
          situacao_descricao_situacao == constants$regex_camara ~
            constants$mesa_camara)
    )

  if (is.na(df[1, ]$local)) {
    df[1, ]$local = constants$mesa_senado
  }

  df %>%
    tidyr::fill(local)
}

#' @title Cria coluna com a fase casa da tramitação no Senado
#' @description Cria uma nova coluna com a fase casa no Senado
#' @param df Dataframe da tramitação no Senado com as descrições formatadas
#' @return Dataframe com a coluna "casa" adicionada.
#' @examples
#' bill_passage %>% extract_fase_casa_Senado(phase_one)
#' @export
extract_fase_casa_Senado <- function(dataframe, fase_apresentacao) {

  dataframe <-
    dataframe %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    dplyr::mutate(
      casa =
        dplyr::case_when(
          grepl(fase_apresentacao, texto_tramitacao) ~ 'Apresentação',
          situacao_descricao_situacao %in% constants$regex_plenario ~
            constants$plenario,
          (stringr::str_detect(tolower(texto_tramitacao), constants$regex_recebimento_comissoes) |
             situacao_descricao_situacao %in% descricoes_comissoes) ~
            constants$comissoes)
    ) %>%
    tidyr::fill(casa)

  dataframe %>%
    mutate(casa = if_else(is.na(casa), constants$mesa_senado, casa))
}

#' @title Extrai os eventos importantes que aconteceram no Senado
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram no Senado
#' @param tramitacao_df Dataframe da tramitação no Senado
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
#' @examples
#' df <- fetch_tramitacao(91341)
#' extract_evento_Senado(df, importants_events)
#' @export
extract_evento_Senado <- function(tramitacao_df, phases_df) {
  dplyr::left_join(tramitacao_df, phases_df, by = "situacao_codigo_situacao")
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
  url <- paste0(senado_codes$endpoints_api$url_base, "movimentacoes/", proposicao_id)
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
    
    apreciacao <- senado_codes$apreciacao 
    tramitacao_data <-
      tramitacao_data %>%
        dplyr::filter(IndicadorDespachoTerminativo == "Sim")
    dplyr::if_else(nrow(tramitacao_data) != 0, apreciacao$conclusiva, apreciacao$plenario)
  } else {
    apreciacao$plenario
  }
}


#' @title Extrai o regime de tramitação do Senado
#' @description Verifica o regime de tramitação de um dataframe. Se apresentar as
#' palavras 'estando a matéria em regime de urgência' é retornado 'Urgência' como resposta, caso contrário
#' é retornado 'Ordinária'.
#' @param df Dataframe da tramitação no Senado.
#' @return String com a situação do regime de tramitação da pl.
#' @examples
#' extract_regime_Senado(93418)
#' @export
extract_regime_Senado <- function(proposicao_id) {
  df <- fetch_tramitacao(proposicao_id)
  regime <- senado_codes$regimes
  df <-
    df %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    dplyr::mutate(
      regime =
        dplyr::case_when(
          stringr::str_detect(tolower(texto_tramitacao), regime$regex) ~
            regime$urgencia)
    ) %>%
    tidyr::fill(regime)

  if(is.na(df[nrow(df), ]$regime)){
    regime$ordinaria
  } else{
    df[nrow(df), ]$regime
  }
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

#' @title Recupera todas as comissões do Senado
#' @description Retorna dataframe contendo o código da proposição, as comissões e a data
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe contendo o código da proposição, as comissões e a data
#' @examples
#' extract_comissoes_Senado(fetch_tramitacao(129808))
#' @export
extract_comissoes_Senado <- function(df) {
  
  codigos_comissoes <- senado_codes$comissoes

  siglas_comissoes <- '
    CAE
    CAS
    CCJ
    CCT
    CDH
    CDIR
    CDR
    CE
    CI
    CMA
    CRA
    CRE
    CSF
    CTFC
    CCAI
    CMCF
    CMCPLP
    CMCVM
    CMMC
    CMO
    FIPA
  '
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
    paste0(siglas_comissoes) %>%
    # Constrói expressão regular adicionando `prefix` ao começo de cada linha
    # e concatenando todas as linhas com `|`.
    strsplit('\n') %>%
    magrittr::extract2(1) %>%
    sapply(trimws) %>%
    magrittr::extract(. != '') %>%
    paste0(codigos_comissoes$prefixo, .) %>%
    paste(collapse='|') %>%
    stringr::regex(ignore_case=FALSE)

  # Faz com que os nomes comecem com 'Comissão'.
  fix_names <- function(name) {
    name %>%
      stringr::str_replace('Comissões', 'Comissão') %>%
      sapply(
        function(name) {
          if(!stringr::str_detect(name, 'Comissão') & !stringr::str_detect(siglas_comissoes, name)) paste0('Comissão', name)
          else name
        },
        USE.NAMES=FALSE)
  }

  detect <- function(texto_tramitacao, regex1, regex2=NULL) {
    if (is.null(regex2)) regex2 <- regex1
    stringr::str_detect(tolower(texto_tramitacao), regex1) ~
      stringr::str_extract(texto_tramitacao, stringr::regex(regex2, ignore_case=TRUE))
  }

  df %>%
    dplyr::mutate(
      comissoes =
        dplyr::case_when(
          detect(texto_tramitacao,
                 codigos_comissoes$regex_1),
          detect(texto_tramitacao,
                 codigos_comissoes$regex_2),
          detect(texto_tramitacao,
                 codigos_comissoes$regex_3))
    ) %>%
    dplyr::filter(!is.na(comissoes)) %>%
    dplyr::arrange(data_tramitacao) %>%
    dplyr::select(codigo_materia, comissoes, data_tramitacao) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      comissoes = stringr::str_extract_all(comissoes, comissoes_permanentes_especiais)
    ) %>%
    unique() %>%
    dplyr::mutate(comissoes = sapply(comissoes, fix_names)) %>%
    dplyr::rowwise() %>%
    dplyr::filter(length(comissoes) != 0)
}


#' @title Recupera as comissões que a proposição originalmente vai passar
#' @description Retorna dataframe contendo o código da proposição, as comissões e a data
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe contendo o código da proposição, as comissões e a data
#' @examples
#' extract_first_comissoes_Senado(fetch_tramitacao(129808))
#' @export
extract_first_comissoes_Senado <- function(df) {
  extract_comissoes_Senado(df)[1, ]

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
    rename_sessions()

  ordem_do_dia_df
}

#' @title Renomeia as colunas do dataframe de Ordem do Dia no Senado
#' @description Renomeia as colunas do dataframe de Ordem do Dia no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe de Ordem do Dia no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_sessions()
#' @export
rename_sessions <- function(df) {
  new_names = names(df) %>%
    to_underscore()

  names(df) <- new_names

  df
}
