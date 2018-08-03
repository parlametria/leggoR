library(rcongresso)
library(magrittr)
library(lubridate)
library(fuzzyjoin)
library(tidyverse)
source(here::here("R/senado-lib.R"))
source(here::here("R/camara-lib.R"))
source(here::here("Controller/fetcher.R"))
source(here::here("R/congresso-lib.R"))

process_proposicao <- function(id, casa){
  if("CAMARA" == toupper(casa)){
    process_proposicao_camara(id)
  } else if("SENADO" == toupper(casa)){
    process_proposicao_senado(id)
  }
}

#' @title Processa dados de uma proposição do senado.
#' @description Recebido um bill_id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/Senado.
#' @param bill_id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' process_proposicao_senado(91341)
#' @export
process_proposicao_senado <- function(bill_id){
  bill_passage <- read_csv(paste0(here::here("data/Senado/"), bill_id, "-tramitacao-senado.csv")) %>% arrange(data_tramitacao)

  phase_one <- c('^Este processo contém')
  recebimento_phase <- 'recebido na|nesta comissão'
  phase_two <- c(91)
  phase_three <- c(42, 14, 78, 90)
  encaminhamento_phase <- c(89, 158, 159, 160, 161, 162, 163)
  phase_four <- c(52)

  bill_passage <-
    extract_fase_Senado(bill_passage, phase_one, recebimento_phase, phase_two, phase_three, encaminhamento_phase, phase_four) %>%
    arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    fill(fase)

  bill_passage$situacao_descricao_situacao <-
    to_underscore(bill_passage$situacao_descricao_situacao) %>%
    str_replace_all("\\s+", "_")

  bill_passage <-
    extract_fase_casa_Senado(bill_passage, phase_one) %>%
    arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    fill(casa) %>%
    filter(!is.na(casa))

  important_events <- frame_data(~ evento, ~ situacao_codigo_situacao,
                                 "aprovacao_audiencia_publica", 110,
                                 "aprovacao_parecer", 89,
                                 "aprovacao_substitutivo", 113,
                                 "pedido_vista", 90,
                                 "aprovacao_projeto", 25)

  evento_devolucao <- c('devolvido pel.*redistribu.*')
  bill_passage <- extract_evento_Senado(bill_passage, important_events, phase_one, evento_devolucao)
  index_of_camara <- ifelse(length(which(bill_passage$situacao_codigo_situacao == 52)) == 0,
                            nrow(bill_passage),
                            which(bill_passage$situacao_codigo_situacao == 52)[1])
  bill_passage <-
    bill_passage[1:index_of_camara, ] %>%
    extract_locais() %>%
    extract_fase_global(bill_id) %>%
    filter(!is.na(fase))

  bill_passage %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-fases-tramitacao-senado.csv"))

  bill_passage_visualization <-
    bill_passage %>%
    select(data_tramitacao, local, fase, evento, casa, global)

  # Print evento freq table
  bill_passage_visualization %>% select(evento) %>% group_by(evento) %>%
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>%
    arrange(-frequência)


  bill_passage_visualization %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-visualizacao-tramitacao-senado.csv"))
}

#' @title Processa dados de um proposição da câmara.
#' @description Recebido um pl_id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/camara.
#' @param pl_id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' process_proposicao_camara(257161)
#' @export
process_proposicao_camara <- function(pl_id) {
  data_path <- here::here('data/camara/')
  tramitacao_pl <- rcongresso::fetch_tramitacao(pl_id)

  csv_path <- paste(c(data_path,'tramitacao-camara-', pl_id, '.csv'),  collapse = '')
  proposicao_csv_path <- paste(c(data_path,'proposicao-camara-', pl_id, '.csv'),  collapse = '')

  # Extract phases, events and writh CSV
  tramitacao_pl %<>%
    rename_df_columns %>%
    extract_events_in_camara() %>%
    extract_locais_in_camara() %>%
    extract_fase_casa_in_camara() %>%
    extract_situacao_comissao() %>%
    refact_date() %>%
    sort_by_date() %>%
    readr::write_csv(csv_path)

  # Print evento freq table
  tramitacao_pl %>% select(evento) %>% group_by(evento) %>%
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>%
    arrange(-frequência)

  proposicao_pl <-
    fetch_proposicao_renamed(pl_id)

  data.frame(lapply(proposicao_pl, as.character), stringsAsFactors=FALSE) %>%
    readr::write_csv(proposicao_csv_path)

  relatorias <- extract_relatorias_in_camara(as.data.frame(read_csv(csv_path)))

  tramitacao_pl
}

#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao_camara(id) %>%
    rename_df_columns

  df[, !sapply(df, is.list)]
}

extract_evento_in_camara <- function(df) {
  camara_codes <- get_environment_camara_json()
  eventos <- camara_codes$eventos
  novo_despacho_regex <- eventos$regex$novo_despacho
  redistribuicao_regex <- eventos$regex$redistribuicao
  redistribuicao_text <- eventos$text$distribuicao %>% tolower()
  df %>%
    mutate(
      evento =
        case_when((str_detect(tolower(despacho), regex(redistribuicao_regex, ignore_case = TRUE)) |
                     str_detect(tolower(despacho), regex(novo_despacho_regex, ignore_case = TRUE))) &
                    tolower(descricao_tramitacao) == redistribuicao_text ~ "redistribuicao"))
}

#' @title Cria coluna com as fases da tramitação no Senado
#' @description Cria uma nova coluna com as fases no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com a coluna "fase" adicionada.
#' @examples
#' tramitacao %>% extract_fase_Senado()
#' @export
extract_fase_Senado <- function(dataframe, recebimento_phase, phase_one, phase_two, phase_three, encaminhamento_phase, phase_four) {
    fases <- senado_env$fase_subfase_comissoes
    dataframe %>%
        dplyr::mutate(
                   fase =
                       dplyr::case_when(stringr::str_detect(tolower(texto_tramitacao), fase$regex) ~ fases$recebimento,
                                        detect_fase(situacao_codigo_situacao, phase_two) ~ fases$analise,
                                        detect_fase(situacao_codigo_situacao, phase_three) ~ fases$discussao,
                                        detect_fase(situacao_codigo_situacao, encaminhamento_phase) ~ fases$encaminhamento))
}

#' @title Cria coluna com a fase global da tramitação no Senado
#' @description Cria uma nova coluna com a fase global no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com a coluna "global" adicionada.
#' @examples
#' tramitacao %>% extract_fase_global()
#' @export
extract_fase_global <- function(data_tramitacao, bill_id) {
  fase_global_constants <- senado_env$fase_global
  data_prop <- read_csv(paste0(here::here("data/Senado/"), bill_id, "-proposicao-senado.csv"))
  casa_origem <- if_else(data_prop$nome_casa_origem == "Senado Federal",
                         fase_global_constants$origem_senado,
                         fase_global_constants$revisao_camara)

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
#' extract_evento_Senado(df, importants_events, phase_one)
#' @export
extract_evento_Senado <- function(tramitacao_df, phases_df, evento_apresentacao, evento_devolucao) {
  dplyr::left_join(tramitacao_df, phases_df, by = "situacao_codigo_situacao") %>%
    dplyr::mutate(evento =
                    dplyr::case_when(
                      grepl(evento_apresentacao, texto_tramitacao) ~ 'apresentação',
                      grepl("aprovado requerimento de realização de audiência pública", texto_tramitacao) ~ "aprovacao_audiencia_publica",
                      grepl(evento_devolucao, tolower(texto_tramitacao)) ~ 'devolvido',
                      TRUE ~ evento
                    ))
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
  codigos_comissoes <- senado_env$comissoes

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

#' @title Recupera o numero e a ementa de uma proposição no Senado
#' @description Retona um dataframe de uma proposição no senado contendo o número, a ementa e o tipo da proposição
#' @param df ID da proposição no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' get_nome_ementa_Senado(91341)
#' @export
get_nome_ementa_Senado <- function(proposicao_id) {
  fetch_proposicao(proposicao_id, 'senado') %>%
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

#' @title Recupera os locais do Senado
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada local
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe da tramitacao contendo mais uma coluna chamada local
#' @examples
#'  extract_locais(fetch_tramitacao(91341))
#' @export
extract_locais <- function(df) {
  comissao_json <- senado_env$fase_comissao
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
    regime <- senado_env$regimes
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
