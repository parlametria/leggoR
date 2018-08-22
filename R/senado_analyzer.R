source(here::here("R/congresso-lib.R"))


#' @title Cria coluna com as fases da tramitação no Senado
#' @description Cria uma nova coluna com as fases no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com a coluna "fase" adicionada.
#' @examples
#' tramitacao %>% extract_fase_Senado()
extract_fase_Senado <-
  function(dataframe,
           recebimento_phase,
           phase_one,
           #phase_two,
           phase_three,
           encaminhamento_phase,
           phase_four,
           comissoes_phase) {
    fases <- senado_env$fase_subfase_comissoes
    dataframe %>%
      dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
      dplyr::mutate(
        fase =
          dplyr::case_when(
            stringr::str_detect(tolower(texto_tramitacao), fases$regex) ~ fases$recebimento,
            stringr::str_detect(tolower(texto_tramitacao), comissoes_phase) ~ fases$analise,
            #detect_fase(situacao_codigo_situacao, phase_two) ~ fases$analise,
            detect_fase(situacao_codigo_situacao, phase_three) ~ fases$discussao,
            detect_fase(situacao_codigo_situacao, encaminhamento_phase) ~ fases$encaminhamento
          )
      )
  }

#' @title Cria coluna com a fase global da tramitação no Senado
#' @description Cria uma nova coluna com a fase global no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com a coluna "global" adicionada.
#' @examples
#' tramitacao %>% extract_fase_global()
extract_fase_global <- function(data_tramitacao, proposicao_df) {
  fase_global_constants <- senado_env$fase_global
  
  casa_origem <-
    dplyr::if_else(
      proposicao_df$nome_casa_origem == "Senado Federal",
      fase_global_constants$origem_senado,
      fase_global_constants$revisao_camara
    )
  
  virada_de_casa <-
    data_tramitacao %>%
    dplyr::filter(local == 'Mesa - Câmara') %>%
    dplyr::arrange(data_tramitacao) %>%
    dplyr::select(data_tramitacao)
  
  if (nrow(virada_de_casa) == 0) {
    data_tramitacao %>%
      dplyr::mutate(global = paste0(casa_origem))
  } else {
    casa_atual <-
      dplyr::if_else(
        casa_origem == " - Origem (Senado)",
        fase_global_constants$revisao_camara,
        fase_global_constants$origem_senado
      )
    data_tramitacao %>%
      dplyr::mutate(global = dplyr::if_else(
        data_tramitacao < virada_de_casa[1, ][[1]],
        casa_origem,
        casa_atual
      ))
  }
}


#' @title Cria coluna com a fase casa da tramitação no Senado
#' @description Cria uma nova coluna com a fase casa no Senado
#' @param df Dataframe da tramitação no Senado com as descrições formatadas
#' @return Dataframe com a coluna "casa" adicionada.
#' @examples
#' bill_passage %>% extract_fase_casa_Senado(phase_one)
extract_fase_casa_Senado <- function(dataframe, fase_apresentacao, recebimento_phase, fase_comissoes) {
  dataframe <-
    dataframe %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    dplyr::mutate( 
      casa =
        dplyr::case_when(
          grepl(fase_apresentacao, texto_tramitacao) ~ 'Apresentação',
          situacao_descricao_situacao %in% senado_constants$regex_plenario ~
            senado_constants$plenario,
          (
            stringr::str_detect(
              tolower(texto_tramitacao),
              senado_constants$regex_recebimento_comissoes
            ) |
              situacao_descricao_situacao %in% senado_constants$regex_comissoes_vector
              |
              (stringr::str_detect(
                tolower(texto_tramitacao),
                stringr::regex(fase_comissoes, ignore_case = TRUE)
              ))
            | (stringr::str_detect(
              tolower(texto_tramitacao),
              stringr::regex(recebimento_phase, ignore_case = TRUE)
            ))
          ) ~
            senado_constants$comissoes
        )
    ) %>%
    tidyr::fill(casa)
  
  dataframe %>%
    dplyr::mutate(casa = dplyr::if_else(is.na(casa), senado_constants$mesa_senado, casa))
}

#' @title Extrai os eventos importantes que aconteceram no Senado
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram no Senado
#' @param tramitacao_df Dataframe da tramitação no Senado
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
#' @examples
#' df <- fetch_tramitacao(91341)
#' extract_evento_Senado(df)
extract_evento_Senado <- function(tramitacao_df) {
  df <- regex_left_match(tramitacao_df, senado_env$eventos, "evento")
  eventos <- senado_env$evento
  
  comissoes <- extract_comissoes_Senado(tramitacao_df)
  date_comissao_especial <- comissoes[match("Comissão Especial", comissoes$comissoes), ]$data_tramitacao
  designacao_relator <- senado_env$eventos %>%
    dplyr::filter(evento == 'designado_relator') 

  if (!is.na(date_comissao_especial)) {
    df %>%
      dplyr::mutate(evento =
                      dplyr::case_when(
                        data_tramitacao == date_comissao_especial ~ 'comissão_especial',
                        TRUE ~ evento
                      ))
  }
  
  df <- df %>% dplyr::mutate(
    evento = dplyr::case_when(
      stringr::str_detect(tolower(texto_tramitacao), stringr::regex(designacao_relator$texto_tramitacao, 
                                                           ignore_case = TRUE)) ~ designacao_relator$evento,
      stringr::str_detect(tolower(texto_tramitacao), eventos$aprovacao_substitutivo$regex) ~ eventos$aprovacao_substitutivo$constant,
      stringr::str_detect(tolower(texto_tramitacao), eventos$arquivamento$regex) ~ eventos$arquivamento$constant,
      (stringr::str_detect(tolower(texto_tramitacao), eventos$realizacao_audiencia_publica$regex) &
         !stringr::str_detect(tolower(texto_tramitacao), eventos$realizacao_audiencia_publica$regex_complementar)) ~ eventos$realizacao_audiencia_publica$constant,
      TRUE ~ evento
  ))
  
  df %>%
    dplyr::mutate(data_audiencia = stringr::str_extract(tolower(texto_tramitacao), "\\d+/\\d+/\\d+")) %>%
    dplyr::mutate(data_audiencia = ifelse(evento == 'realizacao_audiencia_publica', data_audiencia, NA))
  
}

#' @title Recupera os n últimos eventos importantes que aconteceram no Senado
#' @description Retona dataframe contendo os n últimos eventos importantes que aconteceram no Senado
#' @param tramitacao_df Dataframe da tramitação no Senado
#' @param num Quantidade de eventos a serem recuperados
#' @return Dataframe contendo os n últimos eventos importantes que aconteceram no Senado
#' @examples
#' df %>% extract_n_last_eventos_Senado(4)
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
    paste(collapse = '|') %>%
    stringr::regex(ignore_case = FALSE)
  
  # Faz com que os nomes comecem com 'Comissão'.
  fix_names <- function(name) {
    name %>%
      stringr::str_replace('Comissões', 'Comissão') %>%
      sapply(function(name) {
        if (!stringr::str_detect(name, 'Comissão') &
            !stringr::str_detect(siglas_comissoes, name))
          paste0('Comissão', name)
        else
          name
      },
      USE.NAMES = FALSE)
  }
  
  detect <- function(texto_tramitacao, regex1, regex2 = NULL) {
    if (is.null(regex2))
      regex2 <- regex1
    stringr::str_detect(tolower(texto_tramitacao), regex1) ~
      stringr::str_extract(texto_tramitacao,
                           stringr::regex(regex2, ignore_case = TRUE))
  }
  
  df %>%
    dplyr::mutate(comissoes =
                    dplyr::case_when(
                      detect(texto_tramitacao,
                             codigos_comissoes$regex_1),
                      detect(texto_tramitacao,
                             codigos_comissoes$regex_2),
                      detect(texto_tramitacao,
                             codigos_comissoes$regex_3)
                    )) %>%
    dplyr::filter(!is.na(comissoes)) %>%
    dplyr::arrange(data_tramitacao) %>%
    dplyr::select(codigo_materia, comissoes, data_tramitacao) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(comissoes = stringr::str_extract_all
                  (comissoes, comissoes_permanentes_especiais)) %>%
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
tail_descricao_despacho_Senado <- function(df, qtd = 1) {
  df %>%
    dplyr::arrange(data_tramitacao) %>%
    tail(qtd) %>%
    dplyr::select(data_tramitacao,
                  situacao_descricao_situacao,
                  texto_tramitacao)
}

#' @title Recupera os locais do Senado
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada local
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe da tramitacao contendo mais uma coluna chamada local
#' @examples
#'  extract_locais(fetch_tramitacao(91341))
extract_locais <- function(df) {
  comissao_json <- senado_env$fase_comissao
  df <-
    df %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(
          situacao_descricao_situacao %in% senado_constants$regex_plenario ~
            senado_constants$plenario,
          (
            stringr::str_detect(
              tolower(texto_tramitacao),
              senado_constants$regex_recebimento_comissoes
            ) |
              situacao_descricao_situacao %in% senado_constants$regex_comissoes_vector
          ) ~
            origem_tramitacao_local_sigla_local,
          situacao_descricao_situacao == senado_constants$regex_camara ~
            senado_constants$mesa_camara
        )
    )
  
  if (is.na(df[1,]$local)) {
    df[1,]$local = senado_constants$mesa_senado
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
extract_regime_Senado <- function(proposicao_id) {
  df <- fetch_tramitacao(proposicao_id)
  regime <- senado_env$regimes
  df <-
    df %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    dplyr::mutate(regime =
                    dplyr::case_when(
                      stringr::str_detect(tolower(texto_tramitacao), regime$regex) ~
                        regime$urgencia
                    )) %>%
    tidyr::fill(regime)
  
  if (is.na(df[nrow(df),]$regime)) {
    regime$ordinaria
  } else{
    df[nrow(df),]$regime
  }
}

#' @title Recupera as comissões que a proposição originalmente vai passar
#' @description Retorna dataframe contendo o código da proposição, as comissões e a data
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe contendo o código da proposição, as comissões e a data
#' @examples
#' extract_first_comissoes_Senado(fetch_tramitacao(129808))
extract_first_comissoes_Senado <- function(df) {
  extract_comissoes_Senado(df)[1,]
}

#' @title Cria dataframe dos requerimentos aprovados no Senado
#' @description Cria um dataframe com os requerimentos que foram aprovados no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com os requerimentos aprovados no Senado.
#' @examples
#' tramitacao %>% extract_approved_requerimentos_in_senado()
extract_approved_requerimentos_in_senado <- function(df) {
  apr_requerimentos_regex <- c('(aprova.* | deferid.*)requeriment.* nº.*')
  apr_requerimentos_df <- df %>%
    dplyr::filter(str_detect(tolower(texto_tramitacao), stringr::regex(apr_requerimentos_regex, ignore_case = TRUE))) %>%
    dplyr::mutate(
      evento = str_extract_all(tolower(texto_tramitacao), stringr::regex(apr_requerimentos_regex, ignore_case = TRUE))) %>%
    unnest() %>%
    distinct()

  if(nrow(apr_requerimentos_df) > 0) {
    apr_requerimentos_df <- apr_requerimentos_df %>%
      dplyr::mutate(
        numero_requerimento = gsub("^.*((aprova(do(s|)|)|deferido(s|).*) o(s|) requerimento(s|) nº(s|)\\s*)|\\s*(,).*$", "", evento))
  }
  apr_requerimentos_df
}

#' @title Processa dados de uma proposição do senado.
#' @description Recebido um bill_id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/Senado.
#' @param bill_id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' process_proposicao_senado(91341)
process_proposicao_senado <- function(bill_id) {
  proposicao_df <- 
    readr::read_csv(paste0(
      here::here("data/Senado/"),
      bill_id, 
      "-proposicao-senado.csv"))
    
  bill_passage <-
    readr::read_csv(paste0(
      here::here("data/Senado/"),
      bill_id,
      "-tramitacao-senado.csv"
    )) %>% dplyr::arrange(data_tramitacao)
  
  phase_one <- c('^Este processo contém')
  recebimento_phase <- 'recebido na|nesta comissão'
  phase_two <- c(91)
  phase_three <- c(42, 14, 78, 90)
  encaminhamento_phase <- c(89, 158, 159, 160, 161, 162, 163)
  phase_four <- c(52)
  comissoes_phase <- senado_env$fase_comissao %>%
    dplyr::filter(fase == "analise_do_relator")
  
  bill_passage <-
    extract_fase_Senado(
      bill_passage,
      phase_one,
      recebimento_phase,
      #phase_two,
      phase_three,
      encaminhamento_phase,
      phase_four,
      comissoes_phase$regex
    ) %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    tidyr::fill(fase)
  
  bill_passage$situacao_descricao_situacao <-
    to_underscore(bill_passage$situacao_descricao_situacao) %>%
    stringr::str_replace_all("\\s+", "_")
  
  bill_passage <-
    extract_fase_casa_Senado(bill_passage, phase_one, recebimento_phase, comissoes_phase$regex) %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    tidyr::fill(casa) %>%
    dplyr::filter(!is.na(casa))
  
  bill_passage <-
    extract_evento_Senado(bill_passage) %>%
    dplyr::mutate(data_audiencia = as.Date(data_audiencia, "%d/%m/%Y")) 
  
  index_of_camara <-
    ifelse(
      length(which(
        bill_passage$situacao_codigo_situacao == 52
      )) == 0,
      nrow(bill_passage),
      which(bill_passage$situacao_codigo_situacao == 52)[1]
    )
  bill_passage <-
    bill_passage[1:index_of_camara,] %>%
    extract_locais() %>%
    extract_fase_global(proposicao_df) %>%
    dplyr::filter(!is.na(fase))
  
  bill_passage %>%
    readr::write_csv(paste0(
      here::here("data/Senado/"),
      bill_id,
      "-fases-tramitacao-senado.csv"
    ))
  
  bill_passage_visualization <-
    bill_passage %>%
    dplyr::select(data_tramitacao, local, fase, evento, casa, global, data_audiencia)
  
  # Print evento freq table
  bill_passage_visualization %>% dplyr::select(evento) %>% dplyr::group_by(evento) %>%
    dplyr::filter(!is.na(evento)) %>% dplyr::summarise(frequência = n()) %>%
    dplyr::arrange(-frequência)
  
  
  bill_passage_visualization %>%
    readr::write_csv(paste0(
      here::here("data/Senado/"),
      bill_id,
      "-visualizacao-tramitacao-senado.csv"
    ))
  
  bill_passage
}

#' @title Processa dados de uma proposição do senado.
#' @description Recebido um dataframe da tramitação de um PL, a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/Senado.
#' @param tramitacao_df Dataframe da tramitação da proposição
process_proposicao_senado <- function(proposicao_df, tramitacao_df) {
  phase_one <- c('^Este processo contém')
  recebimento_phase <- 'recebido na|nesta comissão'
  phase_three <- c(42, 14, 78, 90)
  encaminhamento_phase <- c(89, 158, 159, 160, 161, 162, 163)
  phase_four <- c(52)
  comissoes_phase <- senado_env$fase_comissao %>%
    dplyr::filter(fase == "analise_do_relator")
  tramitacao_df <-
    extract_fase_Senado(
      tramitacao_df,
      phase_one,
      recebimento_phase,
      phase_three,
      encaminhamento_phase,
      phase_four,
      comissoes_phase$regex
    ) %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    tidyr::fill(fase)
  
  tramitacao_df$situacao_descricao_situacao <-
    to_underscore(tramitacao_df$situacao_descricao_situacao) %>%
    stringr::str_replace_all("\\s+", "_")
  
  tramitacao_df <-
    extract_fase_casa_Senado(tramitacao_df, phase_one, recebimento_phase, comissoes_phase$regex) %>%
    dplyr::arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    tidyr::fill(casa) %>%
    dplyr::filter(!is.na(casa))
  
  tramitacao_df <-
    extract_evento_Senado(tramitacao_df) %>%
    dplyr::mutate(data_audiencia = as.Date(data_audiencia, "%d/%m/%Y")) 
  
  index_of_camara <-
    ifelse(
      length(which(
        tramitacao_df$situacao_codigo_situacao == 52
      )) == 0,
      nrow(tramitacao_df),
      which(tramitacao_df$situacao_codigo_situacao == 52)[1]
    )
  
  tramitacao_df <-
    tramitacao_df[1:index_of_camara,] %>%
    extract_locais() %>%
    extract_fase_global(proposicao_df) %>%
    dplyr::filter(!is.na(fase))
  
  tramitacao_df
}
