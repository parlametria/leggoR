source(here::here("R/camara-lib.R"))
source(here::here("R/requerimentos.R"))

camara_env <- jsonlite::fromJSON(here::here("R/config/environment_camara.json"))

#' @title Busca os últimos n eventos da tramitação na Câmara
#' @description Recupera os útimos n eventos da tramitação na Câmara, caso nenhuma quantidade seja informada, assume-se que é 1
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe dos últimos n eventos na Câmara contendo hora e evento.
extract_last_n_events_in_camara <- function(df, num) {
  df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::arrange(data_hora) %>%
    tail(n = num) %>%
    dplyr::select(data_hora, evento)
}

#' @title Renomeia as colunas do dataframe
#' @description Renomeia as colunas do dataframe usando o padrão de letras minúsculas e underscore
#' @param df Dataframe
#' @return Dataframe com as colunas renomeadas.
rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

#' @title Extrai os eventos importantes que aconteceram na Câmara
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram na Câmara
#' @param tramitacao_df Dataframe da tramitação na Câmara
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
extract_events_in_camara <- function(tramitacao_df) {
  eventos_camara <- camara_env$eventos %>% dplyr::select(evento, regex)
  df <- tramitacao_df %>% 
    dplyr::mutate(texto_lower = tolower(stringr::str_trim(
      stringr::str_replace_all(texto_tramitacao,'[\r\n]', '')))) %>% 
    fuzzyjoin::regex_left_join(eventos_camara, by = c(texto_lower = "regex")) %>%
    dplyr::select(-texto_lower, -regex)
}

#' @title Recupera o autor de uma proposição na Câmara
#' @description Retorna um dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor
#' @param prop_id ID da proposição
#' @return Dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor.
#' @examples
#' extract_autor_in_camara(2121442)
#' @export
extract_autor_in_camara <- function(prop_id) {
  camara_exp <- "câmara dos deputados"
  senado_exp <- "senado federal"

  url_base_autores <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes/"
  url <- paste0(url_base_autores, prop_id, "/autores")
  json_voting <- jsonlite::fromJSON(url, flatten = T)

  authors <- json_voting %>%
    magrittr::extract2("dados") %>%
    dplyr::rename(
      autor.uri = uri,
      autor.nome = nome,
      autor.tipo = tipo,
      autor.cod_tipo = codTipo) %>%
    dplyr::mutate(casa_origem =
                    dplyr::case_when(
                      stringr::str_detect(tolower(autor.nome), camara_exp) | autor.tipo == "Deputado" ~ "Câmara dos Deputados",
                      stringr::str_detect(tolower(autor.nome), senado_exp) | autor.tipo == "Senador" ~ "Senado Federal",
                      autor.cod_tipo == 40000 ~ "Senado Federal",
                      autor.cod_tipo == 2 ~ "Câmara dos Deputados"))

  # authors <- authors %>%
  #   mutate(autor.nome = dplyr::if_else(casa_origem == 'Senado Federal', stringr::str_split(autor.nome,'-')[[2]], autor.nome))

  partido_estado <- rcongresso::extract_partido_estado_autor(authors$autor.uri %>% tail(1))

  authors %>%
    dplyr::mutate(autor.nome = paste0(autor.nome, " ", partido_estado))
}

#' @title Recupera os locais da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada local
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada local
#' @examples
#'  extract_locais_in_camara(fetch_tramitacao(2121442, 'camara', T))
extract_locais_in_camara <- function(df) {
  descricoes_plenario <-
    c('votação', 'pronta para pauta', 'apresentação de proposição',
      'sessão deliberativa')
  descricoes_comissoes <- c('recebimento pela')

  df %<>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(
          (stringr::str_detect(tolower(texto_tramitacao), '(projeto( foi|) encaminhado à sanção presidencial)|(remessa à sanção.*)')) ~ 'Presidência da República',
          (tolower(texto_tramitacao) %in% descricoes_plenario |
             stringr::str_detect(tolower(texto_tramitacao), '^votação')) & sigla_local == 'PLEN' ~ 'Plenário',
          (stringr::str_detect(tolower(texto_tramitacao), '^recebimento pela') |
             tolower(texto_tramitacao) %in% descricoes_comissoes) &
            sigla_local != 'CCP' &
            !stringr::str_detect(tolower(sigla_local), '^s') ~ sigla_local,
          tolower(texto_tramitacao) == 'remessa ao senado federal' ~ 'Câmara')
    ) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(stringr::str_detect(local, "^((PL)|(PEC))") ~ "Comissão Especial",
                         TRUE ~ local)
    )

  if (is.na(df[1, ]$local)) {
    df[1, ]$local = 'CD-MESA-PLEN'
  }

  df %>%
    tidyr::fill(local)
}

#' @title Recupera os eventos da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada evento
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada evento
#' @examples
#'  extract_evento_in_camara(fetch_tramitacao(2121442, 'camara', T))
extract_evento_in_camara <- function(df) {
  eventos <- camara_env$eventos
  novo_despacho_regex <- eventos$regex$novo_despacho
  redistribuicao_regex <- eventos$regex$redistribuicao
  redistribuicao_text <- eventos$text$distribuicao %>% tolower()
  df %>%
    dplyr::mutate(evento =
                    case_when((str_detect(
                      tolower(texto_tramitacao),
                      stringr::regex(redistribuicao_regex, ignore_case = TRUE)
                    ) |
                      str_detect(
                        tolower(texto_tramitacao),
                        stringr::regex(novo_despacho_regex, ignore_case = TRUE)
                      )) &
                      tolower(texto_tramitacao) == redistribuicao_text ~ "redistribuicao"
                    ))
}

#' @title Recupera as casas da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada casa
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada casa
#' @examples
#'  extract_fase_casa_in_camara(fetch_tramitacao(2121442, 'camara', T))
extract_fase_casa_in_camara <- function(df) {
  descricoes_plenario <- c('votação', 'pronta para pauta', 'apresentação de proposição', 'sessão deliberativa')
  descricoes_comissoes <- c('recebimento pela')

  df <- df %>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      casa =
        dplyr::case_when(
          (tolower(texto_tramitacao) %in% descricoes_plenario & sigla_local == 'PLEN' |
             stringr::str_detect(tolower(texto_tramitacao), '^votação')) ~ 'Plenário',
          (stringr::str_detect(tolower(texto_tramitacao), '^recebimento pela') |
             tolower(texto_tramitacao) %in% descricoes_comissoes) & sigla_local != 'CCP' & !stringr::str_detect(tolower(sigla_local), '^s') ~ "Comissões")
    )


  if (is.na(df[1, ]$casa)) {
    df[1, ]$casa <- 'Apresentação'
  }

  df %>%
    tidyr::fill(casa)
}

#' @title Recupera a situação em que a proposição se encontra na comissão na Câmara
#' @description Retorna o dataframe da tramitação com a coluna situacao_comissao adicionada (ex. recebimento, encaminhamento)
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitação contendo a coluna situacao_comissao
#' @examples
#'  extract_situacao_comissao(process_proposicao_camara(345311))
extract_situacao_comissao <- function(df) {

  situacao_comissao <- camara_env$situacao_comissao
  situacao_comissao['local'] <- get_regex_comissoes_camara()

  df %>%
    regex_left_match(situacao_comissao, "situacao_comissao") %>%
    tidyr::fill(situacao_comissao)
}


#' @title Processa dados de um proposição da câmara.
#' @description Recebido um dataframe com a tramitação, a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/camara.
#' @param tramitacao_df Dataframe com tramitação da proposição
#' @importFrom magrittr %>%
process_proposicao_camara_df <- function(proposicao_df, tramitacao_df) {
  proc_tram_df <- tramitacao_df %>%
    extract_events_in_camara()

  virada_de_casa <-
    proc_tram_df %>%
    dplyr::filter(evento == 'virada_de_casa')

  if(nrow(virada_de_casa) == 1){
    proc_tram_df <-
    proc_tram_df[1:get_linha_virada_de_casa(proc_tram_df),]
  }else {
    index_of_sancao <-
      get_linha_finalizacao_tramitacao(proc_tram_df)
    proc_tram_df <-
      proc_tram_df[1:index_of_sancao,]
  }
  
  eventos_reqs <- fetch_eventos_reqs_prop(proposicao_df$prop_id, proposicao_df$casa)

  proc_tram_df <-
    proc_tram_df %>%
    extract_locais_in_camara() %>%
    dplyr::bind_rows(eventos_reqs) %>%
    #extract_fase_global_in_camara(proposicao_df) %>%
    refact_date() %>%
    sort_by_date()
    #tidyr::fill(global)

  return(proc_tram_df)
}

#Fetch a bill with renamed columns
fetch_proposicao_renamed <- function(id) {
  df <-
    fetch_proposicao_camara(id) %>%
    rename_df_columns

  df[,!sapply(df, is.list)]
}

#' @title Extrai o regime de apreciação na Câmara
#' @description Obtém o regime de apreciação de um PL
#' @param proposicao_id id da proposicao
#' @return String com a situação do PL.
#' @examples
#' extract_forma_apreciacao_camara(217161)
#' @export
#' @importFrom stats filter
extract_forma_apreciacao_camara <- function(prop_id) {
  base_url <-
    'http://www.camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao='
  regex_apreciacao <-
    tibble::frame_data(
      ~ forma_apreciacao,
      ~ regex,
      'Conclusiva',
      'Sujeita à Apreciação Conclusiva pelas Comissões',
      'Plenário',
      'Sujeita à Apreciação do Plenário'
    )

  page_df <- data.frame(page_url = paste0(base_url, prop_id), stringsAsFactors = F)

  apreciacao_df <- page_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(page_html = list(xml2::read_html(page_url))) %>%
    dplyr::mutate(temp =
                    rvest::html_node(page_html, '#informacoesDeTramitacao') %>%
                    rvest::html_text()) %>%
    fuzzyjoin::regex_left_join(regex_apreciacao, by = c(temp = "regex"))

  return(apreciacao_df[1,]$forma_apreciacao)
}

#' @title Extrai o regime de tramitação de um PL na Câmara
#' @description Obtém o regime de tramitação de um PL.
#' @param df Dataframe da tramitação na Câmara.
#' @return String com a situação do regime de tramitação da PL.
#' @examples
#' extract_regime_tramitacao_camara(fetch_tramitacao(257161,'camara', TRUE))
extract_regime_tramitacao_camara <- function(tram_df) {
  regimes <- camara_env$regime %>%
    tibble::as.tibble()

  prop_id <- tram_df[1,]$prop_id

  regime_df <- rcongresso::fetch_proposicao_camara(prop_id) %>%
    fuzzyjoin::regex_left_join(regimes,
                               by = c(statusProposicao.regime = "regex"))
  return(regime_df[1,]$regime_tramitacao)
}

#' @title Extrai os locais globais (Comissões/Plenário/Presidência da República) para proposições da Câmara
#' @description Retorna o dataframe da tamitação com o local global setado na coluna chamada local
#' @param tramitacao_com_fases Dataframe da tramitação na Câmara já com as fases globais
#' @return Dataframe da tramitacao com o local global setado na coluna chamada local
extract_local_global_in_camara <- function(tramitacao_com_fases) {
  tramitacao_com_fases %>%
    dplyr::mutate(
      local =
        dplyr::case_when(
          (stringr::str_detect(tolower(texto_tramitacao), "(projeto( foi|) encaminhado à sanção presidencial)|(remessa à sanção.*)")) ~ 'Presidência da República',
          (stringr::str_detect(tolower(texto_tramitacao), camara_env$plen_global$plenario) & sigla_local == "PLEN") ~ "Plenário",
          sigla_local != "PLEN" & (sigla_local %in% camara_env$comissoes$siglas_comissoes_antigas | sigla_local %in% camara_env$comissoes$siglas_comissoes | stringr::str_detect(tolower(sigla_local), "^pl"))  ~ "Comissões"))
}

#' @title Extrai as casas globais (Origem Câmara, Plenário Câmara, etc.) da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada fase_global
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada fase_global
#' @examples
#'  extract_fase_global_in_camara(fetch_tramitacao(2121442, 'camara', T) %>% extract_events_in_camara() %>% extract_locais_in_camara(), fetch_proposicao(2121442, 'camara', '', '', normalized=T))
extract_fase_global_in_camara <- function(data_tramitacao, proposicao_df) {
  fase_global_constants <- camara_env$fase_global
  
  casa_origem <-
    dplyr::if_else((!is.null(proposicao_df$casa_origem) & !is.na(proposicao_df$casa_origem)) & 
                     (proposicao_df$casa_origem == "senado"),
                   fase_global_constants$revisao_camara,
                   fase_global_constants$origem_camara
    )
  
  virada_de_casa <-
    data_tramitacao %>%
    dplyr::filter(evento == 'virada_de_casa') %>%
    dplyr::arrange(data_hora) %>%
    dplyr::select(data_hora)
  
  casa_atual <-
    dplyr::if_else(
      casa_origem == fase_global_constants$origem_camara,
      fase_global_constants$revisao_senado,
      fase_global_constants$origem_camara
    )
  
  casa_revisao2 <-
    dplyr::if_else(
      casa_origem == fase_global_constants$origem_camara,
      fase_global_constants$revisao2_camara,
      fase_global_constants$revisao2_senado
    )
  
  data_apresentacao <- proposicao_df$data_apresentacao
  
  if (nrow(virada_de_casa) == 0) { #não virou de casa
    data_tramitacao <- data_tramitacao %>%
      # Tratando eventos de apensadas que ocorreram antes da apresentação
      dplyr::mutate(global = dplyr::if_else(data_hora < data_apresentacao,paste0("Pre",casa_origem), 
                                            casa_origem))
    
  } else { #virou de casa pelo menos uma vez
    
    data_tramitacao <-
      data_tramitacao %>%
      dplyr::mutate(global = dplyr::if_else(
        data_hora < virada_de_casa[1, ][[1]],
        casa_origem,
        casa_atual
      ))
    
  }
  
  if(nrow(virada_de_casa) > 1) { #virou de casa duas vezes
    data_tramitacao <-
      data_tramitacao %>%
      dplyr::mutate(global = dplyr::if_else(
        data_hora >= virada_de_casa[nrow(virada_de_casa), ][[1]],
        casa_revisao2,
        global
      ))
  }
  
  data_tramitacao <-
    data_tramitacao %>%
    dplyr::mutate(global = dplyr::if_else(evento == "remetida_a_sancao", "- Sanção/Veto", global)) %>%
    tidyr::fill(global, .direction = "down")
  
  return(data_tramitacao)
}

#' @title Extrai os números dos requerimentos da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada num_requerimento
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada num_requerimento
#' @examples
#'  extract_num_requerimento_audiencia_publica_in_camara(fetch_tramitacao(2121442, 'camara', T))
extract_num_requerimento_audiencia_publica_in_camara <- function(tramitacao_df) {
  tramitacao_df <-
    tramitacao_df %>%
    dplyr::filter(evento == 'aprovacao_audiencia_publica') %>%
    dplyr::mutate (
      extract_requerimento_num = dplyr::if_else(
        stringr::str_extract(
          texto_tramitacao, camara_env$extract_requerimento_num$regex) != 'character(0)',
        stringr::str_extract(texto_tramitacao, camara_env$extract_requerimento_num$regex),
        '0'),
      num_requerimento = dplyr::if_else(stringr::str_detect(extract_requerimento_num, stringr::regex('/[0-9]{4}')),
                                        sub('/[0-9]{2}', '/', extract_requerimento_num) %>%
                                          lapply(function(list)(gsub(" ","",list))),
                                        extract_requerimento_num %>%
                                          lapply(function(list)(gsub(" ","",list))))

    ) %>%
    dplyr::select(-extract_requerimento_num)
  tramitacao_df
}

#' @title Extrai as próximas audiências públicas de uma PL na Câmara
#' @description Extrai as próximas audiências públicas de uma PL na Câmara
#' @param initial_date data inicial no formato dd/mm/yyyy
#' @param end_date data final no formato dd/mm/yyyy
#' @param fases_tramitacao_df dataframe da PL preprocessada
#' @return Dataframe com as próximas audiências públicas de uma PL na Câmara
#' @examples
#' get_next_audiencias_publicas_in_camara(initial_date = '01/01/2016', end_date = '30/10/2018', fases_tramitacao_df = process_proposicao(fetch_proposicao(2121442, 'Lei do Teto Remuneratório', 'Meio Ambiente'), fetch_tramitacao(2121442, 'camara', T), 'camara'), next_audiencias_publicas_by_orgao = fetch_audiencias_publicas_by_orgao_camara('01/01/2016', '30/10/2018', process_proposicao(fetch_proposicao(2121442, 'camara', 'Lei do Teto Remuneratório', 'Meio Ambiente'), fetch_tramitacao(2121442, 'camara', T), 'camara')))
get_next_audiencias_publicas_in_camara <- function(initial_date, end_date, fases_tramitacao_df, next_audiencias_publicas_by_orgao){
  prop_id <- fases_tramitacao_df %>% dplyr::select(prop_id) %>% utils::tail(1)
  casa <- fases_tramitacao_df %>% dplyr::select(casa) %>% utils::tail(1)

  num_requerimentos_audiencias_publicas <-
    extract_num_requerimento_audiencia_publica_in_camara(fases_tramitacao_df)

  next_audiencias_publicas_by_orgao <-
    next_audiencias_publicas_by_orgao %>%
    dplyr::filter(num_requerimento != '0')

  if(nrow(next_audiencias_publicas_by_orgao) > 0 & nrow(num_requerimentos_audiencias_publicas) > 0){

    next_audiencias_publicas_by_orgao <-
      next_audiencias_publicas_by_orgao %>%
      tidyr::unnest() %>%
      dplyr::distinct()

    next_audiencias_publicas_pl <-
      next_audiencias_publicas_by_orgao %>%
      merge(num_requerimentos_audiencias_publicas %>%
              dplyr::select(prop_id, casa, num_requerimento), by = "num_requerimento")

    if(nrow(next_audiencias_publicas_pl) > 0){
      next_audiencias_publicas_pl$prop_id <- prop_id$prop_id
      next_audiencias_publicas_pl$casa <- casa$casa

      next_audiencias_publicas_pl <-
        next_audiencias_publicas_pl %>%
        dplyr::select(-num_requerimento, comissao, cod_reuniao, data, hora, local,
                      estado, tipo, titulo_reuniao, objeto, prop_id, casa) %>%
        dplyr::group_by(data) %>%
        dplyr::distinct()

      return(next_audiencias_publicas_pl)
    }
  }
  return(tibble::frame_data(~ comissao, ~ cod_reuniao, ~ data, ~ hora, ~ local,
                            ~ estado, ~ tipo, ~ titulo_reuniao, ~ objeto,
                            ~ prop_id, ~ casa))
}

#' @title Desencadeia as listas de requerimentos de audiências públicas
#' @description Desencadeia as listas de requerimentos de audiências públicas
#' @param df dataframe da agenda das audiências públicas
#' @return Dataframe com as próximas audiências públicas com os requerimentos desencadeados
remove_unnested_list <- function(df){
  df %>%
    tidyr::unnest() %>%
    dplyr::distinct()
}
