source(here::here("R/congresso-lib.R"))
camara_codes <- jsonlite::fromJSON(here::here("data/environment_camara.json"))

#' @title Recupera o número, o tipo e ementa de uma proposição na Câmara
#' @description Retorna um dataframe contendo o número, o tipo e a ementa de uma proposição na Câmara através do ID da proposição
#' @param prop_id ID da proposição
#' @return Dataframe com o número, o tipo e a ementa da proposição na Câmara.
#' @examples
#' get_ementas_in_camara(2121442)
#' @export
get_ementas_in_camara <- function(prop_id) {
  rcongresso::fetch_proposicao(prop_id) %>% dplyr::select(ementa, siglaTipo, numero)
}

#' @title Recupera os n últimos despachos na Câmara
#' @description Retorna um dataframe das últimas n tramitações na Câmara contendo a hora, a descrição e o despacho
#' @param df Dataframe da tramitação na Câmara
#' @param qtd  (opcional) Quantidade de eventos a serem recuperados. (Default: qtd = 1)
#' @return Dataframe com as última n tramitações da Câmara.
#' @examples
#' tramitacao %>% last_n_despacho_in_camara()
#' tramitacao %>% last_n_despacho_in_camara(4)
#' @export
last_n_despacho_in_camara <- function(df, qtd=1) {
  df %>%
    dplyr::arrange(data_hora) %>%
    tail(qtd) %>%
    dplyr::select(data_hora, descricao_tramitacao, despacho)
}

get_comissoes_camara <- function() {
  c <- camara_codes$comissoes
  
  dplyr::tibble(siglas_comissoes_antigas=list(c$siglas_comissoes_antigas),
                siglas_comissoes=list(c$siglas_comissoes),
                comissoes_temporarias=list(c$comissoes_temporarias),
                comissoes_permanentes=list(c$comissoes_permanentes))
}

#' @title Recupera as comissões pelas quais a proposição irá passar
#' @description Retorna um dataframe das comissões pelas quais a proposição irá passar, contendo a hora, o id da proposição e
#' as próximas comissões
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com as próximas comissões que a proposição irá passar.
#' @examples
#' tramitacao %>% get_comissoes_in_camara(df)
#' @export
get_comissoes_in_camara <- function(df) {
  reg <-
    unlist(get_comissoes_camara()) %>%
    paste(collapse='|') %>%
    stringr::regex(ignore_case=TRUE)

  fix_names <- function(name) {
    if(!str_detect(name, 'Comissão') & !grepl("^[[:upper:]]+$", name))
      paste("Comissão de", name)
    else name
  }

  detect <- function(str, regex) {
    stringr::str_detect(str, stringr::regex(regex, ignore_case=TRUE))
  }

  df %>%
    dplyr::mutate(
      comissoes = dplyr::case_when(
        (detect(descricao_tramitacao, 'distribuição') &
          (detect(descricao_tramitacao, 'cria..o de comiss.o tempor.ria') |
           detect(despacho, 'especial'))
        ) ~ 'Comissão Especial',
        (detect(descricao_tramitacao, 'distribuição') &
         (detect(despacho, 'às* comiss..s*|despacho à') |
          detect(despacho, 'novo despacho'))
        ) ~ despacho
    )) %>%
    dplyr::filter(!is.na(comissoes)) %>%
    dplyr::mutate(
      proximas_comissoes = stringr::str_extract_all(comissoes, reg) %>% as.list()
    ) %>%
    dplyr::select(data_hora, id_prop, proximas_comissoes) %>%
    dplyr::mutate(proximas_comissoes = map(proximas_comissoes, fix_names) ) %>%
    dplyr::mutate(proximas_comissoes = unique(proximas_comissoes, incomparables = FALSE))
}

#' @title Cria coluna com os relatores na tramitação na Câmara
#' @description Cria uma nova coluna com os relatores na Câmara. O relator é adicionado à coluna no
#' envento pontual em que ele é designado
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "relator" adicionada.
#' @examples
#' tramitacao %>% extract_relator_in_camara()
#' @export
extract_relator_in_camara <- function(df) {
  df %>%
    dplyr::mutate(relator=dplyr::case_when(
      stringr::str_detect(tolower(despacho), '^designad. relat.r') ~
        stringr::str_extract(despacho, stringr::regex('dep.+', ignore_case=TRUE))))
}

#' @title Recupera o último relator na Câmara
#' @description Recupera o nome do último relator na Câmara
#' @param df Dataframe da tramitação na Câmara
#' @return String do nome do último relator na Câmara
#' @examples
#' tramitacao %>% extract_last_relator_in_camara()
#' @export
extract_last_relator_in_camara <- function(df) {
  relatores <- extract_relator_in_camara(df)
  relator <-
    relatores %>%
    dplyr::filter(!is.na(relator)) %>%
    dplyr::arrange(desc(data_hora)) %>%
    dplyr::select(relator)

  relator$relator[1]
}

#' @title Cria coluna com as fases da tramitação na Câmara
#' @description Cria uma nova coluna com as fases na Câmara.
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna "fase" adicionada.
#' @examples
#' tramitacao %>% extract_phases_in_camara()
#' @export

extract_phases_in_camara <- function(dataframe, recebimento_phase, phase_one, phase_two, phase_three, encaminhamento_phase, phase_four, phase_five) {
  dataframe %<>%
    dplyr::mutate(fase = dplyr::case_when(
                                          detect_fase(id_tipo_tramitacao, phase_one) ~ 'iniciativa',
                                          detect_fase(id_tipo_tramitacao, recebimento_phase) ~ 'recebimento',
                                          detect_fase(id_tipo_tramitacao, phase_two) ~ 'analise_relator',
                                          detect_fase(id_tipo_tramitacao, phase_three) ~ 'discussao_deliberacao',
                                          detect_fase(id_tipo_tramitacao, encaminhamento_phase) ~ 'encaminhamento',
                                          detect_fase(id_tipo_tramitacao, phase_four) ~ 'virada_de_casa',
                                          detect_fase(id_tipo_tramitacao, phase_five) ~ 'final',
                                          detect_fase(id_situacao, 937) ~ 'final')
                                          )
}

#' @title Busca os últimos n eventos da tramitação na Câmara
#' @description Recupera os útimos n eventos da tramitação na Câmara, caso nenhuma quantidade seja informada, assume-se que é 1
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe dos últimos n eventos na Câmara contendo hora e evento.
#' @examples
#' tramitacao %>% extract_last_n_events_in_camara()
#' tramitacao %>% extract_last_n_events_in_camara(3)
#' @export
extract_last_n_events_in_camara <- function(df, num) {
  df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::arrange(data_hora) %>%
    tail(n = num) %>%
    dplyr::select(data_hora, evento)
}

#' @title Recupera relatores de uma proposição
#' @description Recupera todos os relatores de uma proposição, junto com suas informações de parlamentar e comissão
#' @param tramitacao_df Dataframe da tramitação na Câmara
#' @return Dataframe que contém todos os relatores
#' @examples
#' tramitacao %>% extract_relatorias_in_camara()
#' @export
extract_relatorias_in_camara <- function(tramitacao_df) {
  tramitacao_df %>%
    # extract line when a relator is designated by the code
    dplyr::filter(id_tipo_tramitacao == '320') %>%
    # select columns
    dplyr::select(
      data_hora,
      despacho,
      sigla_orgao
    ) %>%
    add_column() %>%
    # extract relator's name and partido
    dplyr::mutate(
      nome_parlamentar = stringr::str_match(despacho,'Dep. (.*?) [(]')[,2],
      partido = stringr::str_match(despacho,'[(](.*?)[)]')[,2]
    ) %>%
    # remove despacho column and return
    dplyr::select(-c(despacho))
}

#' @title Renomeia as colunas do dataframe
#' @description Renomeia as colunas do dataframe usando o padrão de letras minúsculas e underscore
#' @param df Dataframe
#' @return Dataframe com as colunas renomeadas.
#' @examples
#' df %>% rename_df_columns()
#' @export
rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}

#' @title Extrai os eventos importantes que aconteceram na Câmara
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram na Câmara
#' @param tramitacao_df Dataframe da tramitação na Câmara
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
#' @examples
#' df %>% extract_events_in_camara(importants_events)
#' @export
extract_events_in_camara <- function(tramitacao_df) {
  c <- camara_codes$eventos$regex
  
  events_df <- tibble::frame_data(
    ~ evento, ~ regex,
    'requerimento_audiencia_publica', c$requerimento_audiencia_publica,
    'aprovacao_audiencia_publica', c$aprovacao_audiencia_publica,
    'aprovacao_parecer', c$aprovacao_parecer,
    'redistribuicao', c$redistribuicao,
    'projeto_reconstituido', c$projeto_reconstituido,
    'desarquivada', c$desarquivada,
    'alteracao_de_regime', c$alteracao_de_regime,
    'distribuicao', c$distribuicao)
  
  #events with code
  special_comissao <- camara_codes$eventos$code$comissao_especial
  designado_relator <- camara_codes$eventos$code$designado_relator
  parecer <- camara_codes$eventos$code$parecer
  voto_em_separado <- camara_codes$eventos$code$voto_em_separado
  apresentacao_da_pl <- camara_codes$eventos$code$apresentacao_da_pl
  retirada_de_pauta <- camara_codes$eventos$code$retirada_de_pauta
  pedido_de_vista <- camara_codes$eventos$code$pedido_de_vista
  abertura_prazo_emendas <- camara_codes$eventos$code$abertura_prazo_emendas
  encerramento_prazo_emendas <- camara_codes$eventos$code$encerramento_prazo_emendas
  arquivada <- camara_codes$eventos$code$arquivada

  tramitacao_df %>%
    dplyr::mutate(despacho_lower = tolower(despacho)) %>%
    fuzzyjoin::regex_left_join(events_df, by = c(despacho_lower = "regex")) %>%
    dplyr::select(-c(despacho_lower, regex)) %>%
    dplyr::mutate(evento = dplyr::case_when(
      id_tipo_tramitacao == special_comissao ~ 'criacao_comissao_temporaria',
      id_tipo_tramitacao == designado_relator ~ 'designado_relator',
      id_tipo_tramitacao == voto_em_separado ~ 'voto_em_separado',
      id_tipo_tramitacao == apresentacao_da_pl ~ 'apresentacao_da_pl',
      id_tipo_tramitacao == retirada_de_pauta ~ 'retirada_de_pauta',
      id_tipo_tramitacao == pedido_de_vista ~ 'pedido_de_vista',
      id_tipo_tramitacao == abertura_prazo_emendas ~ 'abertura_prazo_emendas',
      id_tipo_tramitacao == encerramento_prazo_emendas ~ 'encerramento_prazo_emendas',
      id_tipo_tramitacao == arquivada ~ 'arquivada',
      id_tipo_tramitacao == parecer ~ dplyr::case_when(
                                                       str_detect(despacho, regex('substitutivo', ignore_case = TRUE)) ~ 'parecer_pela_aprovacao_com_substitutivo',
                                                       str_detect(despacho, regex('rejei..o', ignore_case = TRUE)) ~ 'parecer_pela_rejeicao',
                                                       str_detect(despacho, regex('aprovacao', ignore_case = TRUE)) ~ 'parecer_pela_aprovacao',
                                                       TRUE ~ 'parecer'
                                                      ),
      TRUE ~ evento))
}

#' @title Altera as datas da tramitação para formato mais fácil de tratar
#' @description Formata cada data da coluna para o formato POSIXct
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe com a coluna de datas refatorada para um formato tratável.
#' @examples
#' df %>% refact_date()
#' @export
refact_date <- function(df) {
  dplyr::mutate(df, data_hora = lubridate::ymd_hm(data_hora))
}

#' @title Ordena o dataframe de acordo com a data
#' @description Ordena o dataframe de acordo com a data
#' @param df Dataframe contendo coluna de datas
#' @return Dataframe ordenado pela data
#' @examples
#' df %>% sort_by_date()
#' @export
sort_by_date <- function(df) {
  dplyr::arrange(df, data_hora, sequencia)
}

#' @title Recupera o autor de uma proposição na Câmara
#' @description Retorna um dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor
#' @param prop_id ID da proposição
#' @return Dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor.
#' @examples
#' extract_autor_in_camara(2121442)
#' @export
extract_autor_in_camara <- function(prop_id) {
  camara_exp <- 'câmara dos deputados'
  senado_exp <- 'senado federal'

  url_base_autores <- 'https://dadosabertos.camara.leg.br/api/v2/proposicoes/'
  url <- paste0(url_base_autores, prop_id, '/autores')
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
        stringr::str_detect(tolower(autor.nome), camara_exp) | autor.tipo == 'Deputado' ~ 'Câmara dos Deputados',
        stringr::str_detect(tolower(autor.nome), senado_exp) | autor.tipo == 'Senador' ~ 'Senado Federal',
        autor.cod_tipo == 40000 ~ 'Senado Federal',
        autor.cod_tipo == 2 ~ 'Câmara dos Deputados'))

  partido_estado <- extract_partido_estado_autor(authors$autor.uri %>% tail(1))

  authors %>%
    dplyr::mutate(autor.nome = paste0(autor.nome, " ", partido_estado))
}

fetch_proposicao_with_apensamentos <- function(prop_id) {
  rcongresso::fetch_proposicao(prop_id) %>%
    dplyr::mutate(proposicoes_apensadas = paste(fetch_apensadas(prop_id), collapse=' '))
}

#' @title Baixa dados de requerimentos relacionados
#' @description Retorna um dataframe contendo dados sobre os requerimentos relacionados a uma proposição
#' @param prop_id ID de uma proposição
#' @return Dataframe
#' @examples
#' fetch_releated_requerimentos(2056568)
#' @export
fetch_related_requerimentos <- function(id, mark_deferimento=T) {
  regexes <-
    tibble::frame_data(~ deferimento, ~ regex,
               'indeferido', '^Indefiro',
               'deferido', '^(Defiro)|(Aprovado)')

  related <-
    rcongresso::fetch_relacionadas(id)$uri %>%
    strsplit('/') %>%
    vapply(last, '') %>%
    unique %>%
    rcongresso::fetch_proposicao()

  requerimentos <-
    related %>%
    dplyr::filter(stringr::str_detect(.$siglaTipo, '^REQ'))

  if(!mark_deferimento) return(requerimentos)

  tramitacoes <-
    requerimentos$id %>%
    rcongresso::fetch_tramitacao()

  related <-
    tramitacoes %>%
    # mark tramitacoes rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by=c(despacho='regex')) %>%
    dplyr::group_by(id_prop) %>%
    # fill down marks
    tidyr::fill(deferimento) %>%
    # get last mark on each tramitacao
    dplyr::do(tail(., n=1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_prop, deferimento) %>%
    # and mark proposicoes based on last tramitacao mark
    dplyr::left_join(related, by=c('id_prop' = 'id'))
}

#' @title Recupera os últimos eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' get_latest_events(2121442)
#' @export
get_latest_events <- function(prop_id) {
  fetch_events(prop_id) %>%
    dplyr::filter(timestamp <= lubridate::now())
}

#' @title Recupera os próximos eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' get_next_events(2121442)
#' @export
get_next_events <- function(prop_id) {
  fetch_events(prop_id) %>%
    dplyr::filter(timestamp > lubridate::now())
}

#' @title Recupera os locais da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada local
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada local
#' @examples
#'  extract_locais_in_camara(fetch_tramitacao(91341))
#' @export
extract_locais_in_camara <- function(df) {
  descricoes_plenario <- c('votação', 'pronta para pauta', 'apresentação de proposição', 'sessão deliberativa')
  descricoes_comissoes <- c('recebimento pela')

  df %<>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(
          (tolower(despacho) %in% descricoes_plenario & sigla_orgao == 'PLEN' |
            stringr::str_detect(tolower(descricao_tramitacao), '^votação')) ~ 'Plenário',
          (stringr::str_detect(tolower(despacho), '^recebimento pela') |
            tolower(despacho) %in% descricoes_comissoes) & 
            sigla_orgao != 'CCP' &
            !stringr::str_detect(tolower(sigla_orgao), '^s') ~ sigla_orgao,
          tolower(descricao_tramitacao) == 'remessa ao senado federal' ~ 'Câmara')
    ) %>%
    dplyr::mutate(
      local =
        dplyr::case_when(stringr::str_detect(local, "^PL") ~ "Comissão Especial",
                  TRUE ~ local)
    )

  if (is.na(df[1, ]$local)) {
    df[1, ]$local = 'CD-MESA-PLEN'
  }
  
  df %>%
    tidyr::fill(local)
}

#' @title Recupera as casas da Câmara
#' @description Retorna o dataframe da tamitação contendo mais uma coluna chamada casa
#' @param df Dataframe da tramitação na Câmara
#' @return Dataframe da tramitacao contendo mais uma coluna chamada casa
#' @examples
#'  extract_fase_casa_in_camara(fetch_tramitacao(91341))
#' @export
extract_fase_casa_in_camara <- function(df) {
  descricoes_plenario <- c('votação', 'pronta para pauta', 'apresentação de proposição', 'sessão deliberativa')
  descricoes_comissoes <- c('recebimento pela')
  
  df <- df %>%
    dplyr::arrange(data_hora, sequencia) %>%
    dplyr::mutate(
      casa =
        dplyr::case_when(
          (tolower(despacho) %in% descricoes_plenario & sigla_orgao == 'PLEN' |
             stringr::str_detect(tolower(descricao_tramitacao), '^votação')) ~ 'Plenário',
          (stringr::str_detect(tolower(despacho), '^recebimento pela') | 
             tolower(despacho) %in% descricoes_comissoes) & sigla_orgao != 'CCP' & !stringr::str_detect(tolower(sigla_orgao), '^s') ~ "Comissões")
    )
  
  
  if (is.na(df[1, ]$casa)) {
    df[1, ]$casa = 'Apresentação'
  }
  
  df %>%
    tidyr::fill(casa)
}

# Extrai a situação das comissões (ex. Recebimento, Análise do Relator, Discussão e votação...)
# Necessita do dataframe da tramitação
extract_situacao_comissao <- function(df) {
  recebimento <- camara_codes$situacao_comissao$code$recebimento
  analise_do_relator <- camara_codes$situacao_comissao$code$analise_do_relator
  discussao_votacao <- camara_codes$situacao_comissao$code$discussao_votacao
  encaminhamento <- camara_codes$situacao_comissao$code$encaminhamento

  reg <-
    unlist(get_comissoes_camara()) %>%
    paste(collapse='|') %>%
    regex(ignore_case=TRUE)

  df %>%
    dplyr::mutate(
      comissao =
        dplyr::case_when(str_detect(local, reg) ~ str_extract(local, reg))
    ) %>%
    # dplyr::filter(!is.na(comissao)) %>%
    dplyr::mutate(
      situacao_comissao =
        dplyr::case_when(id_tipo_tramitacao %in% recebimento & !is.na(comissao) ~ "Recebimento",
                         id_tipo_tramitacao %in% analise_do_relator & !is.na(comissao) ~ "Análise do relator",
                         id_tipo_tramitacao %in% discussao_votacao& !is.na(comissao)  ~ "Discussão e votação",
                         id_tipo_tramitacao %in% encaminhamento & !is.na(comissao) ~ "Encaminhamento")
    ) %>%
    dplyr::select(-comissao) %>%
    tidyr::fill(situacao_comissao) %>%
    dplyr::mutate(situacao_comissao = dplyr::case_when(stringr::str_detect(local, reg) ~ situacao_comissao))
                    # dplyr::case_when(!stringr::str_detect(local, reg)) ~ "NA",
                    #                                    TRUE ~ situacao_comissao)

}
