library(tidyverse)
source(here::here("scripts/proposicoes/fetcher_proposicao.R"))

#' @title Filtra as proposições pelo texto da coluna Urgência
#' @description Recebe um dataframe e o regime de tramitação de interesse e
#' retorna as linhas que possuem este regime.
#' @param df Dataframe a ser filtrado
#' @return Dataframe filtrado pelo regime de tramitação
.filter_proposicoes_by_texto_urgencia <- function(df) {
  urgencia_texto_regex <-
    "(urgência (– (despacho|requerimento)|urgentíssima)|transformad.* lei.* 14006/2020|requerimento pendente de aprovação)"

  return(df %>%
           filter(str_detect(
             tolower(urgencia), urgencia_texto_regex
           )))
}

#' @title Filtra as proposições pelo regime de tramitação
#' @description Recebe um dataframe e o regime de tramitação de interesse e
#' retorna as linhas que possuem este regime.
#' @param df Dataframe a ser filtrado
#' @param regime Nome do regime de tramitação de interesse
#' @return Dataframe filtrado pelo regime de tramitação
.filter_proposicoes_by_regime_tramitacao <- function(df, regime) {
  return(df %>%
           filter(regime_tramitacao == regime))
}

#' @title Mapeia o id da proposição
#' @description Se o link existir, o id será extraído dele. Caso contrário,
#' será feita uma requisição a partir do nome e casa.
#' @param link_casa Link para a proposição
#' @param nome nome formal da proposição
#' @param casa casa da proposiçãõ
#' @return Id da proposição
.fetch_id <- function(link_casa, nome, casa) {
  if (!is.na(link_casa)) {
    return(extract_id_from_link(link_casa))
  } else {
    return(as.character(fetch_id_by_nome_formal(nome, casa)))
  }
}

#' @title Recupera os 3 temas mais relevantes de uma proposição
#' @description Recebe um id e casa e retorna os 3 temas mais relevantes de
#' uma proposição separados por ';'.
#' @param id id da proposição
#' @param casa camara ou senado
#' @return Temas da proposição separados por ';'.
.get_temas <- function(id, casa) {
  print(paste0("Extraindo temas da proposição ", id, " na casa ", casa, "..."))

  df <- rcongresso::fetch_temas_proposicao(id, casa)

  if (!is.null(df) & nrow(df) > 0) {
    temas <- df %>% arrange(desc(relevancia)) %>%
      head(3) %>%
      pull(tema)

    temas <- paste(temas, collapse = ";")

    if (temas == "Nao especificado") {
      temas <- as.character(NA)
    }

    return(temas)
  } else {
    return(as.character(NA))
  }
}

#' @title Processa dataframe intermediário de proposições em
#' formato esperado pelo Leggo
#' @description Recebe um dataframe de proposições processados pela função
#' mapeia_ids_proposicoes() e processa-o no formato aceito pelo leggo
#' @param proposicoes Dataframe contendo colunas id, casa, nome e ementa
#' @return Dataframe de proposições formatados.
.formata_dataframe_proposicoes <- function(proposicoes) {
  new_df <- tribble(
    ~ proposicao,
    ~ id_camara,
    ~ id_senado,
    ~ apelido,
    ~ tema,
    ~ advocacy_link,
    ~ keywords,
    ~ tipo_agenda,
    ~ explicacao_projeto
  )

  proposicoes <- proposicoes %>%
    select(id,
           proposicao = nome,
           casa,
           explicacao_projeto = ementa)

  new_df <- new_df %>%
    bind_rows(proposicoes) %>%
    mutate(
      id_camara = if_else(casa == "camara", id, as.character(NA)),
      id_senado = if_else(casa == "senado", id, as.character(NA))
    ) %>%
    select(-c(id, casa))

  return(new_df)

}

#' @title Mapeia ids das proposições a partir do nome formal
#' ou link da proposição
#' @description Recebe uma url para um csv e retorna esse mesmo csv
#' adicionada uma coluna id correspondente
#' @param url URL do dataframe de proposições. Deve conter as colunas
#' nome, casa, link_casa
#' @param filter_by_regime_tramitacao Flag indicando se as proposições devem
#' ser filtradas pelo regime de tramitação ou não.
#' @return Mesmo csv de entrada contendo nova coluna id da proposição.
.mapeia_ids_proposicoes <-
  function(url, filter_by_regime_tramitacao = T) {
    source(here::here("scripts/proposicoes/fetcher_proposicao.R"))

    proposicoes <- read_csv(url) %>%
      agoradigital::rename_table_to_underscore()

    names(proposicoes) <- names(proposicoes) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      gsub(" ", "_", .)

    if (filter_by_regime_tramitacao) {
      proposicoes <- proposicoes %>%
        select(nome,
               casa,
               link_casa,
               regime_tramitacao = regime_de_tramitacao,
               ementa) %>%
        .filter_proposicoes_by_regime_tramitacao("URGENTE")
    } else {
      proposicoes <- proposicoes %>%
        select(nome,
               casa,
               link_casa,
               urgencia,
               ementa) %>%
        .filter_proposicoes_by_texto_urgencia()
    }

    proposicoes_com_id <- proposicoes %>%
      rowwise(.) %>%
      mutate(id = .fetch_id(link_casa, nome, casa)) %>%
      mutate(casa = .process_casa(casa))

    return(proposicoes_com_id)

  }

#' @title Recupera os temas das proposições já processadas
#' @description Recebe o id_camara e id_senado e recupera o tema preenchido pela
#' Câmara (caso exista o id) ou no Senado.
#' @param id_camara Id da proposição na Câmara
#' @param id_senado Id da proposição no Senado
#' @return Os temas de uma proposição.
.get_temas_processed_proposicoes <- function(id_camara, id_senado) {
  temas <- tryCatch({
    if (!is.na(as.numeric(id_camara))) {
      return(.get_temas(id_camara, "camara"))

    } else if (!is.na(as.numeric(id_senado))) {
      return(.get_temas(id_senado, "senado"))

    }
  }, error = function(e) {
    print(e)
    return (as.character(NA))
  })
  if (is.null(temas)) {
    return (as.character(NA))
  }
  return (temas)
}

#' @title Recupera as ementas das proposições já processadas
#' @description Recebe o id_camara e id_senado e recupera a ementa
#' @param id_camara Id da proposição na Câmara
#' @param id_senado Id da proposição no Senado
#' @return A ementa de uma proposição.
.fetch_ementa <- function(id_camara, id_senado) {
  print(
    paste0(
      "Extraindo ementa da proposição id_camara ",
      id_camara,
      " e id_senado ",
      id_senado,
      "..."
    )
  )

  if (!is.na(as.numeric(id_camara))) {
    return(rcongresso::fetch_proposicao_camara(id_camara) %>%
             pull(ementa))

  } else if (!is.na(as.numeric(id_senado))) {
    return(rcongresso::fetch_proposicao_senado(id_senado) %>%
             pull(ementa_materia))

  }
  return (as.character(NA))

}

#' @title Processa o nome formal das proposições
#' @description Recebe um nome formal e retira a sigla da casa, caso exista.
#' @param nome_formal Nome formal da proposição
#' @return Nome formal processado.
.processa_nome_formal <- function(nome_formal) {
  source(here::here("scripts/proposicoes/fetcher_proposicao.R"))

  campos <- .process_inputs(nome_formal)

  nome_processado <-
    paste0(campos$sigla, " ", campos$numero, "/", campos$ano)

  return(nome_processado)
}

#' @title Recupera o nome formal de uma proposição
#' @description Recebe o id_camara e id_senado e recupera o nome formal
#' @param id_camara Id da proposição na Câmara
#' @param id_senado Id da proposição no Senado
#' @return O nome formal de uma proposição.
.fetch_nome_formal <- function(id_camara, id_senado) {
  print(
    paste0(
      "Extraindo nome formal da proposição id_camara ",
      id_camara,
      " e id_senado ",
      id_senado,
      "..."
    )
  )

  nome <- tryCatch({
    if (!is.na(as.numeric(id_camara))) {
      return(
        rcongresso::fetch_proposicao_camara(id_camara) %>%
          mutate(nome_formal = paste0(siglaTipo, " ", numero, "/", ano)) %>%
          pull(nome_formal)
      )

    } else if (!is.na(as.numeric(id_senado))) {
      return(
        rcongresso::fetch_proposicao_senado(id_senado) %>%
          pull(descricao_identificacao_materia)
      )

    }
  }, error = function(e) {
    print(e)
    return (as.character(NA))
  })

  return(nome)

}

#' @title Filtra as proposições com todas as informações necessárias
#' @description Recebe um dataframe e retorna as proposições que possuem
#' todos os campos preenchidos e processados.
#' @param df Dataframe a ser filtrado
#' @return Dataframe com proposições filtradas.
.filtra_proposicoes_com_todas_as_infos <- function(df) {
  return(df %>%
           filter(
             !is.na(tema),
             !str_detect(tolower(proposicao), "^(cn|sf|cd) .*"),
             (str_detect(tolower(proposicao), "mpv.*") & !is.na(id_camara) & !is.na(id_senado)) |
             (!str_detect(tolower(proposicao), "mpv.*") & (!is.na(id_camara) | !is.na(id_senado)))

           ))
}

#' @title Adiciona informações faltantes às proposições
#' @description Recebe um dataframe e retorna as proposições com os dados
#' processados, como tema, nome formal ou explicação do projeto
#' @param df Dataframe com as proposições
#' @return Campos preenchidos das proposições.
.preenche_proposicoes_info <- function(df) {
  df_com_infos <- df %>%
    filter(
      is.na(tema) |
        str_detect(tolower(proposicao), "^(cn|sf|cd) .*") |
        is.na(id_camara) & is.na(id_senado) |
        str_detect(tolower(proposicao), "mpv.*") & (is.na(id_camara) | (is.na(id_senado)))
    ) %>%
    rowwise(.) %>%
    mutate(
      proposicao = ifelse(
        is.na(proposicao),
        .fetch_nome_formal(id_camara, id_senado),
        .processa_nome_formal(proposicao)
      ),
      proposicao = as.character(proposicao)
    )

  df_com_ids <- df_com_infos %>%
    filter(is.na(id_camara) & is.na(id_senado) |
             (str_detect(tolower(proposicao), "mpv.*") & (is.na(id_camara) | (is.na(id_senado))))) %>%
    rowwise(.) %>%
    mutate(
      id_camara = ifelse(
        !is.na(id_camara),
        id_camara,
        .fetch_id(
          link_casa = NA,
          nome = proposicao,
          casa = "camara"
        )
      ),
      id_senado = ifelse(
        !is.na(id_senado),
        id_senado,
        .fetch_id(
          link_casa = NA,
          nome = proposicao,
          casa = "senado"
        )
      )
    ) %>%
    ungroup() %>%
    mutate(id_camara = as.character(id_camara),
           id_senado = as.character(id_senado))

  df_com_infos <- df_com_infos %>%
    filter(!str_detect(tolower(proposicao), "mpv.*") & (!is.na(id_camara) | !is.na(id_senado)) |
             (str_detect(tolower(proposicao), "mpv.*") & !is.na(id_camara) & !is.na(id_senado))) %>%
    bind_rows(df_com_ids) %>%
    rowwise(.) %>%
    mutate(
      tema = ifelse(
        !is.na(tema),
        tema,
        .get_temas_processed_proposicoes(id_camara, id_senado)
      ),
      tema = as.character(tema)
    ) %>%
    ungroup()

  return(df_com_infos)
}

#' @title Adiciona informações às proposições
#' @description Recebe um dataframe e preenche os dados que
#' estão faltando.
#' @param df Dataframe com as proposições
#' @return Campos preenchidos das proposições.
.checa_proposicoes_infos <- function(df) {
  if (!"proposicao" %in% names(df)) {
    col_names <- names(df)
    df$proposicao <- NA
    df <- df %>% select(proposicao, tidyselect::all_of(col_names))
  } else {
    df <- df %>%
      mutate(proposicao = str_remove(proposicao, "\\.")) # Retira ponto dos números das proposições
  }

  proposicoes_with_infos <- df %>%
    .filtra_proposicoes_com_todas_as_infos()

  proposicoes_with_new_infos <- df %>%
    .preenche_proposicoes_info()

  proposicoes <- bind_rows(proposicoes_with_infos,
                           proposicoes_with_new_infos) %>%
    distinct()

  return(proposicoes)
}

#' @title Une duas planilhas a partir de suas urls
#' @description Recebe a url das proposições novas a serem adicionadas, executa o
#' processamento e as unem às proposições já existentes a partir de sua url.
#' @param raw_proposicao_url_camara URL do csv das proposições a serem processadas e
#' adicionadas da Câmara
#' @param raw_proposicao_url_senado URL do csv das proposições a serem processadas e
#' adicionadas do Senado
#' @param processed_proposicao_url URL do csv das proposições já existentes.
#' @param filter_by_regime_tramitacao Flag indicando se as proposições devem
#' ser filtradas pelo regime de tramitação ou não.
#' @return Um só dataframe contendo todas as proposições, novas e existentes.
processa_planilha_proposicoes <- function(raw_proposicao_url_camara,
                                          raw_proposicao_url_senado,
                                          processed_proposicao_url,
                                          filter_by_regime_tramitacao = F) {
  old_proposicoes <-
    read_csv(processed_proposicao_url, col_types = cols(.default = "c"))

  new_proposicoes <-
    bind_rows(
      .mapeia_ids_proposicoes(raw_proposicao_url_camara, filter_by_regime_tramitacao),
      .mapeia_ids_proposicoes(raw_proposicao_url_senado, filter_by_regime_tramitacao)
    ) %>%
    .formata_dataframe_proposicoes()

  merged_proposicoes <-
    bind_rows(old_proposicoes, new_proposicoes) %>%
    distinct(id_camara, id_senado, .keep_all = T) %>%
    .checa_proposicoes_infos()

  return(merged_proposicoes)
}
