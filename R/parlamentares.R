#' @title Faz o merge das relatorias pelo nome, casa e uf do relator
#' @description Realiza o merge das tabelas parlamentares e proposições,
#' recuperando o id do relator através do join de nome, casa, legislatura
#' e uf do relator.
#' @param proposicoes_df Dataframe de proposições
#' @param parlamentares_df Dataframe de parlamentares
#' @return Dataframe de proposições contendo o relator_id completo
#' e relator_nome processado.
.merge_by_nome_casa_uf <-
  function(proposicoes_df, parlamentares_df) {
    proposicoes_df <- proposicoes_df %>%
      filter(is.na(relator_id))

    proposicoes_df <- proposicoes_df %>%
      mutate(nome_relator_padronizado = .padroniza_nome(relator_nome)) %>%
      left_join(
        parlamentares_df,
        by = c(
          "nome_relator_padronizado" = "nome_parlamentar",
          "casa",
          "relator_uf" = "uf",
          "legislatura"
        )
      ) %>%
      mutate(
        relator_id = id_parlamentar,
        relator_nome = nome_relator_padronizado,
        relator_partido = partido
      ) %>%
      select(-c(id_parlamentar, nome_relator_padronizado, partido))

    return(proposicoes_df)
  }

#' @title Faz o merge das relatorias pelo nome e casa do relator
#' @description Realiza o merge das tabelas parlamentares e proposições,
#' recuperando o id do relator através do join de nome,
#' legislatura e casa do relator.
#' @param proposicoes_df Dataframe de proposições
#' @param parlamentares_df Dataframe de parlamentares
#' @return Dataframe de proposições contendo o relator_id completo
#' e relator_nome processado.
.merge_by_nome_casa <- function(proposicoes_df, parlamentares_df) {
  proposicoes_df <- proposicoes_df %>%
    filter(is.na(relator_id)) %>%
    left_join(
      parlamentares_df,
      by = c("relator_nome" = "nome_parlamentar",
             "legislatura",
             "casa"),
      ignore_case = TRUE
    ) %>%
    mutate(
      relator_id = id_parlamentar,
      relator_partido = partido,
      relator_uf = uf
    ) %>%
    select(-c(id_parlamentar, partido, uf, legislatura))

  return(proposicoes_df)
}

#' @title Recupera o id do relator a partir de suas informações
#' @description Realiza o merge das tabelas parlamentares e proposições,
#' recuperando o id do relator através do join de suas informações,
#' como nome, casa, partido, uf e legislatura.
#' @param proposicoes_df Dataframe de proposições
#' @param parlamentares_df Dataframe de parlamentares
#' @param info_relatores TRUE se informações como o nome do relator, uf e partido devem ser retornadas
#' (pode existir mais de uma linha linkando relator à proposição). FALSE caso contrário.
#' @return Dataframe de proposições contendo a coluna relator_id completa,
#' o relator_nome padronizado e o relator_id_parlametria.
#' @export
mapeia_nome_relator_para_id <-
  function(proposicoes_df, parlamentares_df, info_relatores = FALSE) {
    library(tidyverse)

    proposicoes_df <- .mapeia_data_para_legislatura(proposicoes_df)

    parlamentares_df <- parlamentares_df %>%
      mutate(nome_parlamentar = .padroniza_nome(nome_eleitoral)) %>%
      select(nome_parlamentar, partido, uf, id_parlamentar, casa, legislatura)

    proposicoes_com_relator_id <- proposicoes_df %>%
      filter(!is.na(relator_id)) %>%
      mutate(relator_id = as.character(relator_id))

    proposicoes_full_merged <-
      .merge_by_nome_casa_uf(proposicoes_df, parlamentares_df)

    proposicoes_merged_by_nome_casa <-
      .merge_by_nome_casa(proposicoes_full_merged, parlamentares_df)

    proposicoes_alt <-
      proposicoes_full_merged %>%
      filter(!is.na(relator_id)) %>%
      bind_rows(proposicoes_com_relator_id,
                proposicoes_merged_by_nome_casa) %>%
      mutate(enum_casa = if_else(casa == "camara", 1, 2)) %>%
      mutate(relator_id_parlametria = if_else(
        !is.na(relator_id),
        paste0(enum_casa, relator_id),
        relator_id
      ))

    if (!info_relatores) {
      proposicoes_alt <- proposicoes_alt %>%
        select(
          id_ext,
          sigla_tipo,
          numero,
          ementa,
          data_apresentacao,
          casa,
          casa_origem,
          autor_nome,
          autor_uf,
          autor_partido,
          regime_tramitacao,
          forma_apreciacao,
          relator_id,
          relator_id_parlametria,
          id_leggo,
          uri_prop_principal,
          sigla
        ) %>%
        distinct()
    }

    return(proposicoes_alt)
  }

#' @title Padroniza o nome do parlamentar
#' @description Realiza o processamento no nome do relator, retirando
#' Dep. ou Sen., informações sobre partido e uf, espaços desnecessários e
#' acentos.
#' @param nome Nome a ser processado
#' @return Nome processado
.padroniza_nome <- function(nome) {
  nome_processado <- str_to_title(nome) %>%
    str_remove("Dep. |Sen. ") %>%
    str_remove(" \\(.*") %>%
    iconv(., from="UTF-8", to="ASCII//TRANSLIT")

  nome_processado <- if_else(
    str_detect(nome_processado, "/"),
    str_remove(nome_processado, " /.*") %>%
      str_remove(" [^ ]+$"),
    nome_processado
  )

  # Trata caso de Evandro Roman (que possui nome eleitoral Roman)
  nome_processado <- if_else(
    str_detect(nome_processado, "Evandro Roman"),
    "Roman",
    nome_processado
  )

  # Trata erro de digitação em nome de deputado
  nome_processado <- if_else(
    str_detect(nome_processado, "Lucas Vergiio"),
    "Lucas Vergilio",
    nome_processado
  )

  return(nome_processado)
}

#' @title Padroniza e retorna os dados de todos os parlamentares
#' @description Une os dados dos deputados e senadores em um dataframe único
#' com as colunas padronizadas.
#' @param export_path Caminho da pasta onde estão os diretórios camara/ e senado/,
#' que possuem os dataframes parlamentares.csv
#' @return Dataframe de parlamentares tanto da câmara quanto do senado.
.bind_parlamentares <- function(export_path) {
  library(tidyverse)
  if (!str_detect(export_path, "\\/$")) {
    export_path <- paste0(export_path, "/")
  }

  deputados <-
    read_csv(paste0(export_path, "camara/parlamentares.csv"),
             col_types = cols(.default = "c")) %>%
    mutate(casa = "camara") %>%
    select(
      casa,
      id_parlamentar = id,
      nome_completo = nome_civil,
      nome_eleitoral = ultimo_status_nome_eleitoral,
      genero = sexo,
      partido = ultimo_status_sigla_partido,
      uf = ultimo_status_sigla_uf
    )

  senadores <-
    read_csv(paste0(export_path, "senado/parlamentares.csv"),
             col_types = cols(.default = "c"))

  parlamentares <- bind_rows(deputados, senadores)

  return(parlamentares)
}

#' @title Mapeia as datas das designações de relator para legislatura
#' @description Mapeia as datas das designações de relator para a respectiva
#' legislatura.
#' @param proposicoes_df Dataframe de proposições
#' @return Dataframe de proposições contendo a legislatura
.mapeia_data_para_legislatura <- function(proposicoes_df) {
  legislaturas <-
    (jsonlite::fromJSON(here::here(
      "R/config/environment_congresso.json"
    )))$legislaturas

  df <- fuzzyjoin::fuzzy_left_join(
    proposicoes_df, legislaturas,
    by = c(
      "relator_data" = "data_inicio",
      "relator_data" = "data_fim"
    ),
    match_fun = list(`>=`, `<=`)
  )

  df <- df %>%
    select(-c(relator_data, data_inicio, data_fim))

  return(df)
}

#' @title Lê e retorna dataframe com os dados de parlamentares já processados
#' @description Faz a leitura do csv de parlamentares e retorna o dataframe correspondente
#' @param export_path Pasta que contém o csv de parlamentares
#' @return Dataframe de parlamentares
#' @export
read_parlamentares <- function(export_path) {
  parlamentares <- tryCatch({
    export_path_parlamentares <- export_path

    if (!stringr::str_detect(export_path, "\\/$")) {
      export_path_parlamentares <- paste0(export_path, "/")
    }

    readr::read_csv(
      paste0(export_path_parlamentares, "parlamentares.csv"),
      col_types = readr::cols("legislatura" = "i",
                              .default = "c")
    )
  },
  error = function(msg) {
    print("Erro ao importar dados de parlamentares")
    print(msg)
    return(
      tibble::tribble(
        ~ id_parlamentar,
        ~ id_parlamentar_parlametria,
        ~ casa,
        ~ nome_eleitoral,
        ~ nome_civil,
        ~ cpf,
        ~ sexo,
        ~ partido,
        ~ uf,
        ~ situacao,
        ~ em_exercicio
      )
    )
  })
}
