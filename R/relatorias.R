senado_env <-
  jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
senado_constants <- senado_env$constants

#' @title Recupera o histórico de relatorias de uma proposição
#' @description Retorna dataframe com o histórico de relatorias, contendo data e nome do relator. No senado contem informacoes
#' extras sobre motivo do fim da relatoria e informacoes do relator. Na camara, contem o a sigla do local da relatoria
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @param last_n Pegar os ultimos N relatores
#' @return Dataframe com as informações detalhadas do histórico de relatorias de uma proposição no Senado
#' @examples get_relatorias(91341, 'senado', 3)
#' @export
get_relatorias <- function(proposicao_id, casa, last_n = NULL) {
  relatorias <- data.frame()
  if (tolower(casa) == 'senado') {
    relatorias <- extract_relatorias_senado(proposicao_id)
  }
  else if (tolower(casa) == 'camara') {
    relatorias <- extract_relatorias_camara(proposicao_id)
  }

  if (!is.null(last_n)) {
    relatorias <-
      relatorias %>%
      head(last_n)
  }
  return(relatorias)
}


#' @title Recupera o histórico de relatorias de uma proposição no Senado na api do senado
#' @description Retorna dataframe com o histórico de relatorias detalhado de uma proposição no Senado, incluindo a data
#' de designação e destituição, o relator e seu partido e a comissão.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações detalhadas do histórico de relatorias de uma proposição no Senado
#' @examples fetch_relatorias_senado(91341)
#' @export
fetch_relatorias_senado <-
  function(proposicao_id, ultimo_relator = FALSE) {
    url_relatorias <-
      paste0(senado_env$endpoints_api$url_base, "relatorias/")

    url <- paste0(url_relatorias, proposicao_id)
    json_relatorias <- jsonlite::fromJSON(url, flatten = T)

    relatorias <-
      json_relatorias %>%
      magrittr::extract2("RelatoriaMateria") %>%
      magrittr::extract2("Materia")

    relatorias_data <- NULL
    if (ultimo_relator) {
      relatorias_data <-
        relatorias %>%
        magrittr::extract2("RelatoriaAtual")
    }
    if (is.null(relatorias_data)) {
      relatorias_data <-
        relatorias %>%
        magrittr::extract2("HistoricoRelatoria")
    }

    relatorias_df <-
      relatorias_data %>%
      magrittr::extract2("Relator") %>%
      as.data.frame() %>%
      purrr::map_df( ~ .) %>%
      tidyr::unnest()
  }

#' @title Recupera o histórico de relatorias de uma proposição no Senado
#' @description Retorna dataframe com o histórico de relatorias detalhado de uma proposição no Senado, incluindo a data
#' de designação e destituição, o relator e seu partido e a comissão.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição do Senado
#' @return Dataframe com as informações detalhadas do histórico de relatorias de uma proposição no Senado
extract_relatorias_senado <- function(proposicao_id) {
  relatorias <- fetch_relatorias_senado(proposicao_id, T)

  relatorias <-
    relatorias[,!sapply(relatorias, is.list)] %>%
    rename_relatorias_senado_columns
}

#' @title Renomeia as colunas do dataframe do histórico de relatorias no Senado
#' @description Renomeia as colunas do dataframe do histórico de relatorias no Senado usando o padrão
#' de underscore e letras minúsculas
#' @param df Dataframe do histórico de relatorias
#' @return Dataframe com as colunas renomeadas
rename_relatorias_senado_columns <- function(df) {
  new_names = names(df) %>%
    to_underscore() %>%
    stringr::str_replace("identificacao_parlamentar_|identificacao_comissao_", "")
  names(df) <- new_names
  df
}

#' @title Recupera o histórico de relatorias de uma proposição na Camara
#' @description Retorna dataframe com o histórico de relatorias detalhado de uma proposição na Camara, incluindo a data
#' de designação e destituição, o relator e seu partido e a comissão.
#' Ao fim, a função retira todos as colunas que tenham tipo lista para uniformizar o dataframe.
#' @param proposicao_id ID de uma proposição da Camara
#' @return Dataframe com as informações detalhadas do histórico de relatorias de uma proposição na Camara
extract_relatorias_camara <- function(proposicao_id) {
  fetch_tramitacao(proposicao_id, 'camara') %>%
    dplyr::filter(
      stringr::str_detect(tolower(texto_tramitacao), '^designad. relat.r') |
        stringr::str_detect(
          tolower(texto_tramitacao),
          'o relator(.)* deixou de ser membro da comiss.o'
        )
    ) %>%
    dplyr::select(data_hora,
                  texto_tramitacao,
                  sigla_local) %>%
    tibble::add_column() %>%
    dplyr::mutate(
      nome_parlamentar = dplyr::if_else(
        stringr::str_detect(tolower(texto_tramitacao), '^designad. relat.r'),
        stringr::str_extract(
          texto_tramitacao,
          stringr::regex('dep.?([^,]*)', ignore_case = TRUE)
        ),
        "Relator não encontrado"
      ),
      partido =
        dplyr::if_else(
          stringr::str_detect(tolower(texto_tramitacao), '^designad. relat.r'),
          stringr::str_match(texto_tramitacao, '[(](.*?)[)]')[, 2],
          ""
        )
    ) %>%
    dplyr::select(-c(texto_tramitacao)) %>%
    dplyr::arrange(desc(data_hora))
}

#' @title Retorna o ultimo relator
#' @description Recebe id da proposicao e sua casa e retorna o ultimo relator registrado
#' @param proposicao_id ID de uma proposição da Camara
#' @param casa casa da proposicao
#' @return Dataframe com dados do relator
#' @examples get_last_relator(91341, 'senado')
#' @export
get_last_relator <- function(proposicao_id, casa) {
  df_relator <- tibble::tribble(~ id_relator, ~ nome_relator,  ~partido_relator, ~ uf_relator, ~ data_relator)
  relatorias <- agoradigital::get_relatorias(proposicao_id, casa, 1)

  if (nrow(relatorias) == 0) {
    return(df_relator)
  }

  if ("nome_parlamentar" %in% colnames(relatorias)) {
    if (is.na(relatorias$nome_parlamentar) | relatorias$nome_parlamentar == "Relator não encontrado") {
      return(df_relator)
    }
  }

  if (casa == "senado") {
    df_relator <- relatorias %>%
      dplyr::mutate(data_relator = as.POSIXct(data_designacao),
                    id_relator = as.character(codigo_parlamentar)) %>%
      dplyr::select(
        id_relator,
        nome_relator = nome_parlamentar,
        partido_relator = sigla_partido_parlamentar,
        uf_relator = uf_parlamentar,
        data_relator
      )
  } else if (casa == "camara") {
    partido_uf <- (relatorias$partido %>% stringr::str_split("-|/"))[[1]]

    if (all(is.na(partido_uf))) {
      partido_relator <- NA
      uf_relator <- NA
    } else {
      partido_relator <- partido_uf[[1]]
      uf_relator <- partido_uf[[2]]
    }

    df_relator <- relatorias %>%
      dplyr::mutate(
        id_relator = NA_character_,
        partido_relator = partido_relator,
        uf_relator = uf_relator,
        nome_parlamentar = stringr::str_remove(nome_parlamentar, " \\(.*")
      ) %>%
      dplyr::select(id_relator, nome_relator = nome_parlamentar, partido_relator, uf_relator, data_relator = data_hora)
  }
  return(df_relator)
}

#' @title Recupera lista de relatores de uma proposição do Leggo (composta por id_camara e id_senado)
#' @description Recebe os ids das proposições na câmara e no senado e retorna a lista de relatores de
#' forma padronizada
#' @param rowid Número da linha que está executando (para controle de execução).
#' @param id_leggo Id da proposição no Leggo
#' @param id_camara Id da proposição na câmara.
#' @param id_senado Id da proposição no senado.
#' @param total_rows Número total de proposição para execução da captura dos relatores (para controle de execução).
#' @return Dataframe com dados do relator
#' @examples .get_relatorias_leggo(1, "d51d8c078dab1676b4c24b67fc66d4dd", 2192459, 137999, 1)
.get_relatorias_leggo <- function(rowid,
                                 id_leggo,
                                 id_camara,
                                 id_senado,
                                 total_rows = 1) {

  library(tidyverse)

  print(
    paste(
      "Recuperando relatores para a proposição",
      id_leggo,
      "( câmara:",
      id_camara,
      "| senado",
      id_senado,
      ") -",
      rowid,
      "/",
      total_rows
    )
  )
  relatores_camara <- tibble()
  if (!is.na(id_camara)) {
    relatores_camara <- tryCatch({
      relatorias <- get_relatorias(proposicao_id = id_camara,
                                  casa = "camara")

      partido_uf <-
        (relatorias$partido %>% stringr::str_split("-|/"))[[1]]

      if (all(is.na(partido_uf))) {
        partido_relator <- NA
        uf_relator <- NA
      }
      else {
        partido_relator <- partido_uf[[1]]
        uf_relator <- partido_uf[[2]]
      }
      df_relator <- relatorias %>% mutate(
        id_relator = NA_character_,
        casa = "camara",
        partido_relator = partido_relator,
        uf_relator = uf_relator,
        nome_parlamentar = str_remove(nome_parlamentar,
                                               " \\(.*")
      ) %>% select(
        id_relator,
        casa,
        nome_relator = nome_parlamentar,
        partido_relator,
        uf_relator,
        data_relator = data_hora
      )
    }, error = function(e) {
      message(e)
      return(
        tibble::tibble(
          id_relator = character(),
          casa = character(),
          nome_relator = character(),
          partido_relator = character(),
          uf_relator = character(),
          data_relator = as.POSIXct(character())
        )
      )
    })
  }

  relatores_senado <- tibble::tibble()
  if (!is.na(id_senado)) {
    relatores_senado <- tryCatch({
      relatorias <- get_relatorias(proposicao_id = id_senado,
                                  casa = "senado")

      if (nrow(relatorias) > 0) {
        df_relator <- relatorias %>%
          mutate(data_relator = as.POSIXct(data_designacao),
                 casa = "senado",
                 id_relator = as.character(codigo_parlamentar)) %>%
          select(
            id_relator,
            casa,
            nome_relator = nome_parlamentar,
            partido_relator = sigla_partido_parlamentar,
            uf_relator = uf_parlamentar,
            data_relator
          )
      } else {
        stop("Nenhum relator encontrado")
      }
    }, error = function(e) {
      message(e)
      return(
        tibble::tibble(
          id_relator = character(),
          casa = character(),
          nome_relator = character(),
          partido_relator = character(),
          uf_relator = character(),
          data_relator = as.POSIXct(character())
        )
      )
    })
  }

  relatores <- relatores_camara %>% bind_rows(relatores_senado) %>%
    mutate(id_leggo = id_leggo,
           id_camara = id_camara,
           id_senado = id_senado) %>%
    select(id_leggo, id_camara, id_senado, id_relator, casa, nome_relator, partido_relator,
           uf_relator, data_relator)

  return(relatores)
}

#' @title Exporta dados dos relatores das proposições (matérias legislativas) monitoradas pelo Leggo
#' @description Exporta para uma pasta o CSV que liga uma proposição aos seus relatores
#' @param pls_ids_filepath Caminho para o csv com os pls para serem capturados.
#' @param proposicoes_filepath Caminho para o csv com as proposições processadas (metadados).
#' @param export_path pasta para onde exportar dados.
#' @return Dataframe com informações do relatores das proposições
#' @export
process_relatores_props <- function(pls_ids_filepath, proposicoes_filepath, export_path) {
  library(tidyverse)

  pls <- read_csv(pls_ids_filepath, col_types = cols(prioridade = "c")) %>%
    rowwise(.) %>%
    mutate(concat_chave_leggo = paste0(id_camara, " ", id_senado)) %>%
    mutate(id_leggo = digest::digest(concat_chave_leggo, algo="md5", serialize=F)) %>%
    select(-concat_chave_leggo) %>%
    rowid_to_column(var = "rowid") %>%
    filter(!is.na(id_camara) | !is.na(id_senado)) %>%
    select(rowid, id_leggo, id_camara, id_senado)

  pls_relatorias <- purrr::pmap_df(
    list(pls$rowid,
         pls$id_leggo,
         pls$id_camara,
         pls$id_senado),
    ~ .get_relatorias_leggo(..1, ..2, ..3, ..4, total_rows = pls %>% nrow())
  )

  pls_camara <- pls_relatorias %>%
    filter(casa == "camara") %>%
    select(id_leggo, id_ext = id_camara, casa, relator_id = id_relator, relator_nome = nome_relator,
           relator_partido = partido_relator,
           relator_uf = uf_relator, relator_data = data_relator)

  pls_senado <- pls_relatorias %>%
    filter(casa == "senado") %>%
    select(id_leggo, id_ext = id_senado, casa, relator_id = id_relator, relator_nome = nome_relator,
           relator_partido = partido_relator,
           relator_uf = uf_relator, relator_data = data_relator)

  pls_merge <- pls_camara %>%
    bind_rows(pls_senado) %>%
    mutate(id_ext = as.character(id_ext))

  proposicoes_capturadas <- readr::read_csv(proposicoes_filepath,
                                            col_types = cols(id_ext = "c")) %>%
  select(id_ext, sigla_tipo, numero, ementa, data_apresentacao, casa, casa_origem,
         regime_tramitacao, forma_apreciacao, id_leggo)

  parlamentares <- agoradigital::read_parlamentares(export_path)

  proposicoes_relatorias <- proposicoes_capturadas %>%
    left_join(pls_merge, by = c("id_ext", "casa", "id_leggo"))

  relatores <- proposicoes_relatorias %>%
    agoradigital::mapeia_nome_relator_para_id(parlamentares, info_relatores = TRUE) %>%
    select(id_leggo, id_ext, casa, relator_id, relator_id_parlametria, relator_nome) %>%
    filter(!is.na(relator_id)) %>%
    distinct()

  return(relatores)
}
