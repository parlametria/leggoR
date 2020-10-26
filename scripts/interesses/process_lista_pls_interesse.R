library(tidyverse)
library(here)

#' @title Processa todas as PLs de diversos interesses e que são analisadas pelo LEGGO
#' @description Junta todos as PL's listadas para os interesses analisados pelo LEGGO
#' @param url URL para lista de interesses do leggo
#' @return Dataframe com as PL's que serviram de entrada para o pipeline
#' contendo id_camara,id_senado,apelido,tema,advocacy_link,keywords,tipo_agenda
#' @example
#' tabela_pls <- processa_lista_pls_interesses(url)
processa_lista_pls_interesses <- function(url) {

  if (is.null(url) | url == "") {
    stop("URL para planilha de interesses precisa ser diferente de vazio e não nula.")
  }

  interesses <- readr::read_csv(url)

  pls_para_analise <- purrr::pmap_dfr(list(interesses$interesse,
                                           interesses$url,
                                           interesses$nome),
                  function(interesse, url, nome) {
                    print(interesse)
                    source(here::here("scripts/proposicoes/process_proposicao.R"))
                    pls <- readr::read_csv(url, col_types = cols(.default = "c")) %>%
                      dplyr::select(proposicao, id_camara, id_senado, apelido, tema, advocacy_link,
                                    keywords, tipo_agenda, explicacao_projeto, dplyr::contains("prioridade")) %>%
                      .checa_proposicoes_infos() %>%
                      dplyr::mutate(interesse = interesse) %>%
                      dplyr::mutate(nome_interesse = nome)
                    return(pls)
                  })

  return(pls_para_analise)
}

#' @title Processa temas
#' @description Realiza o processamento de temas, criando um slug sem acentos, espaços, etc.
#' @param tema Tema a ser processado
#' @return Tema processado, sem acentos ou espaços.
#' @example
#' tema_slug <- .processa_tema("Primeira infância; Educação")
.processa_tema <- function(tema) {
  tema_processado <- tema %>%
    tolower() %>%
    iconv(., from="UTF-8", to="ASCII//TRANSLIT") %>%
    gsub(x = ., " ", "-")

  return(tema_processado)
}

#' @title Mapeia pls e interesses analisados pelo Leggo
#' @description Realiza o mapeamento entre pls e interesses analisados pelo Leggo
#' @param url URL para lista de interesses do leggo
#' @param proposicoes_filepath Caminho para o arquivo de proposições processadas
#' @return Dataframe com o mapeamento entre pls e interesses
#' contendo id_ext, casa, id_leggo, interesse
#' @example
#' interesses <- processa_interesses_leggo(url, proposicoes_filepath)
processa_interesses_leggo <- function(url, proposicoes_filepath) {
  colunas <- c("interesse", "nome_interesse", "apelido", "tema",
               "tema_slug", "advocacy_link", "keywords", "tipo_agenda")

  pls_interesse <- processa_lista_pls_interesses(url) %>%
    dplyr::mutate(tema = trimws(tema, which = "both")) %>%
    dplyr::mutate(tema = gsub(pattern = "; ", replacement = ";", x = tema)) %>%
    dplyr::mutate(tema = dplyr::case_when(
      is.na(prioridade) ~ tema,
      str_detect(prioridade, "^priorit.rio") & !is.na(tema) ~ paste(tema, "Prioritário", sep = ";"),
      str_detect(prioridade, "^priorit.rio") & is.na(tema) ~ "Prioritário",
      is.na(tema) ~ "Não especificado",
      TRUE ~ tema
    )) %>%
    dplyr::mutate(tema_slug = .processa_tema(tema)) %>%
    dplyr::select(id_camara, id_senado, tidyselect::all_of(colunas))

  pls_interesse_camara <- pls_interesse %>%
    dplyr::mutate(id_ext = id_camara) %>%
    dplyr::filter(!is.na(id_ext)) %>%
    dplyr::select(id_ext, tidyselect::all_of(colunas))

  pls_interesse_senado <- pls_interesse %>%
    dplyr::mutate(id_ext = id_senado) %>%
    dplyr::filter(!is.na(id_ext)) %>%
    dplyr::select(id_ext, tidyselect::all_of(colunas))

  pls_interesse_processed <- pls_interesse_camara %>%
    dplyr::bind_rows(pls_interesse_senado)

  proposicoes_capturadas <- readr::read_csv(proposicoes_filepath,
                                            col_types = cols(id_ext = "c")) %>%
    dplyr::inner_join(pls_interesse_processed, by = "id_ext") %>%
    dplyr::select(id_ext, casa, id_leggo, tidyselect::all_of(colunas)) %>%
    dplyr::distinct(id_leggo, interesse, nome_interesse, apelido, keywords, tema, tema_slug, advocacy_link, keywords, tipo_agenda)

  return(proposicoes_capturadas)
}
