library(tidyverse)
library(here)
library(rlang)

#' @title Preenche o id na outra casa caso já tenha sido previamente preenchido (por outro interesse)
#' @description Esta função lida com proposições repetidas, onde em um dos casos há o preenchimento de 
#' id na câmara e no sendo e outra somente em uma das casas (em diferentes interesses ou não).
#' @param df Dataframe com todas as proposições preenchidas
#' @param col_casa Quo do nome da coluna a ser checada se foi preenchida
#' @param col_segunda_casa Quo do nome da coluna da segunda casa que será preenchida
#' @return Dataframe com proposições id_camara e id_senado mapeadas caso tenham sido 
#' preenchidos em outro interesse.
.preenche_id_casa_repetido <-
  function(df, col_casa, col_segunda_casa) {
    df_com_ids <- df %>%
      distinct(id_camara, id_senado) %>%
      count(!!col_casa) %>%
      filter(!is.na(!!col_casa), n > 1)
    
    if (nrow(df_com_ids) > 0) {
      df_filtrado <- df %>%
        filter(!!col_casa %in% (df_com_ids %>% pull(!!col_casa)))
      
      df_preenchido <- df_filtrado %>%
        filter(!is.na(!!col_segunda_casa)) %>%
        distinct(id_camara, id_senado)
      
      df_para_preencher <- df_filtrado %>%
        filter(is.na(!!col_segunda_casa))
      
      df_sem_repeticoes <-
        df %>% anti_join(df_para_preencher, by = c("id_camara", "id_senado"))
      
      df_para_preencher <- df_para_preencher %>%
        select(-!!col_segunda_casa) %>%
        inner_join(df_preenchido, by = quo_name(col_casa)) %>%
        select(all_of(names(df)))
      
      df_sem_repeticoes <- df_sem_repeticoes %>%
        bind_rows(df_para_preencher)
      
      return(df_sem_repeticoes)
      
    } else {
      return(df)
    }
  }

#' @title Checa se há repetição de ids em alguma das casas.
#' @description Esta função lida com proposições repetidas, onde em um dos casos há o preenchimento de 
#' id na câmara e no sendo e outra somente em uma das casas (em diferentes interesses ou não).
#' Checa se há alguma proposição preenchida em somente uma das casas e, em outro interesse, em ambas.
#' @param df Dataframe com todas as proposições preenchidas
#' @return Dataframe com proposições id_camara e id_senado mapeadas caso tenham sido 
#' preenchidos em outro interesse.
.checa_id_casa_repetidos <- function(df) {
  df_alt <-
    .preenche_id_casa_repetido(df, quo(id_camara), quo(id_senado))
  
  df_alt_completo <-
    .preenche_id_casa_repetido(df_alt, quo(id_senado), quo(id_camara))
  
  return(df_alt_completo)
}

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
                                           interesses$nome,
                                           interesses$descricao),
                  function(interesse, url, nome, descricao) {
                    print(interesse)
                    source(here::here("scripts/proposicoes/process_proposicao.R"))
                    pls <- readr::read_csv(url, col_types = cols(.default = "c")) %>%
                      dplyr::select(proposicao, id_camara, id_senado, apelido, tema, advocacy_link,
                                    keywords, tipo_agenda, dplyr::contains("prioridade")) %>%
                      .checa_proposicoes_infos() %>%
                      dplyr::mutate(interesse = interesse) %>%
                      dplyr::mutate(nome_interesse = nome) %>%
                      dplyr::mutate(descricao_interesse = descricao)
                    return(pls)
                  })
  
  pls_para_analise_completo <- .checa_id_casa_repetidos(pls_para_analise)

  return(pls_para_analise_completo)
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
               "tema_slug", "advocacy_link", "keywords",
               "tipo_agenda", "descricao_interesse")

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
    dplyr::select(id_leggo, tidyselect::all_of(colunas)) %>%
    dplyr::distinct(id_leggo, interesse, .keep_all = TRUE)

  return(proposicoes_capturadas)
}
