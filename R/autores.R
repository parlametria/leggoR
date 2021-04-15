congresso_env <-
  jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))

#' @title Recupera a casa da proposição que será usada para recuperar os seus autores
#' @description A partir do id da proposição na Câmara e no Senado recupera qual a casa que será
#' usada para recuperação dos autores.
#' @param id_camara Id da proposição na Câmara
#' @param id_senado Id da proposição no Senado
#' @return "camara" se a câmara deve ser usada para recuperar as informações dos autores ou "senado" caso o
#' senado deva ser usado.
get_casa_proposicao <- function(id_camara, id_senado) {
  if (is.na(id_camara)) {
    casa_proposicao = "senado"
  } else if (is.na(id_senado)) {
    casa_proposicao = "camara"
  } else {
    
    prop_casa <- tryCatch({
      agoradigital::fetch_proposicao(id_senado, "senado") %>%
        dplyr::pull(casa_origem)
    }, error = function(e) {
      message(e)
      return(NA)
    })
    
    if (is.na(prop_casa)) {
      prop_casa <- tryCatch({
        agoradigital::fetch_proposicao(id_camara, "camara") %>%
          dplyr::pull(casa_origem)
      }, error = function(e) {
        message(e)
        return(NULL)
      })
      
      if (is.na(prop_casa) | prop_casa == "senado") {
        casa_proposicao = "senado"
      } else {
        casa_proposicao = "camara"
      }
      
    } else {
      if (prop_casa == "camara") {
        casa_proposicao = "camara"
      } else {
        casa_proposicao = "senado"
      }
    }
  }
  
  return(casa_proposicao)
}

#' @title Recupera autores a partir dos ids da proposição na Câmara e no Senado
#' @description A partir do id da proposição na Câmara e no Senado recupera autores
#' @param id_leggo Id da proposição no Leggo
#' @param id_camara Id da proposição na Câmara
#' @param id_senado Id da proposição no Senado
#' @param total_rows Número total de proposições para análise. Usada apenas no print do log de execução.
#' @return Dataframe com os autores da proposição
process_autores_pl <-
  function(id_leggo,
           id_camara,
           id_senado,
           total_rows = 1) {
    print(
      paste(
        "Recuperando autores para a proposição: câmara",
        id_camara,
        " senado",
        id_senado,
        "-",
        id_leggo,
        "/",
        total_rows
      )
    )
    
    casa_origem <- get_casa_proposicao(id_camara, id_senado)
    print(paste0("Casa da proposição: ", casa_origem))
    
    autores <- tryCatch({
      if (casa_origem == "camara") {
        autores <-
          agoradigital::fetch_autores_documento(id_camara, "camara") %>%
          dplyr::mutate(
            id_autor = as.character(id_autor),
            id_ext = id_camara,
            id_leggo = id_leggo
          ) %>%
          dplyr::select(id_leggo,
                        id_ext,
                        id_autor,
                        tipo_autor = tipo,
                        nome_autor = nome)
        
      } else if (casa_origem == "senado") {
        autores <-
          agoradigital::fetch_autores_documento(id_senado, "senado") %>%
          dplyr::mutate(
            id_autor = as.character(id_parlamentar),
            id_ext = id_senado,
            id_leggo = id_leggo
          ) %>%
          dplyr::mutate(descricao_tipo_autor = dplyr::if_else(descricao_tipo_autor == "Líder",
                                                              "Líder Senado",
                                                              descricao_tipo_autor)) %>%
          dplyr::select(id_leggo,
                        id_ext,
                        id_autor,
                        tipo_autor = descricao_tipo_autor,
                        nome_autor = nome_autor)
      } else {
        stop("Casa inválida!")
      }
    }, error = function(e) {
      cat(paste0("Problemas ao baixar proposição de id", id_leggo, "\n"))
      message(e)
      return(tibble::tribble( ~ id_leggo, ~ id_ext, ~ id_autor, ~ tipo_autor, ~
                                nome_autor))
    })
    
    return(autores)
  }

#' @title Exporta dados dos autores das proposições (matérias legislativas) monitoradas pelo Leggo
#' @description Exporta para uma pasta o CSV que liga uma proposição aos seus autores
#' @param pls dataframe com proposições.
#' @param autores_filepath caminho para o dataframe de autores
#' @export
process_autores_props <- function(pls_ids_filepath, autores_filepath) {
  
  proposicoes_com_autores <- tibble::tibble(id_leggo = character())
  
  if (!is.null(autores_filepath) && file.exists(autores_filepath)) {
    proposicoes_com_autores <- readr::read_csv(autores_filepath, 
                                               col_types = readr::cols("id_leggo" = "c"))
  }
  
  pls <- readr::read_csv(pls_ids_filepath, col_types = readr::cols(prioridade = "c")) %>%
    dplyr::rowwise(.) %>%
    dplyr::mutate(concat_chave_leggo = paste0(id_camara, " ", id_senado)) %>%
    dplyr::mutate(id_leggo = digest::digest(concat_chave_leggo, algo="md5", serialize=F)) %>%
    dplyr::select(-concat_chave_leggo) %>%
    dplyr::select(id_leggo, id_camara, id_senado) %>% 
    dplyr::filter(!id_leggo %in% proposicoes_com_autores$id_leggo)
  
  total_rows <- pls %>% nrow()
  
  if (total_rows > 0) {
    pls_autores <- purrr::pmap_df(
      list(pls$id_leggo,
           pls$id_camara,
           pls$id_senado,
           total_rows),
      ~ process_autores_pl(..1, ..2, ..3, total_rows = total_rows)
    )
    
    if (nrow(pls_autores) > 0) {
      autores_leggo <- pls %>%
        dplyr::select(id_leggo, id_camara, id_senado) %>%
        dplyr::inner_join(
          pls_autores %>%
            select(id_leggo, id_autor, nome_autor, tipo_autor),
          by = c("id_leggo")
        ) %>%
        fuzzyjoin::regex_left_join(
          congresso_env$tipos_autores,
          by = c(nome_autor = "regex"),
          ignore_case = T
        ) %>%
        dplyr::mutate(id_autor = dplyr::if_else(!is.na(id_entidade),
                                                as.character(id_entidade),
                                                id_autor)) %>%
        dplyr::select(-regex,-id_entidade) %>%
        dplyr::mutate(
          id_autor_parlametria = dplyr::case_when(
            tipo_autor == "Deputado" ~ paste0(1, id_autor),
            tipo_autor == "Senador" ~ paste0(2, id_autor),
            tipo_autor == "Líder Senado" ~ paste0(2, id_autor),
            TRUE ~ paste0(3, id_autor)
          )
        ) %>%
        dplyr::select(id_leggo,
                      id_camara,
                      id_senado,
                      id_autor_parlametria,
                      id_autor)
      
      pls_sem_autores_baixados <- pls %>%
        dplyr::filter(!id_leggo %in% (autores_leggo %>% dplyr::pull(id_leggo)))
      
      if (pls_sem_autores_baixados %>% nrow() > 0) {
        message("Não foram recuperados os Autores para as seguintes proposições \n")
        print(pls_sem_autores_baixados)
      }
      
      autores <- proposicoes_com_autores %>%
        dplyr::bind_rows(
          autores_leggo %>%
            dplyr::mutate(
              id_camara = as.character(id_camara),
              id_senado = as.character(id_senado),
              id_autor = as.integer(id_autor),
              id_autor_parlametria = as.integer(id_autor_parlametria)
            )
        )
      
      readr::write_csv(autores,
                       autores_filepath,
                       na = "None")
    }
  } else {
    cat("Não há novos autores de proposições.")
  }
}
