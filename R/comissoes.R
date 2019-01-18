source(here::here("R/utils.R"))

#' @title Retorna a composição da comissão da camara
#' @description Retorna um dataframe contendo os membros da comissão
#' @param sigla_comissao Sigla da comissão da Camara
#' @return dataframe
#' @examples 
#' fetch_composicao_comissoes_camara('cmads')
fetch_composicao_comissoes_camara <- function(sigla_comissao) {
  orgaos_camara <- 
    fetch_orgaos_camara() %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::filter(trimws(sigla) == toupper(sigla_comissao)) %>%
    dplyr::select(orgao_id) %>% head(1)
  
  if (nrow(orgaos_camara) == 0) {
    warning("Comissão não encontrada")
    n <- tibble::frame_data(~ cargo, ~ id, ~ partido, ~ uf, ~ situacao, ~ nome, ~ sigla, ~ casa)
    return(n)
  }
  
  url <- paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterMembrosOrgao?IDOrgao=', orgaos_camara[[1]])
  
  eventos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    eventos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    magrittr::extract2('membros') %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column("VALUE")
  
  if (nrow(df) == 0) {
    return(tibble::frame_data(~ cargo, ~ id, ~ nome, ~ partido, ~ uf, ~ situacao))
  }
  
  new_names <- c('cargo', 'id', 'nome', 'partido', 'uf', 'situacao')
  
  names(df) <- new_names
  df %>% 
    rowwise() %>% 
    dplyr::mutate(partido = ifelse(length(partido) == 0, "", partido)) %>% 
    dplyr::mutate(uf = ifelse(length(uf) == 0, "", uf)) %>% 
    dplyr::mutate(id = ifelse(length(id) == 0, "", id)) %>% 
    tidyr::unnest() %>%
    dplyr::arrange(nome)
}

#' @title Retorna a composição da comissão 
#' @description Retorna dataframe com os dados dos membros de uma comissão
#' @param sigla sigla da comissão
#' @return Dataframe com os dados dos membros de uma comissão
#' @examples
#' fetch_composicao_comissao("CCJ",'senado')
#' @export
fetch_composicao_comissao <- function(sigla, casa) {
  print(paste0('Baixando composição da comissão ', sigla, ' em ', casa))
  casa <- tolower(casa)
  
  if (casa == 'camara') {
    fetch_composicao_comissoes_camara(sigla) %>%
      dplyr::mutate(sigla = sigla) %>%
      dplyr::mutate(casa = casa)
  } else if (casa == 'senado') {
    new_name <- c("cargo", "id", "partido", "uf", "situacao", "nome", "sigla", "casa")
    comissao <- 
      fetch_composicao_comissoes_senado(sigla) %>% 
      dplyr::mutate(sigla = sigla,
                    casa = casa)
    names(comissao) <- new_name
    return(comissao)
  } else {
    print('Parâmetro "casa" não identificado.')
  }
}

#' @title Retorna a composição da comissão do senado
#' @description Retorna dataframe com os dados dos membros de uma comissão do Senado
#' @param sigla Sigla da comissão do Senado
#' @return Dataframes
fetch_composicao_comissoes_senado <- function(sigla) {
  url <- paste0('http://legis.senado.leg.br/dadosabertos/comissao/', sigla)
  tryCatch(
    {
      json_sessions <- jsonlite::fromJSON(url, flatten = T)
      
      colegiado <-
        json_sessions %>%
        magrittr::extract2('DetalheComissao') %>%
        magrittr::extract2('COLEGIADO') %>%
        magrittr::extract2('COLEGIADO_ROW') 
      
      colegiado[sapply(colegiado, is.null)] <- NULL
      comissao <-
        colegiado %>% 
        tibble::as.tibble() 
      
      cargos <- 
        comissao %>%
        magrittr::extract2('CARGOS') %>%
        magrittr::extract2('CARGOS_ROW') %>%
        tibble::as.tibble()
      
      membros <- 
        comissao %>%
        magrittr::extract2('MEMBROS_BLOCO') %>%
        magrittr::extract2('MEMBROS_BLOCO_ROW')
      
      if(!is.null(membros)) {
        
        if('PARTIDOS_BLOCO.PARTIDOS_BLOCO_ROW' %in% names(membros) |
           "MEMBROS.MEMBROS_ROW" %in% names(membros) &
           typeof(membros$MEMBROS.MEMBROS_ROW) == "list") {
          membros <- 
            membros %>%
            dplyr::select(-PARTIDOS_BLOCO.PARTIDOS_BLOCO_ROW) %>% 
            tidyr::unnest()
        }
        membros <-
          membros %>%
          tidyr::unnest()
        
        if (nrow(cargos) == 0 | !('HTTP' %in% names(cargos))) {
          membros %>%
            dplyr::mutate(CARGO = NA) %>%
            dplyr::select(c("CARGO", "@num", "PARTIDO", "UF", "TIPO_VAGA", "PARLAMENTAR"))
        } else {
          if ("MEMBROS.MEMBROS_ROW.HTTP" %in% names(membros)) {
            membros <- 
              membros %>%
              dplyr::left_join(cargos, by = c ("MEMBROS.MEMBROS_ROW.HTTP" = "HTTP")) %>%
              dplyr::select(c("CARGO", "@num.x", "MEMBROS.MEMBROS_ROW.PARTIDO", "MEMBROS.MEMBROS_ROW.UF", "MEMBROS.MEMBROS_ROW.TIPO_VAGA", "MEMBROS.MEMBROS_ROW.PARLAMENTAR"))
          }else {
            membros %>%
              dplyr::left_join(cargos, by = 'HTTP') %>%
              dplyr::select(c("CARGO", "@num.x", "PARTIDO", "UF", "TIPO_VAGA", "PARLAMENTAR.x"))
          } 
        }
      }else {
        tibble::frame_data(~ CARGO, ~ num.x, ~ PARTIDO, ~ UF, ~ TIPO_VAGA, ~ PARLAMENTAR.x)
      }
    },
    error=function(cond) {
      return(tibble::frame_data(~ CARGO, ~ num.x, ~ PARTIDO, ~ UF, ~ TIPO_VAGA, ~ PARLAMENTAR.x))
    }
  )  
}

#' @title Baixa todas as composições das comissões atuais do Senado e da Câmara
#' @description Retorna um dataframe contendo dados sobre as composições das comissões atuais do Senado e da Câmara
#' @return Dataframe
#' @examples
#' fetch_all_composicao_comissao()
#' @importFrom RCurl getURL
#' @importFrom dplyr %>%
#' @export
fetch_all_composicao_comissao <- function() {
  siglas_comissoes <- fetch_orgaos_camara() %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::select(sigla) %>% 
    dplyr::mutate(casa = 'camara',
                  sigla = trimws(sigla)) %>% 
    dplyr::filter(sigla != 'PLEN')
  
  siglas_comissoes <- 
    rbind(siglas_comissoes,
          fetch_orgaos_senado() %>%
            dplyr::mutate(casa = 'senado')) %>% 
    dplyr::distinct()
  
  composicao_comissoes <-
    purrr::map2_df(siglas_comissoes$sigla, siglas_comissoes$casa, ~ fetch_composicao_comissao(.x, .y)) %>%
    dplyr::mutate(partido = trimws(partido))
  
  return(composicao_comissoes)
}