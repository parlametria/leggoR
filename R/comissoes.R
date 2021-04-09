source(here::here("R/utils.R"))

#' @title Recupera informações dos membros de uma Comissão em uma data de inicio específica
#' @description A partir de um id, retorna os membros daquela comissão a partir da data de início especificada
#' @param page Página a ser requisitada
#' @param url Url da requisição
#' @param max_tentativas Número máximo de tentativas
#' @return Dataframe com informações dos membros da Comissão para uma data de início
fetch_membros_comissao_camara_by_page <- function(page = 1,  url, sigla_comissao, max_tentativas = 10) {
  library(tidyverse)
  
  url_paginada <- paste0(url, '&pagina=', page)
  
  for (tentativa in seq_len(max_tentativas)) {
    
    membros <- tryCatch(
      {
        membros <-
          (RCurl::getURL(url_paginada) %>%
             jsonlite::fromJSON())$dados %>%
          tibble::as_tibble()
        
        if (nrow(membros) == 0) {
          return(tibble::tibble(cargo = character(), 
                                id = character(),
                                nome = character(), 
                                partido = character(),
                                uf = character(), 
                                situacao = character(), 
                                data_inicio = character(),
                                data_fim = character()))
        } else {
          
          membros <- membros %>% 
            dplyr::select(cargo = titulo, id, nome, partido = siglaPartido, uf = siglaUf, data_inicio = dataInicio, data_fim = dataFim) %>%
            dplyr::mutate(sigla = sigla_comissao, 
                          id = as.character(id),
                          casa = "camara",
                          cargo = dplyr::case_when(
                            startsWith(cargo, "Presidente") ~ "PRESIDENTE",
                            startsWith(cargo, "Titular") ~ "TITULAR",
                            startsWith(cargo, "1º Vice-Presidente") ~ "PRIMEIRO VICE-PRESIDENTE",
                            startsWith(cargo, "Suplente") ~ "SUPLENTE",
                            startsWith(cargo, "2º Vice-Presidente") ~ "SEGUNDO VICE-PRESIDENTE",
                            startsWith(cargo, "3º Vice-Presidente") ~ "TERCEIRO VICE-PRESIDENTE"
                          ),
                          situacao = dplyr::if_else(cargo == 'SUPLENTE', 'Suplente', 'Titular'))
          membros <- membros[!duplicated(membros$id) | membros$cargo %in% 
                               c("PRESIDENTE", "VICE-PRESIDENTE", "SEGUNDO VICE-PRESIDENTE", "TERCEIRO VICE-PRESIDENTE"),,drop=FALSE]
        }
        return(membros)
      }, error = function(e) {
        print(e)
        return(tibble::tibble(cargo = character(), 
                              id = character(),
                              nome = character(), 
                              partido = character(),
                              uf = character(), 
                              situacao = character(), 
                              data_inicio = character(),
                              data_fim = character()))
      }
    )
    
    if (nrow(membros) == 0) {
      backoff <- runif(n = 1, min = 0, max = 2 ^ tentativa - 1)
      message("Backing off for ", backoff, " seconds.")
      Sys.sleep(backoff)
    } else {
      break
    }
  }
  return(membros)
}

#' @title Recupera informações dos membros de uma Comissão em uma data de inicio específica
#' @description A partir de um id, retorna os membros daquela comissão a partir da data de início especificada
#' @param orgao_id Id da Comissão
#' @param sigla_comissao Sigla da Comissão
#' @param data_inicio_arg Data inicial de interesse (formato AAAA-MM-DD)
#' @param max_tentativas Número máximo de tentativas
#' @return Dataframe com informações dos membros da Comissão para uma data de início
fetch_membros_comissao_camara_with_backoff <- function(orgao_id, sigla_comissao, data_inicio_arg = '2019-02-01', max_tentativas = 10) {
  
  print(paste0('Baixando informações dos membros da comissão ', sigla_comissao, ' na casa camara'))
  url <- paste0('https://dadosabertos.camara.leg.br/api/v2/orgaos/',
                orgao_id, 
                '/membros?dataInicio=', 
                data_inicio_arg, 
                '&itens=100')
  
  links <- (RCurl::getURL(url) %>% jsonlite::fromJSON())$links
  
  last_page <- links %>% 
    dplyr::filter(rel == "last") %>% 
    dplyr::pull(href) %>% 
    stringr::str_match("pagina=(.*?)&") %>% 
    tibble::as_tibble(.name_repair = c("universal")) %>% 
    dplyr::pull(`...2`)
  
  if (rlang::is_empty(last_page)) {
    last_page <- 1
  } 
  
  membros <- tibble::tibble(page = 1:as.numeric(last_page)) %>%
    dplyr::mutate(data = purrr::map(
      page,
      fetch_membros_comissao_camara_by_page,
      url,
      sigla_comissao,
      max_tentativas
    )) %>% 
    tidyr::unnest(data)
  
  return(membros)
}

#' @title Retorna a composição da comissão da camara
#' @description Retorna um dataframe contendo os membros da comissão
#' @param sigla_comissao Sigla da comissão da Camara
#' @return dataframe
#' @examples
#' fetch_composicao_comissoes_camara('cmads')
fetch_composicao_comissoes_camara <- function(sigla_comissao, orgaos_camara, data_inicio_arg = "2019-02-01") {
  orgaos_camara <-
    orgaos_camara %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::filter(trimws(sigla) == toupper(sigla_comissao)) %>%
    dplyr::select(orgao_id) %>% head(1)

  if (nrow(orgaos_camara) == 0) {
    warning("Comissão não encontrada")
    n <- tibble::tibble(
      cargo = character(), 
      id = integer(),
      partido = character(),
      uf = character(),
      situacao = character(),
      nome = character(),
      sigla = character(),
      casa = character(),
      data_inicio = character(),
      data_fim= character())
    return(n)
  }

  df <-
    fetch_membros_comissao_camara_with_backoff(orgaos_camara %>%
                                                 head(1) %>%
                                                 dplyr::pull(orgao_id),
                                               sigla_comissao,
                                               data_inicio_arg)
  
  df <- df %>%
    dplyr::mutate(sigla = sigla_comissao, casa = "camara") %>%
    dplyr::rowwise(.) %>%
    dplyr::mutate(foto = paste0(
      'https://www.camara.leg.br/internet/deputado/bandep/',
      id,
      '.jpg'
    )) %>%
    dplyr::ungroup() %>% 
    dplyr::select(cargo,
                  id,
                  partido,
                  uf,
                  situacao,
                  nome,
                  foto,
                  sigla,
                  casa,
                  data_inicio,
                  data_fim) %>%
    dplyr::distinct()
}

#' @title Retorna a composição da comissão
#' @description Retorna dataframe com os dados dos membros de uma comissão
#' @param sigla sigla da comissão
#' @return Dataframe com os dados dos membros de uma comissão
#' @examples
#' fetch_composicao_comissao("CCJ",'senado')
#' @export
fetch_composicao_comissao <- function(sigla, casa, orgaos_camara, data_inicio_arg = "2019-02-01") {
  print(paste0('Baixando composição da comissão ', sigla, ' no(a) ', casa))
  casa <- tolower(casa)

  if (casa == 'camara') {
    comissao <- 
      fetch_composicao_comissoes_camara(sigla, orgaos_camara, data_inicio_arg) 
      
  } else if (casa == 'senado' || casa == 'congresso_nacional') {
    new_name <- c("cargo", "id", "partido", "uf", "situacao", "nome", "foto", "sigla", "casa")
    comissao <-
      fetch_composicao_comissoes_senado(sigla) %>%
      dplyr::mutate(sigla = strsplit(sigla, "/")[[1]][1],
                    casa = casa)
    names(comissao) <- new_name
    comissao <-
      comissao %>% 
      dplyr::filter(!is.na(id))
  } else {
    return('Parâmetro "casa" não identificado.')
  }
  
  return(comissao)
}

#' @title Retorna a composição da comissão do senado
#' @description Retorna dataframe com os dados dos membros de uma comissão do Senado
#' @param sigla Sigla da comissão do Senado
#' @return Dataframes
fetch_composicao_comissoes_senado <- function(sigla) {
  url <- paste0('https://legis.senado.leg.br/dadosabertos/comissao/', sigla)
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
        tibble::as_tibble()

      cargos <-
        comissao %>%
        magrittr::extract2('CARGOS') %>%
        magrittr::extract2('CARGOS_ROW') %>%
        tibble::as_tibble()

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
            dplyr::select(c("CARGO", "HTTP", "PARTIDO", "UF", "TIPO_VAGA", "PARLAMENTAR", "FOTO"))
        } else {
          if ("MEMBROS.MEMBROS_ROW.HTTP" %in% names(membros)) {
            membros <-
              membros %>%
              dplyr::left_join(cargos, by = c ("MEMBROS.MEMBROS_ROW.HTTP" = "HTTP")) %>%
              dplyr::select(
                c("CARGO", "MEMBROS.MEMBROS_ROW.HTTP", "MEMBROS.MEMBROS_ROW.PARTIDO", "MEMBROS.MEMBROS_ROW.UF", "MEMBROS.MEMBROS_ROW.TIPO_VAGA", "MEMBROS.MEMBROS_ROW.PARLAMENTAR", "MEMBROS.MEMBROS_ROW.FOTO"))
          }else {
            membros %>%
              dplyr::left_join(cargos, by = "HTTP") %>%
              dplyr::select(c("CARGO", "HTTP", "PARTIDO", "UF", "TIPO_VAGA", "PARLAMENTAR.x", "FOTO"))
          }
        }
      }else {
        tibble::tibble(
          CARGO = character(),
          HTTP = character(),
          PARTIDO = character(),
          UF = character(),
          TIPO_VAGA = character(),
          PARLAMENTAR.x = character(),
          FOTO = character()
        )
      }
    }, 
    error=function(cond) {
      return(
        tibble::tibble(
          CARGO = character(),
          HTTP = character(),
          PARTIDO = character(),
          UF = character(),
          TIPO_VAGA = character(),
          PARLAMENTAR.x = character(),
          FOTO = character()
        )
      )
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
fetch_all_composicao_comissao <- function(data_inicio = "2019-02-01") {
  orgaos_camara <- fetch_orgaos_camara()
  
  siglas_camara <- tryCatch({
    orgaos_camara %>% 
      dplyr::filter(tipo_orgao_id == 2 |
                      (tipo_orgao_id == 3 & dataFim == "")) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::select(sigla) %>%
      dplyr::mutate(casa = 'camara',
                    sigla = trimws(sigla)) %>%
      dplyr::filter(sigla != 'PLEN')
  }, error=function(e){
    return(tibble::tibble(sigla = character(), casa = character()))
  })
   
  
  siglas_senado <- tryCatch({
    fetch_orgaos_senado() %>% 
      dplyr::mutate(casa = 'senado', 
                    sigla = stringr::str_replace_all(sigla, " ", ""))
  }, error=function(e){
    return(tibble::tibble(sigla = character(), casa = character()))
  })
    
  siglas_cong_nacional <- tryCatch({
    fetch_orgaos_congresso_nacional() %>%
      dplyr::mutate(casa = 'congresso_nacional', 
                    sigla = stringr::str_replace_all(sigla, " ", ""))
  }, error=function(e){
    return(tibble::tibble(sigla = character(), casa = character()))
  })

  siglas_comissoes <- 
    rbind(siglas_camara, siglas_senado, siglas_cong_nacional) %>%
    dplyr::distinct() %>%
    dplyr::arrange(casa,sigla)
  
  composicao_comissoes <-
    purrr::map2_df(siglas_comissoes$sigla, siglas_comissoes$casa, ~ fetch_composicao_comissao(.x, .y, orgaos_camara, data_inicio)) %>%
    dplyr::mutate(partido = trimws(partido))

  return(composicao_comissoes)
}
