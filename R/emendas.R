source(here::here("R/utils.R"))

#' @title Retorna as emendas de uma proposição no Congresso
#' @description Retorna dataframe com os dados das emendas de uma proposição no Congresso.
#' @param bill_id ID de uma proposição do Congresso
#' @return Dataframe com as informações sobre as emendas de uma proposição no Congresso.
#' @examples
#' fetch_emendas(91341,'senado')
#' @export
fetch_emendas <- function(id, casa) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    emendas <- fetch_emendas_camara(id)
  } else if (casa == 'senado') {
    emendas <- fetch_emendas_senado(id)
  } else {
    print('Parâmetro "casa" não identificado.')
    return()
  }
  
  emendas  <-
    emendas %>%
    dplyr::mutate(prop_id = id, codigo_emenda = as.integer(codigo_emenda)) %>%
    dplyr::select(
      prop_id, codigo_emenda, data_apresentacao, numero, local, autor, casa, tipo_documento, inteiro_teor) 
  return(emendas)
}

#' @title Retorna as emendas de uma proposição no Senado
#' @description Retorna dataframe com os dados das emendas de uma proposição no Senado.
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as emendas de uma proposição no Senado.
#' @examples
#' fetch_emendas_senado(91341)
fetch_emendas_senado <- function(bill_id) {
  url_base_emendas <-
    "http://legis.senado.leg.br/dadosabertos/materia/emendas/"
  url <- paste0(url_base_emendas, bill_id)
  
  json_emendas <- fetch_json_try(url)
  
  emendas_data <- json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia")
  
  emendas_df <- emendas_data %>%
    magrittr::extract2("Emendas") %>%
    purrr::map_df( ~ .) %>% rename_df_columns()
  
  num_emendas = nrow(emendas_df)
  
  if (num_emendas == 0) {
    emendas_df <-
      tibble::frame_data( ~ codigo_emenda, ~ data_apresentacao, ~ numero, ~ local, ~ autor, ~ partido, ~ casa, ~ tipo_documento, ~ inteiro_teor)
    
  } else if (num_emendas == 1) {
    texto <- generate_dataframe(emendas_df$textos_emenda) %>%
      dplyr::select(tipo_documento, url_texto)
    
    autoria <- generate_dataframe(emendas_df$autoria_emenda) %>%
      dplyr::mutate(
        partido = paste0(
          identificacao_parlamentar_sigla_partido_parlamentar,
          "/",
          identificacao_parlamentar_uf_parlamentar
        )
      )
    
    emendas_df <- emendas_df %>%
      plyr::rename(
        c(
          "numero_emenda" = "numero",
          "colegiado_apresentacao" = "local"
        )
      ) %>%
      dplyr::mutate(autor = autoria$nome_autor,
                    partido = autoria$partido,
                    tipo_documento = texto$tipo_documento,
                    inteiro_teor = texto$url_texto,
                    casa = 'senado') 
    
    
  } else{
    emendas_df <- emendas_df %>%
      tidyr::unnest() %>%
      plyr::rename(
        c(
          "numero_emenda" = "numero",
          "colegiado_apresentacao" = "local",
          "autoria_emenda_autor_nome_autor" = "autor",
          "textos_emenda_texto_emenda_url_texto" = "inteiro_teor",
          "textos_emenda_texto_emenda_tipo_documento" = "tipo_documento",
          "autoria_emenda_autor_identificacao_parlamentar_sigla_partido_parlamentar" = "partido",
          "autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar" = "uf"
        )
      ) %>%
      dplyr::mutate(
        partido = paste0(partido, "/", uf),
        casa = "senado"
      ) 
    
  }
  
  emendas_df %>%
    dplyr::mutate(autor = paste0(autor, " ", partido), 
                  numero = as.integer(numero),
                  tipo_documento = as.character(tipo_documento),
                  inteiro_teor = as.character(inteiro_teor)) %>%
    dplyr::select(-partido)
  
}

#' @title Retorna as emendas de uma proposição na Camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
#' @param id ID de uma proposição da Camara
#' @param sigla Sigla da proposição
#' @param numero Numero da proposição
#' @param ano Ano da proposição
#' @return Dataframe com as informações sobre as emendas de uma proposição na Camara
#' @examples
#' fetch_emendas_camara(408406)
fetch_emendas_camara <- function(id=NA, sigla="", numero="", ano="") {
  if(is.na(id)) {
    url <- 
      paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal?tipo=', sigla, '&numero=', numero, '&ano=', ano)
  }else {
    prop <- agoradigital::fetch_proposicao(id, 'camara', '', '', T, F)
    url <- 
      paste0('http://www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal?tipo=', prop$tipo_materia, '&numero=', prop$numero, '&ano=', prop$ano)
  }
  
  eventos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()
  
  df <-
    eventos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON() %>%
    magrittr::extract2('Emendas') %>%
    tibble::as.tibble() %>%
    t() %>%
    as.data.frame()
  
  if(nrow(df) == 0) {
    return(tibble::frame_data( ~ codigo_emenda, ~ data_apresentacao, ~ numero, ~ local, ~ autor, ~ casa, ~ tipo_documento, ~ inteiro_teor))
  }
  
  new_names <- c("cod_proposicao", "descricao")
  names(df) <- new_names
  
  emendas <- purrr::map_df(df$cod_proposicao, fetch_emendas_camara_auxiliar)
  normalizes_names <- c("codigo_emenda", "data_apresentacao", "numero", "local", "autor", "casa", "tipo_documento", "inteiro_teor")
  names(emendas) <- normalizes_names
  
  emendas %>%
    dplyr::mutate(data_apresentacao = as.character(as.Date(data_apresentacao)))
}

#' @title Função auxiliar para o fetch_emendas_camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
fetch_emendas_camara_auxiliar <- function(id) {
  fetch_proposicao(id, "camara", normalized = T, emendas = T) %>%
    dplyr::select(c(prop_id, data_apresentacao, numero, status_proposicao_sigla_orgao, autor_nome, casa, tipo_materia, ementa))
}