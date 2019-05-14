#' @title Baixa todos os órgãos
#' @description Retorna um dataframe contendo todos os órgãos da câmara
#' @return Dataframe contendo os órgãos da Câmara
#' @export
fetch_orgaos_camara <- function() {
    rcongresso::fetch_orgaos_camara() %>%
        dplyr::rename("tipo_orgao_id"="codTipoOrgao", "descricao"="nome") %>%
        dplyr::mutate(
                    orgao_id=uri %>%
                        strsplit("/") %>%
                        sapply(tail, 1L, simplify = TRUE) %>%
                        as.numeric())
}


#' @title Baixa todas as siglas das comissões atuais do Senado
#' @description Retorna um dataframe contendo as siglas das comissões atuais do Senado
#' @return Dataframe
#' @examples
#' fetch_orgaos_senado()
#' @importFrom RCurl getURL
#' @importFrom dplyr %>%
fetch_orgaos_senado <- function() {
  url_base <- 'http://legis.senado.leg.br/dadosabertos/comissao/lista/'
  
  comissoes_permanentes_df <- tibble::tibble()
  comissoes_temporarias_df <- tibble::tibble()
  cpis_df <- tibble::tibble()

  url_comissoes_permanentes <- RCurl::getURL(paste0(url_base, 'permanente'))
  
  url_comissoes_temporarias <- RCurl::getURL(paste0(url_base, 'temporaria'))

  url_cpis <- RCurl::getURL(paste0(url_base, 'cpi'))
  
  comissoes_permanentes_df <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_permanentes),
      "//colegiado")) %>%
    dplyr::select(sigla = SiglaColegiado) %>%
    dplyr::filter(!is.na(sigla))

  comissoes_temporarias_tmp <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_temporarias),
      "//colegiado"))
  
  if (ncol(comissoes_temporarias_tmp) > 1) {
    comissoes_temporarias_df <- comissoes_temporarias_tmp %>% 
      dplyr::select(sigla = SiglaColegiado) %>%
      dplyr::filter(!is.na(sigla))
  }
    
  cpis_tmp <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_cpis),
      "//colegiado"))
  
  if (ncol(cpis_tmp) > 1) {
    cpis_df <- cpis_tmp %>% 
      dplyr::select(sigla = SiglaColegiado) %>%
      dplyr::filter(!is.na(sigla))
  }
  
  df <-
    rbind(comissoes_permanentes_df, comissoes_temporarias_df, cpis_df) %>%
    dplyr::distinct()

  return(df)
}

#' @title Baixa todas as siglas das comissões atuais do Congresso Nacional
#' @description Retorna um dataframe contendo as siglas das comissões atuais do Congresso Nacional
#' @return Dataframe
#' @examples
#' fetch_orgaos_cn()
#' @importFrom RCurl getURL
#' @importFrom dplyr %>%
fetch_orgaos_congresso_nacional <- function() {
  url_base <- 'https://www.congressonacional.leg.br/dados/comissao/lista/'
  
  comissoes_permanentes_df <- tibble::tibble()
  comissoes_mpvs_df <- tibble::tibble()
  comissoes_mistas_especiais_df <- tibble::tibble()
  comissoes_analise_veto_df <- tibble::tibble()
  
  url_comissoes_permanentes <- RCurl::getURL(paste0(url_base, 'permanente'))
  
  url_comissoes_mpvs <- RCurl::getURL(paste0(url_base, 'mpv'))
  
  url_comissoes_mistas_especiais <- RCurl::getURL(paste0(url_base, 'mistaEspecial'))
  
  url_comissoes_analise_veto <- RCurl::getURL(paste0(url_base, 'veto'))
  
  comissoes_permanentes_df <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_permanentes),
      "//Colegiado")) %>%
    dplyr::select(sigla = SiglaColegiado) %>%
    dplyr::filter(!is.na(sigla))
  
  comissoes_mpvs_tmp <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_mpvs),
      "//Colegiado"))
  
  if (ncol(comissoes_mpvs_tmp) > 1) {
    comissoes_mpvs_df <- comissoes_mpvs_tmp %>% 
      dplyr::select(sigla = SiglaColegiado) %>%
      dplyr::filter(!is.na(sigla))
  }
  
  comissoes_mistas_especiais_tmp <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_mistas_especiais),
      "//Colegiado"))
  
  if (ncol(comissoes_mistas_especiais_tmp) > 1) {
    comissoes_mistas_especiais_df <- comissoes_mistas_especiais_tmp %>% 
      dplyr::select(sigla = SiglaColegiado) %>%
      dplyr::filter(!is.na(sigla))
  }
  
  comissoes_analise_veto_tmp <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_analise_veto),
      "//Colegiado"))
  
  if (ncol(comissoes_analise_veto_tmp) > 1) {
    comissoes_analise_veto_df <- comissoes_analise_veto_tmp %>% 
      dplyr::select(sigla = SiglaColegiado) %>%
      dplyr::filter(!is.na(sigla))
  }
  
  df <-
    rbind(comissoes_permanentes_df, comissoes_mpvs_df, 
          comissoes_mistas_especiais_df, comissoes_analise_veto_df) %>%
    dplyr::distinct()
  
  return(df)
}
