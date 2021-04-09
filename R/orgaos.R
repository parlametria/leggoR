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
  url_base <- 'https://legis.senado.leg.br/dadosabertos/comissao/lista/'
  no_xml <- '//colegiado'
  
  comissoes_permanentes_df <- parse_senado_comissoes_xml(url_base,'permanente',no_xml)
  comissoes_temporarias_df <- parse_senado_comissoes_xml(url_base,'temporaria',no_xml)
  cpis_df <- parse_senado_comissoes_xml(url_base,'cpi',no_xml)
  
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
  no_xml <- '//Colegiado'
  
  comissoes_permanentes_df <- parse_senado_comissoes_xml(url_base,'permanente',no_xml)
  comissoes_mpvs_df <- parse_senado_comissoes_xml(url_base,'mpv',no_xml)
  comissoes_mistas_especiais_df <- parse_senado_comissoes_xml(url_base,'mistaEspecial',no_xml)
  comissoes_analise_veto_df <- parse_senado_comissoes_xml(url_base,'veto',no_xml)
  
  df <-
    rbind(comissoes_permanentes_df, comissoes_mpvs_df, 
          comissoes_mistas_especiais_df, comissoes_analise_veto_df) %>%
    dplyr::distinct()
  
  return(df)
}

#' @title Acessa a URL de comissões do Senado/Congresso Nacional e retorna as comissões encontradas.
#' @description Retorna um dataframe contendo as siglas das comissões encontradas no XML retornado pela URL de Comissões passada como parâmetro.
#' @param url_base URL base de comissões da API
#' @param tipo_comissao tipo da comissão a ser buscado na API (string a ser concatenada com base_url)
#' @param nome_no_xml nome do nó no XML que conterá os dados da comissão
#' @return Dataframe
#' @examples
#' parse_senado_comissoes_xml()
#' @importFrom RCurl getURL
#' @importFrom dplyr %>%
parse_senado_comissoes_xml <- function(url_base, tipo_comissao, nome_no_xml) {
  comissoes_df <- tibble::tibble()
  
  xml_response <- RCurl::getURL(paste0(url_base, tipo_comissao))
  
  comissoes_tmp_df <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(xml_response),
      nome_no_xml))
  
  if (ncol(comissoes_tmp_df) > 1) {
    comissoes_df <- comissoes_tmp_df %>% 
      dplyr::select(sigla = SiglaColegiado) %>%
      dplyr::filter(!is.na(sigla))
  }
  
  return(comissoes_df)
}
