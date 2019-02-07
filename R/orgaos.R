#' @title Baixa os órgãos na Câmara
#' @description Retorna um dataframe contendo os órgãos da câmara
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
  url <- 'http://legis.senado.leg.br/dadosabertos/dados/'

  url_comissoes_permanentes <- RCurl::getURL(paste0(url, 'ComissoesPermanentes.xml'))

  url_comissoes_temporarias <- RCurl::getURL(paste0(url, 'ComissoesTemporarias.xml'))

  comissoes_permanentes_df <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_permanentes),
      "//Colegiado")) %>%
    dplyr::select(sigla = SiglaColegiado)

  comissoes_temporarias_df <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(
      XML::xmlParse(url_comissoes_temporarias),
      "//Colegiado")) %>%
    dplyr::select(sigla = SiglaColegiado)

  url <- 'https://www.congressonacional.leg.br/dados/comissao/lista/'

  url_comissoes_mistas <- RCurl::getURL(paste0(url, 'mistas'))

  comissoes_mistas_df <-
    XML::xmlToDataFrame(nodes = XML::getNodeSet(XML::xmlParse(url_comissoes_mistas),
                                                "//IdentificacaoComissao")) %>%
    dplyr::select(sigla = SiglaComissao)

  df <-
    rbind(comissoes_permanentes_df, comissoes_mistas_df) %>%
    rbind(comissoes_mistas_df) %>%
    dplyr::distinct()

  df <-
    df %>% dplyr::filter(!stringr::str_detect(sigla, '^CMMPV'))

  return(df)
}
