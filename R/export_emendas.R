#' @title Get Emendas
#' @description Baixa todas as emendas de uma proposição
#' @param id Id da proposição
#' @param casa senado ou camara
#' @export
get_emendas <- function(id, casa) {
  cat(paste(
    "\n--- Processando: ", "\nid:", id,
    "\ncasa", casa, "\n"))
  prop <- agoradigital::fetch_proposicao(id, casa)
  emendas <- 
    rcongresso::fetch_emendas(id, casa, prop$sigla_tipo, prop$numero, prop$ano)
  
  if (casa == 'camara') {
      emendas %>% 
      dplyr::mutate(inteiro_teor = agoradigital::get_emendas_links(codigo_emenda))
  }
  
  emendas
  
}

#' @title Safe get Emendas
safe_get_emendas <- purrr::safely(
  get_emendas,
  otherwise =
    tibble::tribble(
      ~ prop_id,
      ~ codigo_emenda,
      ~ data_apresentacao,
      ~ numero,
      ~ local,
      ~ autor,
      ~ casa,
      ~ tipo_documento
    )
)

#' @title Escreve as emendas
#' @description Escreve os cvss de emendas
#' @param emendas DataFrame das emendas
#' @param export_path pasta para onde exportar dados
#' @export
write_emendas <- function(emendas, export_path) {
  emendas_raw_old <- readr::read_csv(paste0(export_path, "/emendas_raw.csv"))
  readr::write_csv(emendas_raw_old, paste0(export_path, "/emendas_raw_old.csv"))
  readr::write_csv(emendas, paste0(export_path, "/emendas_raw.csv"))
}

#' @title Processa e escre as emendas
#' @description Processa e escreve os cvs de emendas
#' @param pls dataframe com proposições
#' @param export_path pasta para onde exportar dados
#' @export
export_emendas <- function(pls, export_path) {
  res <- list()
  tabela_geral <- agoradigital::converte_tabela_geral_ids_casa(pls)
  res <- append(res, purrr::map2(tabela_geral$id_casa, tabela_geral$casa, safe_get_emendas)) 
  
  emendas <-
    purrr::map_df(res, ~ .$result) %>%
    dplyr::rename(id_ext = prop_id) %>% 
    unique()
  
  write_emendas(emendas, export_path)
}