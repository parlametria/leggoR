#' @title Get Emendas
#' @description Baixa todas as emendas de uma proposição
#' @param id Id da proposição
#' @param casa senado ou camara
#' @export
get_emendas <- function(id, casa) {
  cat(paste(
    "\n--- Processando: ", "\nid:", id,
    "\ncasa", casa, "\n"))
  rcongresso::fetch_emendas(id, casa)
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
    ),
  quiet = FALSE
)

#' @title Escreve as emendas
#' @description Escreve os cvss de emendas
#' @param emendas DataFrame das emendas
#' @param export_path pasta para onde exportar dados
#' @export
write_emendas <- function(emendas, export_path) {
  curr_emendas_raw_filepath <- paste0(export_path, "/emendas_raw.csv")
  if(!file.exists(curr_emendas_raw_filepath)) {
    curr_emendas_raw <- tibble::tribble(~id_ext, ~codigo_emenda, ~data_apresentacao, 
    ~numero, ~local, ~autor, ~casa, ~tipo_documento, ~inteiro_teor)  
  } else {
    curr_emendas_raw <- readr::read_csv(paste0(export_path, "/emendas_raw.csv"))
  }
  readr::write_csv(curr_emendas_raw, paste0(export_path, "/emendas_raw_old.csv"))
  readr::write_csv(emendas, paste0(export_path, "/emendas_raw.csv"))
}

#' @title Processa e escre as emendas
#' @description Processa e escreve os cvs de emendas
#' @param pls dataframe com proposições
#' @param export_path pasta para onde exportar dados
#' @export
fetch_emendas <- function(pls, export_path) {
  res <- list()
  tabela_geral <- agoradigital::converte_tabela_geral_ids_casa(pls)
  res <- append(res, purrr::map2(tabela_geral$id_casa, tabela_geral$casa, safe_get_emendas)) 
  
  emendas <-
    purrr::map_df(res, ~ .$result) %>%
    dplyr::rename(id_ext = prop_id) %>% 
    unique()
  
  write_emendas(emendas, export_path)
}