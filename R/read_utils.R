#' @export
read_pls_ids <- function(pls_ids_filepath) {
  pls_ids <- readr::read_csv(pls_ids_filepath,
                             col_types = list(
                               id_camara = readr::col_double(),
                               id_senado = readr::col_double(),
                               apelido = readr::col_character(),
                               tema = readr::col_character()
                             ))
  pls_ids
}

#' @export
read_current_docs_camara <- function(file_path) {
  current_docs <- readr::read_csv(file_path,
                                  col_types = list(
                                    .default = readr::col_character(),
                                    id_documento = readr::col_double(),
                                    id_principal = readr::col_double(),
                                    numero = readr::col_integer(),
                                    ano = readr::col_integer(),
                                    data_apresentacao = readr::col_datetime(format = ""),
                                    cod_tipo_documento = readr::col_integer(),
                                    status_proposicao_cod_situacao = readr::col_integer(),
                                    status_proposicao_cod_tipo_tramitacao = readr::col_integer(),
                                    status_proposicao_data_hora = readr::col_datetime(format = ""),
                                    status_proposicao_sequencia = readr::col_integer()
                                  ))
}

#' @export
read_current_autores_camara <- function(file_path) {
  current_autores <- readr::read_csv(file_path,
                                     col_types = list(
                                       .default = readr::col_character(),
                                       id_autor = readr::col_double(),
                                       cod_tipo_autor = readr::col_integer()
                                     ))

}

#' @export
read_deputados <- function(file_path) {
  deputados <- readr::read_csv(file_path,
                               col_types = list(
                                 .default = readr::col_character(),
                                 data_falecimento = readr::col_date(format = ""),
                                 data_nascimento = readr::col_date(format = ""),
                                 id = readr::col_double(),
                                 ultimo_status_gabinete_andar = readr::col_double(),
                                 ultimo_status_gabinete_sala = readr::col_double(),
                                 ultimo_status_id = readr::col_double(),
                                 ultimo_status_id_legislatura = readr::col_double()
                               ))

  deputados <- deputados %>% dplyr::select(id, partido = ultimo_status_sigla_partido, uf = ultimo_status_sigla_uf)

}

#' @export
read_current_docs_senado <- function(file_path) {
  current_docs <- readr::read_csv(file_path,
                                  col_types = list(
                                    .default = readr::col_character(),
                                    id_documento = readr::col_double(),
                                    id_principal = readr::col_double(),
                                    ano = readr::col_double(),
                                    data_apresentacao = readr::col_date(format = ""),
                                    codigo_assunto_especifico = readr::col_double(),
                                    codigo_assunto_geral = readr::col_double(),
                                    codigo_natureza = readr::col_double(),
                                    data_leitura = readr::col_date(format = ""),
                                    proposicoes_apensadas = readr::col_logical()
                                  ))

}

#' @export
read_current_autores_senado <- function(file_path) {
  current_autores <- readr::read_csv(file_path,
                                     col_types = list(
                                       .default = readr::col_character(),
                                       id_autor = readr::col_double(),
                                       id_documento = readr::col_double()
                                     ))

}
