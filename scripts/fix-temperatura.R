devtools::install()
library(magrittr)

pautas <- readr::read_csv('data/pautas.csv')

pls_ids <- readr::read_csv('data/tabela_geral_ids_casa.csv') %>%
  dplyr::mutate(id_leggo = dplyr::row_number())

leggo_ids <- dplyr::bind_rows(dplyr::select(pls_ids, id_leggo, id_ext = id_camara, apelido, tema),
                   dplyr::select(pls_ids, id_leggo, id_ext = id_senado, apelido, tema)) %>%
  dplyr::filter(!is.na(id_ext)) %>%
  dplyr::arrange(id_leggo) %>%
  dplyr::distinct()

leggo_ids_temas <- leggo_ids %>%
  dplyr::select(-id_ext) %>%
  dplyr::distinct()

eventos_por_leggo_id <- readr::read_csv('data/trams.csv',
                                        col_types = list(
                                          .default = readr::col_character(),
                                          id_ext = readr::col_double(),
                                          data = readr::col_datetime(format = ""),
                                          sequencia = readr::col_double(),
                                          id_situacao = readr::col_double(),
                                          nivel = readr::col_double(),
                                          data_audiencia = readr::col_datetime(format = "")
                                        )) %>%
  dplyr::inner_join(leggo_ids %>% dplyr::select(id_leggo,id_ext)) %>%
  dplyr::select(id_leggo, dplyr::everything()) %>%
  dplyr::rename(prop_id = id_ext, data_hora = data)

temperatura_por_id_leggo <- eventos_por_leggo_id %>%
  split(.$id_leggo) %>%
  purrr::map_dfr(
    ~ agoradigital::get_historico_temperatura_recente(
      .x,
      granularidade = 's',
      decaimento = 0.25,
      max_date = lubridate::now(),
      pautas = pautas
    ),
    .id = "id_leggo"
  ) %>%
  dplyr::mutate(id_leggo = as.integer(id_leggo))

test <- temperatura_por_id_leggo %>%
  dplyr::group_by(id_leggo, periodo) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::filter(count > 1)

readr::write_csv(temperatura_por_id_leggo, 'data/novo_historico_temperatura.csv')
readr::write_csv(leggo_ids, 'data/leggo_ids.csv')
