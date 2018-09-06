# CAMARA

.COLNAMES_EMENTAS_CAMARA <- c("ementa" = "character",
                              "sigla_tipo" = "character",
                              "numero" = "numeric")

.COLNAMES_LAST_DESPACHO_CAMARA <- list(
  "data_hora" = c("POSIXct","POSIXt"),
  "texto_tramitacao" = "character"
)

.COLNAMES_EXTRACT_LOCAIS_IN_CAMARA <- c(
  "data_hora" = 'character',
  "descricao_situacao" = "character",
  "descricao_tramitacao" = "character",
  "despacho" = "character",
  "id_prop" = "integer",
  "id_situacao" = "integer",
  "id_tipo_tramitacao" = "character",
  "regime" = "character",
  "sequencia" = "integer",
  "sigla_orgao" = "character",
  "uri_orgao" = "character",
  "url" = "character",
  "local" = "character"
)

.COLNAMES_AUTHOR_CAMARA <- c(
  "autor.uri" = "logi",
  "autor.nome" = "character",
  "autor.cod_tipo" = "integer",
  "autor.cod_tipo" = "character",
  "autor.tipo" = "integer",
  "casa_origem" = "integer"
)


# SENADO
