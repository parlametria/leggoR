atores <- function(documentos_df, autores_df) {
  atores <- merge(documentos_df, autores_df, by = "id_documento") %>%
    dplyr::select(casa = casa.x,
                  id_autor,
                  nome_autor = nome,
                  sigla_tipo,
                  codTipo,
                  id_documento)

  atores <- atores %>%
    dplyr::group_by(sigla_tipo,
                    nome_autor,
                    id_autor,
                    codTipo,
                    casa) %>%
    dplyr::summarise(qtd_de_documentos = dplyr::n())

  tipos_props <- rcongresso::fetch_tipos_proposicao()
  atores <- merge(atores, tipos_props, by.x = "codTipo", by.y = "cod")
}
