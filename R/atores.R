atores <- function(documentos_df, autores_df) {
  atores <- merge(documentos_df, autores_df, by = "id_documento") %>%
    dplyr::select(casa = casa.x,
                  id_autor,
                  nome,
                  sigla_tipo,
                  codTipo,
                  id_documento)

  atores <- atores %>%
    dplyr::group_by(sigla_tipo,
                    nome,
                    id_autor,
                    casa,
                    id_documento) %>%
    dplyr::summarise(qtd = dplyr::n())
}
