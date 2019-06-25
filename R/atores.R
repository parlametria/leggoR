#' @title Cria tabela com autores de documentos com seus respectivos tipos de documentos
#' @description Retorna um dataframe contendo informações com os autores dos documentos e seus tipos
#' @param docs_ids_df Dataframe com os ids dos documentos e de autores
#' @return Dataframe
#' @export
create_tabela_atores <- function(documentos_df, autores_df) {
  atores <- merge(documentos_df, autores_df, by = "id_documento") %>%
    dplyr::select(casa = casa.x,
                  id_autor,
                  nome_autor = nome,
                  sigla_tipo,
                  codTipo,
                  id_documento,
                  descricao_tipo = descricaoTipo)

  atores <- atores %>%
    dplyr::group_by(sigla_tipo,
                    nome_autor,
                    id_autor,
                    codTipo,
                    casa,
                    descricao_tipo) %>%
    dplyr::summarise(qtd_de_documentos = dplyr::n())

  tipos_props <- rcongresso::fetch_tipos_proposicao()
  atores <- merge(atores, tipos_props, by.x = "codTipo", by.y = "cod") %>%
    dplyr::select(-nome,
                  -sigla)
}
