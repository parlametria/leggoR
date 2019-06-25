#' @title Cria tabela com autores de documentos com seus respectivos tipos de documentos
#' @description Retorna um dataframe contendo informações com os autores dos documentos e seus tipos
#' @param docs_ids_df Dataframe com os ids dos documentos e de autores
#' @return Dataframe
#' @export
create_tabela_atores <- function(documentos_df, autores_df) {
  atores <- merge(documentos_df, autores_df, by = c("id_documento", "casa")) %>%
    dplyr::select(casa,
                  id_autor,
                  nome_autor = nome,
                  sigla_tipo,
                  codTipo,
                  id_documento,
                  id_principal,
                  descricao_tipo = descricaoTipo)

  atores <- atores %>%
    dplyr::group_by(id_principal,
                    casa,
                    id_autor,
                    nome_autor,
                    codTipo,
                    sigla_tipo,
                    descricao_tipo) %>%
    dplyr::summarise(qtd_de_documentos = dplyr::n()) %>%
    dplyr::arrange(id_principal, -qtd_de_documentos)

}
