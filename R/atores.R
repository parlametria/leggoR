#' @title Cria tabela com atores de documentos com seus respectivos tipos de documentos
#' @description Retorna um dataframe contendo informações com os autores dos documentos e seus tipos
#' @param documentos_df Dataframe dos documentos
#' @param autores_df Dataframe com autores dos documentos
#' @return Dataframe
#' @export
create_tabela_atores <- function(documentos_df, autores_df) {

  if ((is.null(documentos_df) | is.null(autores_df)) |
      ((nrow(documentos_df) == 0) | (nrow(autores_df) == 0))) {
    warning("Dataframes de entrada devem ser não-nulos e não-vazios.")
    return(tibble::tibble())
  }

  autores_docs <- merge(documentos_df, autores_df, by = c("id_documento", "casa")) %>%
    dplyr::mutate(nome_autor = ifelse(is.na(nome), nome_autor, nome)) %>%
    dplyr::select(id_principal,
                  casa,
                  id_documento,
                  id_autor,
                  nome_autor,
                  sigla_tipo,
                  descricao_tipo = descricaoTipo)

  atores_df <- autores_docs %>%
    dplyr::group_by(id_principal,
                    casa,
                    id_autor,
                    nome_autor,
                    codTipo,
                    sigla_tipo,
                    descricao_tipo) %>%
    dplyr::summarise(qtd_de_documentos = dplyr::n()) %>%
    dplyr::arrange(id_principal, -qtd_de_documentos)

  return(atores_df)
}
