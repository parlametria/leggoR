context("Proposições")

# Setup
setup <- function(){
  CAMARA_ID <<- c(2158425, 2121442)
  SENADO_ID <<- c(91341, 111048)
  return(TRUE)
}

pls_ids <- tibble::tibble(id_camara = c(257161,2088990),
                          id_senado = c(NA,91341),
                          apelido = c('Lei do Licenciamento Ambiental',
                                      'Lei da Qualidade Fiscal'),
                          tema = c('Meio Ambiente','Agenda Nacional'))

current_docs <- tibble::tibble(id_documento = c(260606,257161),
                               id_principal = c(257161,257161),
                               casa = c('camara','camara'))

autores <- "Comissão de Constituição Justiça e Cidadania, Comissão de Constituição Justiça e Cidadania, Senador Cidinho Santos (PL/MT)"
gabarito_extract_autor <-
  tibble::tibble(nome_autor = c("Comissão de Constituição Justiça e Cidadania", "Senador Cidinho Santos "), 
                 partido = c(NA, "PL"),
                 uf = c(NA, "MT"),
                 id_documento = c(1, 1))

doc_example <-
  tibble::tibble(acao_legislativa = "  Apresentada, nesta data, emendas de autoria do Senador Eduardo Gomes. Devolvido ao relator, Senador Marco Rogério, para manifestação sobre as emendas apresentas.",
                 autor = "Senador Eduardo Gomes (MDB/TO)",
                 casa = "senado",
                 data = "09/07/2019",
                 descricao_ementa = NA, 
                 identificacao = "EMENDA 5 - PLS 232/2016",
                 id_principal = 126049,
                 local = "Comissão de Serviços de Infraestrutura",
                 id_documento = 2135)
gabarito_fetch_autor <- 
  tibble::tibble(id_principal = 126049,
                 id_documento = 2135,
                 casa = "senado",
                 nome_autor = "Senador Eduardo Gomes ",
                 partido = "MDB",
                 uf = "TO")

all_pls_ids <- agoradigital::get_all_leggo_props_ids(pls_ids)

current_docs_ids <- current_docs %>%
  dplyr::select(id_documento,
                id_principal,
                casa)

relacionadas_257161 <- rcongresso::fetch_relacionadas("camara",257161)
relacionadas_2088990 <- rcongresso::fetch_relacionadas("camara",2088990)

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){

  test_that("fetch_proposicao() returns dataframe and is not empty", {
    proposicoes_senado <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$SENADO_ID, 'senado'))

    proposicoes_camara <- as.data.frame(CAMARA_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$CAMARA_ID, 'camara'))

    expect_true(is.data.frame(proposicoes_senado))
    expect_true(is.data.frame(proposicoes_camara))
    expect_true(nrow(proposicoes_senado) != 0)
    expect_true(nrow(proposicoes_camara) != 0)
  })

  test_that('fetch_proposicoes() returns dataframe', {
    all_proposicios <- rbind(data.frame(id = SENADO_ID, casa = 'senado'), data.frame(id = CAMARA_ID, casa = 'camara'))
    expect_true(is.data.frame(fetch_proposicoes(all_proposicios)))
  })

  test_that('fetch_proposicao() with invalid casa', {
    expect_true(fetch_proposicao(3123, 'invalid') == "Parâmetro 'casa' não identificado.")
  })

  test_that('import_proposicao() returns dataframe and is not empty', {
    proposicoes_senado <- as.data.frame(SENADO_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$SENADO_ID, 'senado', '', ''))

    proposicoes_camara <- as.data.frame(CAMARA_ID) %>%
      dplyr::rowwise() %>%
      dplyr::do(fetch_proposicao(.$CAMARA_ID, 'camara', '', ''))

    expect_true(is.data.frame(proposicoes_senado))
    expect_true(is.data.frame(proposicoes_camara))
    expect_true(nrow(proposicoes_senado) != 0)
    expect_true(nrow(proposicoes_camara) != 0)
  })

  test_that('import_proposicao() with invalid casa', {
    expect_true(fetch_proposicao(3123, 'invalid', '', '') == "Parâmetro 'casa' não identificado.")
  })

  test_that('find_new_documentos() return dataframe', {
    expect_true(is.data.frame(find_new_documentos(all_pls_ids, current_docs_ids, 'camara')))
  })

  test_that('find_new_documentos() finds correct documents', {
    expect_true(nrow(find_new_documentos(all_pls_ids, current_docs_ids, 'camara')) ==
                                (nrow(relacionadas_257161) + nrow(relacionadas_2088990)))
  })

  test_that('fetch_documentos_data() return dataframe', {
    expect_true(is.data.frame(fetch_documentos_data(current_docs_ids)))
  })

  test_that('fetch_autores_documentos() return dataframe', {
    expect_true(is.data.frame(fetch_autores_documentos(current_docs_ids %>% dplyr::mutate(sigla_tipo = c('PL','PL')))))
  })
  
  test_that('extract_autor_relacionadas_senado() is equal', {
    expect_equal(extract_autor_relacionadas_senado(autores, 1), gabarito_extract_autor)
  })
  
  test_that('fetch_autores_relacionadas_senado() is equal', {
    expect_equal(
      fetch_autores_relacionadas_senado(doc_example), gabarito_fetch_autor)
  })
  

}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
