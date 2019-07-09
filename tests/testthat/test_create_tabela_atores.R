context("Cria Tabela Atores")

setup <- function(){
  docs_sample_df <<- tibble::tibble(id_principal = c(1,1,2),
                            casa = c('camara','camara','camara'),
                            id_documento = c(11,12,21),
                            codTipo = c(125,125,121),
                            sigla_tipo = c('EMC','EMC','REQ'),
                            descricaoTipo = c('Emenda na Comissão', 'Emenda na Comissão', 'Requerimento de Audiência Pública'))

  autores_sample_df <<- tibble::tibble(id_documento = c(11,11,12,21,21),
                                      casa = c('camara','camara','camara','camara','camara'),
                                      id_autor = c(1,5,5,5,6),
                                      partido = c('Partido A','Partido C','Partido C','Partido C', 'Partido B'),
                                      uf = c('SP', "PB", "PB", "PB", "BA"),
                                     nome = c('Dep. A', 'Dep. C', 'Dep. C', 'Dep. C', 'Dep. D'))

  atores_sample_df <<- tibble::tibble(id_ext = c(1,1,2,2),
                                      casa = c('camara','camara','camara','camara'),
                                      id_autor = c(5,1,5,6),
                                      nome_autor = c('Dep. C','Dep. A','Dep. C','Dep. D'),
                                      partido = c('Partido C','Partido A','Partido C','Partido B'),
                                      uf = c('PB', "SP", "PB", "BA"),
                                      cod_tipo = c(125,125,121,121),
                                      sigla_tipo = c('EMC','EMC','REQ','REQ'),
                                      descricao_tipo = c('Emenda na Comissão','Emenda na Comissão',
                                                         'Requerimento de Audiência Pública','Requerimento de Audiência Pública'),
                                      qtd_de_documentos = as.integer(c(2,1,1,1)))

  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that('create_tabela_atores() returns dataframe', {
    expect_true(is.data.frame(create_tabela_atores(docs_sample_df, autores_sample_df)))
  })

  test_that('create_tabela_atores() returns warning with empty docs table', {
    expect_warning(create_tabela_atores(tibble::tibble(), autores_sample_df))
  })

  test_that('create_tabela_atores() returns warning with empty authors table', {
    expect_warning(create_tabela_atores(docs_sample_df, tibble::tibble()))
  })

  test_that('create_tabela_atores() returns correct atores table', {
    expect_equal(create_tabela_atores(docs_sample_df, autores_sample_df), atores_sample_df)
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
