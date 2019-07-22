context("Cria Tabela Atores")

setup <- function(){
  docs_sample_df <<- tibble::tibble(id_principal = c(1,1,2),
                            casa = c('camara','camara','camara'),
                            id_documento = c(11,12,21),
                            cod_tipo = c(125,125,121),
                            sigla_tipo = c('EMC','EMC','REQ'),
                            descricao_tipo_documento = c('Emenda na Comissão', 'Emenda na Comissão', 'Requerimento de Audiência Pública'),
                            status_proposicao_sigla_orgao = c('PLEN', 'CCJ', 'CFT'))

  autores_sample_df <<- tibble::tibble(id_documento = c(11,11,12,21,21),
                                      casa = c('camara','camara','camara','camara','camara'),
                                      id_autor = c(1,5,5,5,6),
                                      partido = c('Partido A','Partido C','Partido C','Partido C', 'Partido B'),
                                      uf = c('SP', "PB", "PB", "PB", "BA"),
                                     nome = c('Dep. A', 'Dep. C', 'Dep. C', 'Dep. C', 'Dep. D'))

  atores_sample_df <<- tibble::tibble(id_ext = c(1,1,1,2,2),
                                      casa = c('camara','camara','camara','camara','camara'),
                                      id_autor = c(1,5,5,5,6),
                                      nome_autor = c('Dep. A','Dep. C','Dep. C','Dep. C','Dep. D'),
                                      partido = c('Partido A','Partido C','Partido C','Partido C','Partido B'),
                                      uf = c("SP","PB", "PB", "PB", "BA"),
                                      tipo_generico = c('Emenda','Emenda','Emenda',
                                                         'Requerimento','Requerimento'),
                                      qtd_de_documentos = as.integer(c(1,1,1,1,1)),
                                      sigla_local = c('PLEN', 'CCJ', 'PLEN', 'CFT', 'CFT'),
                                      is_important = c(T, F, T, T, T))

  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that('create_tabela_atores() returns dataframe', {
    expect_true(is.data.frame(create_tabela_atores_camara(docs_sample_df, autores_sample_df)))
  })

  test_that('create_tabela_atores() returns warning with empty docs table', {
    expect_warning(create_tabela_atores_camara(tibble::tibble(), autores_sample_df))
  })

  test_that('create_tabela_atores() returns warning with empty authors table', {
    expect_warning(create_tabela_atores_camara(docs_sample_df, tibble::tibble()))
  })

  test_that('create_tabela_atores() returns correct atores table', {
    expect_equal(create_tabela_atores_camara(docs_sample_df, autores_sample_df), atores_sample_df)
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
