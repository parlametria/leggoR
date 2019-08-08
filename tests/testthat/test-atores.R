context("Atores")

setup <- function() {
  documentos_camara <- read_current_docs_camara("../../data/camara/documentos.csv")
  autores_camara <- read_current_autores_camara("../../data/camara/autores.csv")
  documentos_senado <- read_current_docs_senado("../../data/senado/documentos.csv")
  autores_senado <- read_current_autores_senado("../../data/senado/autores.csv")
  documentos_senado_scrap <- read_current_docs_senado_scrap("../../data/senado/documentos_scrap.csv")
  autores_senado_scrap <- read_current_autores_senado_scrap("../../data/senado/autores_scrap.csv")

  atores_camara <<- create_tabela_atores_camara(documentos_camara, autores_camara)
  atores_senado <<- create_tabela_atores_senado(documentos_senado, autores_senado)
  atores_senado_scrap <<- create_tabela_atores_senado_scrap(documentos_senado_scrap, autores_senado_scrap)
  
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
                                      tipo_autor = rep("deputado",5),
                                      nome_autor = c('Dep. A','Dep. C','Dep. C','Dep. C','Dep. D'),
                                      partido = c('Partido A','Partido C','Partido C','Partido C','Partido B'),
                                      uf = c("SP","PB", "PB", "PB", "BA"),
                                      tipo_generico = c('Emenda','Emenda','Emenda',
                                                        'Requerimento','Requerimento'),
                                      sigla_local = c('PLEN', 'CCJ', 'PLEN', 'CFT', 'CFT'),
                                      qtd_de_documentos = as.integer(c(1,1,1,1,1)),
                                      is_important = c(T, F, T, T, T))
  
  docs_sample_df_scrap <<- tibble::tibble(acao_legislativa = c(NA, NA, NA),
                                    autor = c("Câmara dos Deputados", "Senador Ataídes Oliveira (PSDB/TO)", "Senador Lasier Martins (PSD/RS)"),
                                    casa = c("senado","senado","senado"),
                                    data = c("04/04/2017", "05/04/2017", "12/04/2017"),
                                    descricao_ementa = c("Estabelece medidas de combate à impunidade, à corrupção; altera os Decretos-Leis nºs 2.848, de 7 de dezembro de 1940 – Código Penal, e 3.689, de 3 de outubro de 1941 –Código de Processo Penal; as Leis nºs 4.717, de 29 de junho de 1965, 4.737, de 15 de julho de 1965, 8.072, de 25 de julho de 1990, 8.112, de 11 de dezembro de 1990, 8.429, de 2 de junho de 1992, 8.906, de 4 de julho de 1994, 9.096, de 19 de setembro de 1995, 9.504, de 30 de setembro de 1997, 9.613, de 3 de março de 1998, e 7.347, de 24 de julho de 1985; revoga dispositivos do Decreto-Lei nº 201, de 27 de fevereiro de 1967, e da Lei nº 8.137, de 27 de dezembro de 1990; e dá outras providências.",
                                                         "Requeiro, nos termos do art. 258 do Regimento Interno, a tramitação conjunta do Projeto de Lei do Senado nº 147, de 2016, que estabelece medidas contra a corrupção e demais crimes contra o patrimônio público e combate o enriquecimento ilícito de agentes públicos, e do Projeto de Lei Câmara nº 27, de 2017, que estabelece medidas de combate à impunidade, à corrupção; altera os Decretos-Leis nºs 2.848, de 7 de dezembro de 1940 – Código Penal, e 3.689, de 3 de outubro de 1941 –Código de Processo Penal; as Leis nºs 4.717, de 29 de junho de 1965, 4.737, de 15 de julho de 1965, 8.072, de 25 de julho de 1990, 8.112, de 11 de dezembro de 1990, 8.429, de 2 de junho de 1992, 8.906, de 4 de julho de 1994, 9.096, de 19 de setembro de 1995, 9.504, de 30 de setembro de 1997, 9.613, de 3 de março de 1998, e 7.347, de 24 de julho de 1985; revoga dispositivos do Decreto-Lei nº 201, de 27 de fevereiro de 1967, e da Lei nº 8.137, de 27 de dezembro de 1990; e dá outras providências., por versarem sobre a mesma matéria.",
                                                         NA),
                                    identificacao = c("PLC 27/2017", "Requerimento", "EMENDA 1 - PLC 27/2017"),
                                    id_principal = c(128634, 128634, 128634), 
                                    local = c("Plenário do Senado Federal", "Plenário do Senado Federal", "Comissão de Constituição, Justiça e Cidadania"),
                                    id_documento = c(1, 2, 3))
  
  autores_sample_df_scrap <<- tibble::tibble(id_principal = c(128634, 128634, 128634),
                                             id_documento = c(1, 2, 3),
                                             casa = c("senado","senado","senado"),
                                             nome_autor = c("Câmara dos Deputados", "Senador Ataídes Oliveira", "Senador Lasier Martins"),
                                             partido = c(NA, "PSDB", "PSD"),
                                             uf = c(NA, "TO", "RS"),
                                             tipo_autor = c("nao_parlamentar","senador","senador"),
                                             id_autor = c(NA, 5164, 5533))
  
  atores_senado_scrap_gabarito <<- tibble::tibble(id_ext = c(128634, 128634),
                                                  casa = c("senado","senado"),
                                                  id_autor = c(5164, 5533),
                                                  tipo_autor = c("senador","senador"),
                                                  nome_autor = c("Ataídes Oliveira", "Lasier Martins"),
                                                  partido = c("PSDB", "PSD"),
                                                  uf = c("TO", "RS"),
                                                  tipo_generico = c("Requerimento", "Emenda"),
                                                  sigla_local = c("Plenário", "CCJ"),
                                                  qtd_de_documentos = c(1, 1),
                                                  is_important = c(T, T)) %>% 
    dplyr::mutate(qtd_de_documentos = as.integer(qtd_de_documentos))
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){
    print(e)
    return(FALSE)
  })
}

test <- function() {
  test_that("create_tabela_atores_camara() returns dataframe and is not empty", {
    expect_true(is.data.frame(atores_camara))
    expect_true(is.data.frame(atores_senado))
    expect_true(is.data.frame(atores_senado_scrap))
    expect_true(nrow(atores_camara) > 0)
    expect_true(nrow(atores_senado) > 0)
    expect_true(nrow(atores_senado_scrap) > 0)
  })
  
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
  
  test_that('create_tabela_atores_senado_scrap() returns correct atores table', {
    expect_equal(create_tabela_atores_senado_scrap(docs_sample_df_scrap, autores_sample_df_scrap), atores_senado_scrap_gabarito)
  })
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')