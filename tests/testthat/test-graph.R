context("Graph")

setup <- function() {
  
  docs_sample_df <<- tibble::tibble(id_principal = c(1,1,2),
                                    casa = c('camara','camara','camara'),
                                    id_documento = c(11,12,21),
                                    cod_tipo = c(125,125,121),
                                    data_apresentacao = c("2019-09-21", "2019-09-01", "2019-10-07"),
                                    data = c("2019-09-21", "2019-09-01", "2019-10-07"),
                                    sigla_tipo = c('EMC','EMC','REQ'),
                                    descricao_tipo_documento = c('Emenda na Comissão', 'Emenda na Comissão', 'Requerimento de Audiência Pública'),
                                    status_proposicao_sigla_orgao = c('PLEN', 'CCJ', 'CFT'),
                                    url_inteiro_teor =c("google.com", "google.com", "google.com"),
                                    id_leggo = c(1, 1, 1))
  
  autores_sample_df <<- tibble::tibble(id_documento = c(11,11,12,21,21),
                                       casa = c('camara','camara','camara','camara','camara'),
                                       id_autor = c(1,5,5,5,6),
                                       partido = c('Partido A','Partido C','Partido C','Partido C', 'Partido B'),
                                       uf = c('SP', "PB", "PB", "PB", "BA"),
                                       nome = c('Dep. A', 'Dep. C', 'Dep. C', 'Dep. C', 'Dep. D'))
  
  coautorias_sample <<-
    tibble::tribble(~id_leggo, ~ id_principal, ~ casa, ~ id_autor.x, ~ id_autor.y, ~ peso_arestas, ~ num_coautorias, ~ nome.x, ~ partido.x, ~ uf.x, ~ bancada.x, ~ nome.y, ~ partido.y, ~ uf.y, ~ bancada.y,
    1,            1, "camara",          1,          5,          0.5,              1, "Dep. A", "Partido A", "SP",    "governo",   "Dep. C", "Partido C", "PB",    "governo",  
    1,            1, "camara",          5,          5,          1,                1, "Dep. C", "Partido C", "PB",    "governo",   "Dep. C", "Partido C", "PB",    "governo",  
    1,            2, "camara",          5,          6,          0.5,              1, "Dep. C", "Partido C", "PB",    "governo",   "Dep. D", "Partido B", "BA",    "governo" ) 
    
  coautorias <<-
    agoradigital::get_coautorias(docs_sample_df, autores_sample_df, "camara", 0.1) %>% 
    dplyr::mutate(num_coautorias = as.numeric(num_coautorias))
  
  nodes_sample <<-
    tibble::tribble(~id_leggo, ~ id_autor, ~ nome, ~ partido, ~ uf, ~ bancada, ~ nome_eleitoral,        
                    1,        1, "Dep. A", "Partido A", "SP",    "governo", "Dep. A (Partido A/SP)",
                    1,        5, "Dep. C", "Partido C",    "PB",    "governo", "Dep. C (Partido C/PB)", 
                    1,        6, "Dep. D", "Partido B",    "BA",    "governo", "Dep. D (Partido B/BA)")
  
  nodes <<-
    agoradigital::get_unique_nodes(coautorias_sample)
  
  edges_sample <<-
    tibble::tribble(~ id_leggo, ~ source, ~ target, ~ value,
                      1,      1,      5,   0.5,
                      1,      5,      5,   1,  
                      1,      5,      6,   0.5)
  edges <<-
    coautorias_sample %>% 
    dplyr::group_by(id_leggo) %>% 
    dplyr::group_modify(~ agoradigital::generate_edges(., graph_nodes = nodes, edges_weight = 1), keep = T) %>% 
    dplyr::distinct()
  
  nodes_sample_with_size <<-
    tibble::tribble(~id_leggo, ~ id_autor, ~ nome, ~ partido, ~ uf, ~ bancada, ~ nome_eleitoral, ~ node_size,        
                    1,        1, "Dep. A", "Partido A", "SP",    "governo", "Dep. A (Partido A/SP)", 0.5,
                    1,        5, "Dep. C", "Partido C",    "PB",    "governo", "Dep. C (Partido C/PB)", 3,
                    1,        6, "Dep. D", "Partido B",    "BA",    "governo", "Dep. D (Partido B/BA)", 0.5)
  
  nodes_with_size <<-
    agoradigital::compute_nodes_size(edges, nodes)
  
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){
    print(e)
    return(FALSE)
  })
}

test <- function() {
  test_that("get_coautorias() ", {
    expect_true(is.data.frame(coautorias))
    expect_true(nrow(coautorias) > 0)
    expect_equal(coautorias_sample, coautorias)
  })
  
  test_that("get_unique_nodes() ", {
    expect_true(is.data.frame(nodes))
    expect_true(nrow(nodes) > 0)
    expect_equal(nodes_sample, nodes)
  })
  
  test_that("generate_edges() ", {
    expect_true(is.data.frame(edges))
    expect_true(nrow(edges) > 0)
    expect_equal(edges_sample, edges)
  })
  
  test_that("compute_nodes_size() ", {
    expect_true(is.data.frame(nodes_with_size))
    expect_true(nrow(nodes_with_size) > 0)
    expect_equal(nodes_sample_with_size, nodes_with_size)
  })
  
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')
