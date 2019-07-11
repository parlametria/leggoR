context("Tipo de Documento")

# Setup
setup <- function(){
  tipos_documentos <- c("Emenda","Proposição Apensada","Parecer","Requerimento","Voto em Separado","Outros")
  regex_documento_emenda <<- tipos_documentos[1]
  regex_documento_proposicao <<- tipos_documentos[2]
  regex_documento_parecer <<- tipos_documentos[3]
  regex_documento_requerimento <<- tipos_documentos[4]
  regex_documento_voto_em_separado <<- tipos_documentos[5]
  regex_documento_outros <<- tipos_documentos[6]
  
  docs_emendas <<- tibble::tibble(descricao_tipo = c("Emenda",
                                                   "Emenda adotada pela comissão",
                                                   "Emenda adotada pela comissao",
                                                   "Emenda ao substitutivo",
                                                   "Emenda ao plenário",
                                                   "Emenda ao plenario",
                                                   "Emenda de redação",
                                                   "Emenda de redacao",
                                                   "Emenda na comissão",
                                                   "Emenda na comissao",
                                                   "Emenda/substitutivo do senado",
                                                   "Subemenda de relator",
                                                   "Subemenda substitutiva de plenário",
                                                   "Subemenda substitutiva de plenario"))
  
  docs_emendas_gt <<- dplyr::mutate(docs_emendas, tipo = rep(regex_documento_emenda,14))
  
  docs_proposicoes <<- tibble::tibble(descricao_tipo = c("Medida provisoria",
                                                          "Medida provisória",
                                                          "Projeto de lei",
                                                          "Projeto de lei complementar",
                                                          "Projeto de lei de conversão",
                                                          "Projeto de lei de conversao",
                                                          "Proposta de Emenda a constituicao",
                                                          "Proposta de Emenda à constituição"))
  
  docs_proposicoes_gt <<- dplyr::mutate(docs_proposicoes, tipo = rep(regex_documento_proposicao,8))
  
  docs_pareceres <<- tibble::tibble(descricao_tipo = c("Parecer às Emendas apresentadas ao substitutivo do relator",
                                                     "Parecer às Emendas de plenário",
                                                     "Parecer às Emendas ou ao substitutivo do senado",
                                                     "Parecer de comissão",
                                                     "Parecer de comissão para redação final",
                                                     "Parecer do relator",
                                                     "Parecer proferido em plenário",
                                                     "Parecer reformulado",
                                                     "Parecer reformulado de plenário",
                                                     "Parecer vencedor",
                                                     "Complementação de voto",
                                                     "Complementacao de voto",
                                                     "Redação final",
                                                     "Redacao Final",
                                                     "Substitutivo",
                                                     "Substitutivo adotado pela Comissão",
                                                     "Substitutivo adotado pela comissao"))
  
  docs_pareceres_gt <<- dplyr::mutate(docs_pareceres, tipo = rep(regex_documento_parecer,17))
  
  docs_requerimentos <<- tibble::tibble(descricao_tipo = c("Requerimento",
                                                         "Requerimento de apensação",
                                                         "Requerimento de audiência pública",
                                                         "Requerimento de constituição de comissão especial de pec",
                                                         "Requerimento de constituição de comissão especial de projeto",
                                                         "Requerimento de convocação de ministro de estado na comissão",
                                                         "Requerimento de desapensação",
                                                         "Requerimento de desarquivamento de proposições",
                                                         "Requerimento de envio de proposições pendentes de parecer à comissão seguinte ou ao plenário",
                                                         "Requerimento de inclusão de matéria extra-pauta na ordem do dia das comissões",
                                                         "Requerimento de inclusão na ordem do dia",
                                                         "Requerimento de informação",
                                                         "Requerimento de participação ou realização de eventos fora da câmara",
                                                         "Requerimento de prorrogação de prazo de comissão temporária",
                                                         "Requerimento de reconstituição de proposição",
                                                         "Requerimento de redistribuição",
                                                         "Requerimento de retirada de assinatura em proposição de iniciativa coletiva",
                                                         "Requerimento de retirada de assinatura em proposição que não seja de iniciativa coletiva",
                                                         "Requerimento de retirada de proposição",
                                                         "Requerimento de retirada de proposição de iniciativa coletiva",
                                                         "Requerimento de retirada de proposição de iniciativa individual",
                                                         "Requerimento de transformação de sessão plenaria em comissão geral",
                                                         "Requerimento de urgência (art. 154 do ricd)",
                                                         "Requerimento de urgência (art. 155 do ricd)"))
  
  docs_requerimentos_gt <<- dplyr::mutate(docs_requerimentos, tipo = rep(regex_documento_requerimento,24))
  
  docs_voto_em_separado <<- tibble::tibble(descricao_tipo = c("Voto em Separado"))
  
  docs_voto_em_separado_gt <<- dplyr::mutate(docs_voto_em_separado, tipo = c(regex_documento_voto_em_separado))
  
  docs_outros <<- tibble::tibble(descricao_tipo = c("Ata",
                                                  "Autógrafo",
                                                  "Mensagem",
                                                  "Reclamação",
                                                  "Recurso"))
  
  docs_outros_gt <<- dplyr::mutate(docs_outros, tipo = rep(regex_documento_outros,5))
  
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that("Regex do tipo do documento funciona para as emendas", {
    expect_equal(add_tipo_evento_documento(docs_emendas),docs_emendas_gt)
  })
  
  test_that("Regex do tipo do documento funciona para as proposicoes", {
    expect_equal(add_tipo_evento_documento(docs_proposicoes),docs_proposicoes_gt)
  })
  
  test_that("Regex do tipo do documento funciona para os pareceres", {
    expect_equal(add_tipo_evento_documento(docs_pareceres),docs_pareceres_gt)
  })
  
  test_that("Regex do tipo do documento funciona para os requerimentos", {
    expect_equal(add_tipo_evento_documento(docs_requerimentos),docs_requerimentos_gt)
  })
  
  test_that("Regex do tipo do documento funciona para o voto em separado", {
    expect_equal(add_tipo_evento_documento(docs_voto_em_separado),docs_voto_em_separado_gt)
  })
  
  test_that("Regex do tipo do documento funciona para Outros", {
    expect_equal(add_tipo_evento_documento(docs_outros),docs_outros_gt)
  })
  
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')