context("Regex")

# Setup
setup <- function(){
  tipos_documentos <<- camara_env$tipos_documentos
  regex_documento_emenda <<- tipos$regex[1]
  regex_documento_proposicao <<- tipos$regex[2]
  regex_documento_mensagem <<- tipos$regex[3]
  regex_documento_parecer <<- tipos$regex[4]
  regex_documento_recurso <<- tipos$regex[5]
  regex_documento_requerimento <<- tipos$regex[6]
  regex_documento_substitutivo <<- tipos$regex[7]
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that("Regex das emendas", {
    expect_true(stringr::str_detect("emenda", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda adotada pela comissão", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda adotada pela comissao", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda ao substitutivo", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda ao plenário", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda ao plenario", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda de redação", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda de redacao", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda na comissão", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda na comissao", regex_documento_emenda))
    expect_true(stringr::str_detect("emenda/substitutivo do senado", regex_documento_emenda))
    expect_true(stringr::str_detect("subemenda de relator", regex_documento_emenda))
    expect_true(stringr::str_detect("subemenda substitutiva de plenário", regex_documento_emenda))
    expect_true(stringr::str_detect("subemenda substitutiva de plenario", regex_documento_emenda))
  })
  
  test_that("Regex das proposicoes", {
    expect_true(stringr::str_detect("medida provisoria", regex_documento_proposicao))
    expect_true(stringr::str_detect("medida provisória", regex_documento_proposicao))
    expect_true(stringr::str_detect("projeto de lei", regex_documento_proposicao))
    expect_true(stringr::str_detect("projeto de lei complementar", regex_documento_proposicao))
    expect_true(stringr::str_detect("projeto de lei de conversão", regex_documento_proposicao))
    expect_true(stringr::str_detect("projeto de lei de conversao", regex_documento_proposicao))
    expect_true(stringr::str_detect("proposta de emenda a constituicao", regex_documento_proposicao))
    expect_true(stringr::str_detect("proposta de emenda à constituição", regex_documento_proposicao))
  })
  
  test_that("Regex das mensagens", {
    expect_true(stringr::str_detect("mensagem", regex_documento_mensagem))
    expect_true(stringr::str_detect("mensagem de cancelamento de urgência", regex_documento_mensagem))
    expect_true(stringr::str_detect("mensagem de cancelamento de urgencia", regex_documento_mensagem))
    expect_true(stringr::str_detect("mensagem de solicitação de urgência", regex_documento_mensagem))
    expect_true(stringr::str_detect("mensagem de solicitacao de urgencia", regex_documento_mensagem))
  })
  
  test_that("Regex dos paraceres", {
    expect_true(stringr::str_detect("parecer às emendas apresentadas ao substitutivo do relator", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer às emendas de plenário", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer às emendas ou ao substitutivo do senado", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer de comissão", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer de comissão para redação final", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer do relator", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer proferido em plenário", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer reformulado", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer reformulado de plenário", regex_documento_parecer))
    expect_true(stringr::str_detect("parecer vencedor", regex_documento_parecer))
  })
  
  test_that("Regex dos recursos", {
    expect_true(stringr::str_detect("recurso", regex_documento_recurso))
    expect_true(stringr::str_detect("recurso contra apensação/desapensação de proposição (art. 142, i, ricd)", regex_documento_recurso))
    expect_true(stringr::str_detect("recurso contra apreciação conclusiva de comissão (art. 58, § 1º c/c art. 132, § 2º, ricd)", regex_documento_recurso))
    expect_true(stringr::str_detect("recurso contra decisão de presidente de comissão em questão de ordem (art. 57, XXI c/c art. 17, iii, f, ricd)", regex_documento_recurso))
    expect_true(stringr::str_detect("recurso contra declaração de prejudicialidade. (art. 164, § 2º, ricd)", regex_documento_recurso))
    expect_true(stringr::str_detect("recurso contra devolução de proposição (art. 137, § 2º, ricd)", regex_documento_recurso))
  })
  
  test_that("Regex dos requerimentos", {
    expect_true(stringr::str_detect("requerimento", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de apensação", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de audiência pública", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de constituição de comissão especial de pec", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de constituição de comissão especial de projeto", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de convocação de ministro de estado na comissão", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de desapensação", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de desarquivamento de proposições", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de envio de proposições pendentes de parecer à comissão seguinte ou ao plenário", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de inclusão de matéria extra-pauta na ordem do dia das comissões", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de inclusão na ordem do dia", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de informação", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de participação ou realização de eventos fora da câmara", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de prorrogação de prazo de comissão temporária", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de reconstituição de proposição", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de redistribuição", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de retirada de assinatura em proposição de iniciativa coletiva", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de retirada de assinatura em proposição que não seja de iniciativa coletiva", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de retirada de proposição", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de retirada de proposição de iniciativa coletiva", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de retirada de proposição de iniciativa individual", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de transformação de sessão plenaria em comissão geral", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de urgência (art. 154 do ricd)", regex_documento_requerimento))
    expect_true(stringr::str_detect("requerimento de urgência (art. 155 do ricd)", regex_documento_requerimento))
  })
  
  test_that("Regex dos substitutivos", {
    expect_true(stringr::str_detect("substitutivo", regex_documento_substitutivo))
    expect_true(stringr::str_detect("substitutivo adotado pela comissão", regex_documento_substitutivo))
    expect_true(stringr::str_detect("substitutivo adotado pela comissao", regex_documento_substitutivo))
  })
  
}

if(check_api()){
  test()
} else testthat::skip('Erro no setup!')