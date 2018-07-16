#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%

COMISSOES_CAMARA <- data.frame("comissao" = c("Comissão de Agricultura, Pecuária, Abastecimento e Desenvolvimento Rural",
                                              "Comissão de Ciência e Tecnologia, Comunicação e Informática",
                                              "Comissão de Constituição e Justiça e de Cidadania",
                                              "Comissão de Cultura",
                                              "Comissão de Defesa do Consumidor",
                                              "Comissão de Defesa dos Direitos da Mulher",
                                              "Comissão de Defesa dos Direitos da Pessoa Idosa",
                                              "Comissão de Defesa dos Direitos das Pessoas com Deficiência",
                                              "Comissão de Desenvolvimento Urbano",
                                              "Comissão de Desenvolvimento Econômico, Indústria, Comércio e Serviços",
                                              "Comissão de Direitos Humanos e Minorias",
                                              "Comissão de Educação",
                                              "Comissão do Esporte",
                                              "Comissão de Finanças e Tributação",
                                              "Comissão de Fiscalização Financeira e Controle",
                                              "Comissão de Integração Nacional, Desenvolvimento Regional e da Amazônia",
                                              "Comissão de Legislação Participativa",
                                              "Comissão de Meio Ambiente e Desenvolvimento Sustentável",
                                              "Comissão de Minas e Energia",
                                              "Comissão de Relações Exteriores e de Defesa Nacional",
                                              "Comissão de Segurança Pública e Combate ao Crime Organizado",
                                              "Comissão de Seguridade Social e Família",
                                              "Comissão de Trabalho, de Administração e Serviço Público",
                                              "Comissão de Turismo",
                                              "Comissão de Viação e Transportes"),
                               "regex"=c(c("comiss.o de agricultura, pecu.ria, abastecimento e desenvolvimento rural", "capadr"),
                                         c("comiss.o de ci.ncia e tecnologia, comunica..o e informatica", "cctci"),
                                         c("comiss.o de constitui..o e justi.a e de cidadania", "ccjc"),
                                         c("comiss.o de cultura", "ccult"),
                                         c("comiss.o de defesa do consumidor", "cdc"),
                                         c("comiss.o de defesa dos direitos da mulher", "cmulher"),
                                         c("comiss.o de defesa dos direitos da pessoa idosa", "cidoso"),
                                         c("comiss.o de defesa dos direitos das pessoas com defici.ncia", "cpd"),
                                         c("comiss.o de desenvolvimento urbano", "cdu"),
                                         c("comiss.o de desenvolvimento econ.mico, ind.stria, com.rcio e servi.os", "cdeics"),
                                         c("comiss.o de direitos humanos e minorias", "cdhm"),
                                         c("comiss.o de educa..o", "ce"),
                                         c("comiss.o do esporte", "cespo"),
                                         c("comiss.o de finan.as e tributa..o", "cft"),
                                         c("comiss.o de fiscaliza..o financeira e controle", "cffc"),
                                         c("comiss.o de integra..o nacional, desenvolvimento regional e da amaz.nia", "cindra"),
                                         c("comiss.o de legisla..o participativa", "clp"),
                                         c("comiss.o de meio ambiente e desenvolvimento sustent.vel", "cmads"),
                                         c("comiss.o de minas e energia", "cme"),
                                         c("comiss.o de rela..es exteriores e de defesa nacional", "credn"),
                                         c("comiss.o de seguran.a p.blica e combate ao crime organizado", "cspcco"),
                                         c("comiss.o de seguridade social e fam.lia", "cssf"),
                                         c("comiss.o de trabalho, de administra..o e servi.o p.blico", "ctasp"),
                                         c("comiss.o de turismo", "ctur"),
                                         c("comiss.o de via..o e transportes", "cvt")))

#Senado
.COLNAMES_VOT_SEN <- c("indicador_votacao_secreta" , "descricao_votacao" , "descricao_resultado" , "codigo_sessao" ,                              
"sigla_casa_sessao" , "nome_casa_sessao" , "codigo_sessao_legislativa" , "sigla_tipo_sessao" , "numero_sessao" , 
"data_sessao" , "hora_inicio_sessao" , "codigo_tramitacao" , "numero_autuacao" , "data_tramitacao" , 
"numero_ordem_tramitacao" , "texto_tramitacao" , "indicador_recebimento" , "data_recebimento" ,                          
 "origem_tramitacao_local_codigo_local" , "origem_tramitacao_local_tipo_local" , "origem_tramitacao_local_sigla_casa_local" ,  
"origem_tramitacao_local_nome_casa_local" , "origem_tramitacao_local_sigla_local" , "origem_tramitacao_local_nome_local" ,        
"destino_tramitacao_local_codigo_local" , "destino_tramitacao_local_tipo_local" , "destino_tramitacao_local_sigla_casa_local" , 
"destino_tramitacao_local_nome_casa_local" , "destino_tramitacao_local_sigla_local" , "destino_tramitacao_local_nome_local" ,       
 "situacao_codigo_situacao" , "situacao_sigla_situacao" , "situacao_descricao_situacao" , "descricao_voto" , "codigo_parlamentar" , 
"nome_parlamentar" , "nome_completo_parlamentar" , "sexo_parlamentar" , "forma_tratamento"  ,                         
 "url_foto_parlamentar" , "url_pagina_parlamentar" , "email_parlamentar" , "sigla_partido_parlamentar" , "uf_parlamentar" , 
"codigo_materia" , "sigla_casa_identificacao_materia" , "nome_casa_identificacao_materia" , "sigla_subtipo_materia" ,                     
 "descricao_subtipo_materia" , "numero_materia" , "ano_materia" , "indicador_tramitando" )

.COLNAMES_TRAMI_SEN <- c("codigo_tramitacao", "numero_autuacao", "data_tramitacao", "numero_ordem_tramitacao", "texto_tramitacao", "indicador_recebimento", "data_recebimento",  "origem_tramitacao_local_codigo_local", 
"origem_tramitacao_local_tipo_local", "origem_tramitacao_local_sigla_casa_local", "origem_tramitacao_local_nome_casa_local", "origem_tramitacao_local_sigla_local", "origem_tramitacao_local_nome_local",
"destino_tramitacao_local_codigo_local", "destino_tramitacao_local_tipo_local", "destino_tramitacao_local_sigla_casa_local", "destino_tramitacao_local_nome_casa_local", "destino_tramitacao_local_sigla_local", 
"destino_tramitacao_local_nome_local", "situacao_codigo_situacao", "situacao_sigla_situacao", "situacao_descricao_situacao", "codigo_materia", "sigla_casa_identificacao_materia", "nome_casa_identificacao_materia",
"sigla_subtipo_materia", "descricao_subtipo_materia", "numero_materia", "ano_materia", "indicador_tramitando")

.COLNAMES_DEFE_SEN <- c("proposicao_id", "deferimento")

.COLNAMES_RELATORAIS_SEN <- c("codigo_tipo_relator","descricao_tipo_relator", "data_designacao", "data_destituicao", "descricao_motivo_destituicao", "codigo_parlamentar", "nome_parlamentar", "nome_completo_parlamentar",
 "sexo_parlamentar", "forma_tratamento", "url_foto_parlamentar", "url_pagina_parlamentar", "email_parlamentar", "sigla_partido_parlamentar", "uf_parlamentar", "codigo_comissao", "sigla_comissao", "nome_comissao", "sigla_casa_comissao",
 "nome_casa_comissao")

.COLNAMES_EMENTA_SEN <- c("ementa_materia", "sigla_subtipo_materia", "numero_materia")

.COLNAMES_DESPACHO_SEN <- c("data_tramitacao", "situacao_descricao_situacao", "texto_tramitacao")

.COLNAMES_DESPACHO_SEN <- c("codigo_materia", "comissoes", "data_tramitacao")