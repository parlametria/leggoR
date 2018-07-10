library(vistime)
library(dplyr)

pl_3729_2004_data_estado <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color,
                                       "Sob Analise", "Estado", "2004-06-08", "2007-01-30","#c8e6c9",
                                       "Arquivado", "Estado", "2007-01-31", "2007-04-25","#DD4B39",
                                       "Sob Analise", "Estado", "2007-04-26", "2011-01-30","#c8e6c9",
                                       "Arquivado", "Estado", "2011-01-31", "2011-02-14","#DD4B39",
                                       "Sob Analise", "Estado", "2011-02-15", "2015-01-30","#c8e6c9",
                                       "Arquivado", "Estado", "2015-01-31", "2015-02-04","#DD4B39",
                                       "Sob Analise", "Estado", "2015-02-05", "2018-06-12","#c8e6c9")


pl_3729_2004_data_global <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color,
                                       "Tramitação - Casa de Origem", "Global", "2004-06-08", "2018-06-12","#fb8c00")

pl_3729_2004_data_casa_origem <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color,
                                            "Apresentação", "Casa Origem", "2004-06-08", "2004-06-18","#c8e6c9",
                                            "Comissões", "Casa Origem", "2004-06-22", "2007-01-30","#fb8c00",
                                            "Comissões", "Casa Origem", "2007-04-26", "2011-01-30","#fb8c00",
                                            "Comissões", "Casa Origem", "2011-02-15", "2018-06-12","#fb8c00")

pl_3729_2004_data_comissoes <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color,
                                          "CMADS", "Comissões", "2004-06-22", "2007-01-24","#a6cee3",
                                          "CMADS", "Comissões", "2007-05-31", "2014-01-06","#a6cee3",
                                          "CAPADR", "Comissões", "2014-02-10", "2014-05-14","#1f78b4",
                                          "CMADS", "Comissões", "2014-05-16", "2014-12-18","#a6cee3",
                                          "CMADS", "Comissões", "2015-04-09", "2015-10-14","#a6cee3",
                                          "CFT", "Comissões", "2015-10-20", "2018-07-05","#b2df8a")

pl_3729_2004_data_comissao <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color,
                                         #CMADS
                                         "Receb.", "Comissão", "2004-06-22", "2004-07-07","#a6cee3",
                                         "Análise do Relator", "Comissão", "2004-07-08", "2007-01-24","#a6cee3",
                                         "Análise do Relator", "Comissão", "2007-05-31", "2009-01-30","#a6cee3",
                                         "Análise do Relator", "Comissão", "2009-03-19", "2009-12-15","#a6cee3",
                                         "Análise do Relator", "Comissão", "2011-03-28", "2013-10-22","#a6cee3",
                                         "Análise do Relator", "Comissão", "2013-12-04", "2013-12-05","#a6cee3",
                                         
                                         #CAPADR
                                         "Receb.", "Comissão", "2014-02-10", "2014-03-11","#1f78b4",
                                         "Análise do Relator", "Comissão", "2014-03-12", "2014-04-28","#1f78b4",
                                         "Discussão e Votação", "Comissão", "2014-04-29", "2014-04-14","#1f78b4",
                                         
                                         #CMADS
                                         "Receb.", "Comissão", "2014-05-16", "2014-05-21","#a6cee3",
                                         "Análise do Relator", "Comissão", "2014-05-22", "2014-12-18","#a6cee3",
                                         "Análise do Relator", "Comissão", "2015-04-09", "2015-09-22","#a6cee3",
                                         "Discussão e Votação", "Comissão", "2015-10-14", "2015-10-16","#a6cee3",
                                         
                                         #CTF
                                         "Receb.", "Comissão", "2015-10-20", "2015-10-27","#b2df8a",
                                         "Análise do Relator", "Comissão", "2015-10-28", "2015-12-09","#b2df8a",
                                         "Análise do Relator", "Comissão", "2015-12-17", "2017-08-29","#b2df8a",
                                         "Análise do Relator", "Comissão", "2018-04-11", "2018-07-05","#b2df8a")

pl_3729_2004_data_eventos <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color,
                                        "Apresentação do PL", "Evento", "2004-06-08", "2004-06-08", "#938ecc",
                                        "Distribuição a comissões", "Evento", "2004-06-18", "2004-06-18", "#938ecc",
                                        "Designado relator", "Evento", "2004-07-07", "2004-07-07","#87c465",
                                        "Apensamento", "Evento", "2004-08-03", "2004-08-03","#fb8c00",
                                        "Apensamento", "Evento", "2005-06-24", "2005-06-24","#fb8c00",
                                        "Apensamento", "Evento", "2005-07-07", "2005-07-07","#fb8c00",
                                        "Arquivada",  "Evento", "2007-01-31", "2007-01-31", "#938ecc",
                                        "Desarquivada",  "Evento","2007-04-25", "2007-04-25", "#938ecc",
                                        "Designado relator", "Evento", "2007-05-31", "2007-05-31","#87c465",
                                        "Apensamento", "Evento", "2007-06-14", "2007-06-14","#fb8c00",
                                        "Apensamento", "Evento", "2007-09-27", "2007-09-27","#fb8c00",
                                        "Parecer", "Evento", "2009-01-30", "2009-01-30","#a561a2",
                                        "Designado relator", "Evento", "2009-03-19", "2009-03-19","#87c465",
                                        "Parecer", "Evento", "2009-12-16", "2009-12-16","#a561a2",
                                        "Arquivado",  "Evento", "2011-01-31", "2011-01-31", "#938ecc",
                                        "Desarquivada", "Evento", "2011-02-14", "2011-02-14", "#938ecc",
                                        "Designado relator", "Evento", "2011-03-28", "2011-03-28","#87c465",
                                        "Apensamento", "Evento", "2011-07-11", "2011-07-11","#fb8c00",
                                        "Reconstituição", "Evento", "2012-04-09", "2012-04-09", "#938ecc",
                                        "Apensamento", "Evento", "2012-07-02", "2012-07-02","#fb8c00",
                                        "Apensamento", "Evento", "2013-06-21", "2013-06-21","#fb8c00",
                                        "Parecer", "Evento", "2013-10-23", "2013-10-23","#a561a2",
                                        "Aprovação do requerimento de Redistribuição - CAPADR", "Evento", "2013-11-27","2013-11-27", "#938ecc",
                                        "Designado relator", "Evento", "2013-12-04", "2013-12-04","#87c465",
                                        "Parecer", "Evento", "2013-12-06", "2013-12-06","#a561a2",
                                        "Apensamento", "Evento", "2013-12-12", "2013-12-12", "#938ecc",
                                        "Parecer", "Evento", "2013-12-17", "2013-12-17","#a561a2",
                                        "Redistribuição - CAPADR", "Evento", "2013-12-20","2013-12-20", "#938ecc",
                                        "Designado relator", "Evento", "2014-03-12", "2014-03-12","#87c465",
                                        "Parecer", "Evento", "2014-04-29", "2014-04-29","#a561a2",
                                        "Parecer Aprovado",  "Evento", "2014-05-14", "2014-05-14","#a561a2",
                                        "Designado relator", "Evento", "2014-05-21", "2014-05-21","#87c465",
                                        "Apensamento", "Evento", "2014-11-06", "2014-11-06", "#938ecc",
                                        "Arquivada", "Evento", "2015-01-31","2015-01-31", "#938ecc",
                                        "Desarquivada", "Evento", "2015-02-04","2015-02-04", "#938ecc",
                                        "Designado relator", "Evento", "2015-04-09", "2015-04-09","#87c465",
                                        "Parecer", "Evento", "2015-09-23", "2015-09-23","#a561a2",
                                        "Pedido de vista", "Evento", "2015-10-07", "2015-10-07","#b1b03a",
                                        "Complementação Voto", "Evento", "2015-10-14", "2015-10-14","#b1b03a",
                                        "Parecer Aprovado",  "Evento", "2015-10-14", "2015-10-14","#a561a2",
                                        "Designado relator", "Evento", "2015-10-28", "2015-10-28","#87c465",
                                        "Designado relator", "Evento", "2015-12-17", "2015-12-17","#87c465",
                                        "Apensamento", "Evento", "2016-02-29", "2016-02-29", "#938ecc",
                                        "Alteração de regime", "Evento","2016-03-22", "2016-03-22", "#938ecc",
                                        "Designado relator CCJC", "Evento", "2016-06-30", "2016-06-30","#87c465",
                                        "Parecer CTF 1", "Evento", "2016-07-05", "2016-07-05","#a561a2",
                                        "Parecer CTF 2", "Evento", "2016-07-27", "2016-07-27","#a561a2",
                                        "Parecer CTF 3", "Evento", "2016-09-01", "2016-09-01","#a561a2",
                                        "Parecer CTF 4", "Evento", "2016-09-15", "2016-09-15","#a561a2",
                                        "Parecer CTF 5", "Evento", "2016-12-12", "2016-12-12","#a561a2",
                                        "Parecer CTF 6", "Evento", "2017-04-19", "2017-04-19","#a561a2",
                                        "Parecer CTF 7", "Evento", "2017-04-27", "2017-04-27","#a561a2",
                                        "Retirada de pauta", "Evento", "2017-05-03", "2017-05-03","#a561a2",
                                        "Parecer CTF 8", "Evento", "2017-06-05", "2017-06-05","#a561a2",
                                        "Retirada de pauta", "Evento", "2017-06-07", "2017-06-07","#a561a2",
                                        "Apensamento", "Evento", "2017-06-13", "2017-06-13", "#938ecc",
                                        "Parecer CTF 10", "Evento", "2017-08-04", "2017-08-04","#a561a2",
                                        "Parecer CTF 11", "Evento", "2017-08-08", "2017-08-08","#a561a2",
                                        "Parecer CTF 12", "Evento", "2017-08-08", "2017-08-08","#a561a2",
                                        "Apensamento", "Evento", "2017-08-09", "2017-08-09", "#938ecc",
                                        "Parecer CTF 13", "Evento", "2017-08-29", "2017-08-29","#a561a2",
                                        "Apensamento", "Evento", "2017-09-11", "2017-09-11", "#938ecc",
                                        "Desapensação", "Evento", "2017-10-18", "2017-10-18", "#938ecc",
                                        "Apensamento", "Evento", "2017-12-01", "2017-12-01", "#938ecc",
                                        "Designado relator", "Evento", "2018-04-11", "2018-04-11","#87c465")

# pl_3729_2004_data_general <- bind_rows(pl_3729_2004_data_estado,
#                                        pl_3729_2004_data_global,
#                                        pl_3729_2004_data_casa_origem,
#                                        pl_3729_2004_data_comissoes,
#                                        pl_3729_2004_data_comissao,
#                                        pl_3729_2004_data_eventos)

pl_3729_2004_data_general <- bind_rows(pl_3729_2004_data_comissoes,
                                       pl_3729_2004_data_comissao,
                                       pl_3729_2004_data_eventos)

vistime(pl_3729_2004_data_general)

#pl_3729_2004_data_detailed <- bind_rows(pl_3729_2004_data_comissoes,
#                                        pl_3729_2004_data_comissao)

#vistime(pl_3729_2004_data_detailed, showLabels=FALSE, title="Timeline das comissões")