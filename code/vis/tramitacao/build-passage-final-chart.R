library(vistime)
library(dplyr)

pl_490_2007_data_estado <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                               "Sob Análise", "Estado", "2007-03-20", "2011-01-30","#57d39b",  
                               "Arquivado", "Estado", "2011-01-31", "2011-02-15","#f37340",  
                               "Sob Análise", "Estado", "2011-02-16", "2015-01-30","#57d39b",  
                               "Arquivado", "Estado", "2015-01-31", "2015-02-09","#f37340",  
                               "Sob Análise", "Estado", "2015-02-10", "2018-06-12","#57d39b")

pl_490_2007_data_global <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                      "Tramitação - Casa de Origem", "Global", "2007-03-20", "2018-06-12","#87c465")

pl_490_2007_data_casa_origem <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                      "Apresentação", "Casa", "2007-03-20", "2007-03-28","#d6952a",
                                      "Comissões", "Casa", "2007-03-29", "2011-01-30","#938ecc",
                                      "Comissões", "Casa", "2011-02-16", "2015-01-30","#938ecc",
                                      "Comissões", "Casa", "2015-02-10", "2018-06-12","#938ecc")

pl_490_2007_data_comissoes <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                           "CDHM", "Comissões", "2007-03-29", "2007-11-12","#f37340",
                                           "CAPADR", "Comissões", "2007-11-19", "2008-07-02","#b1b03a",
                                           "CDHM", "Comissões", "2008-07-07", "2009-08-05","#f37340",
                                           "CCJC", "Comissões", "2009-08-07", "2018-06-12","#d6952a")

pl_490_2007_data_comissao <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                         "Receb.", "Comissão", "2007-03-29", "2007-04-10","#e08c2d",
                                         "Análise do Relator", "Comissão", "2007-04-11", "2007-08-26","#7ac974",
                                         "Discussão e Votação", "Comissão", "2007-08-27", "2007-11-12","#a561a2",
                                         "Receb.", "Comissão", "2007-11-19", "2007-11-27","#e08c2d",
                                         "Análise do Relator", "Comissão", "2007-11-28", "2008-05-12","#7ac974",
                                         "Discussão e Votação", "Comissão", "2008-05-13", "2008-07-02","#a561a2",
                                         "Receb.", "Comissão", "2008-07-07", "2008-08-03","#e08c2d",
                                         "Análise do Relator", "Comissão", "2008-08-04", "2009-06-21","#7ac974",
                                         "Discussão e Votação", "Comissão", "2009-06-22", "2009-08-05","#a561a2",
                                         "Receb.", "Comissão", "2009-08-07", "2010-03-03","#e08c2d",
                                         "Análise do Relator", "Comissão", "2010-03-04", "2011-01-31","#7ac974",
                                         "Receb.", "Comissão", "2011-02-10", "2013-05-13","#e08c2d",
                                         "Análise do Relator", "Comissão", "2013-05-14", "2015-01-31","#7ac974",
                                         "Receb.", "Comissão", "2015-02-10", "2018-01-31","#e08c2d",
                                         "Análise do Relator", "Comissão", "2018-02-01", "2018-04-09","#7ac974",
                                         "Discussão e Votação", "Comissão", "2018-04-10", "2018-06-12","#a561a2")

pl_490_2007_data_eventos <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                       "Distribuição às comissões", "Evento", "2007-03-28", "2007-03-28","#f9929f",
                                       "Designação do relator", "Evento", "2007-04-11", "2007-04-11","#87c465",
                                       "Apensamento", "Evento", "2007-07-22", "2007-07-22","#fb8c00",
                                       "Parecer", "Evento", "2007-08-27", "2007-08-27","#a561a2",
                                       "Parecer", "Evento", "2007-10-19", "2007-10-19","#a561a2",
                                       "Pedido de vista", "Evento", "2007-10-24", "2007-10-24","#b1b03a",
                                       "Redistribuição de comissões", "Evento", "2007-11-06", "2007-11-06","#f9929f",
                                       "Apensamento", "Evento", "2007-11-08", "2007-11-08","#fb8c00",
                                       "Apensamento", "Evento", "2007-11-08", "2007-11-08","#fb8c00",
                                       "Designação do relator", "Evento", "2007-11-28", "2007-11-28","#87c465",
                                       "Parecer", "Evento", "2008-03-27", "2008-03-27","#a561a2",
                                       "Parecer", "Evento", "2008-05-13", "2008-05-13","#a561a2",
                                       "Pedido de vista", "Evento", "2008-05-25", "2008-05-25","#b1b03a",
                                       "Voto em separado", "Evento", "2008-06-10", "2008-06-10","#7590ab",
                                       "Aprovado parecer", "Evento", "2008-07-02", "2008-07-02","#1bded1",
                                       "Designado relator", "Evento", "2008-08-04", "2008-08-04","#87c465",
                                       "Parecer", "Evento", "2009-06-22", "2009-06-22","#a561a2",
                                       "Aprovado parecer", "Evento", "2009-08-05", "2009-08-05","#1bded1",
                                       "Apensamento", "Evento", "2009-09-23", "2009-09-23","#fb8c00",
                                       "Designação do relator", "Evento", "2010-03-04", "2010-03-04","#87c465",
                                       "Apensamento", "Evento", "2011-10-25", "2011-10-25","#fb8c00",
                                       "Designação do relator", "Evento", "2013-05-14", "2013-05-14","#87c465",
                                       "Apensamento", "Evento", "2013-12-03", "2013-12-03","#fb8c00",
                                       "Designação do relator", "Evento", "2018-02-01", "2018-02-01","#87c465",
                                       "Parecer", "Evento", "2018-04-10", "2018-04-10","#a561a2",
                                       "Voto em separado", "Evento", "2018-05-22", "2018-05-22","#7590ab",
                                       "Retirado de pauta", "Evento", "2018-06-12", "2018-06-12","#bd0624")

pl_490_2007_data_general <- bind_rows(pl_490_2007_data_estado, 
                              pl_490_2007_data_global, 
                              pl_490_2007_data_casa_origem,
                              pl_490_2007_data_comissoes,
                              pl_490_2007_data_comissao,
                              pl_490_2007_data_eventos)



vistime(pl_490_2007_data_general)

