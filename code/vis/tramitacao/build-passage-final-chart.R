library(vistime)
library(dplyr)

pl_490_2007_data_estado <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                               "Sob Analise", "Estado", "2007-03-20", "2011-01-30","#c8e6c9",  
                               "Arquivado", "Estado", "2011-01-31", "2011-02-15","#DD4B39",  
                               "Sob Analise", "Estado", "2011-02-16", "2015-01-30","#c8e6c9",  
                               "Arquivado", "Estado", "2015-01-31", "2015-02-09","#DD4B39",  
                               "Sob Analise", "Estado", "2015-02-10", "2018-06-12","#c8e6c9")

pl_490_2007_data_global <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                      "Tramitação - Casa de Origem", "Global", "2007-03-20", "2018-06-12","#fb8c00")

pl_490_2007_data_casa_origem <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                      "Apresentação", "Casa Origem", "2007-03-20", "2007-03-28","#c8e6c9",
                                      "Comissões", "Casa Origem", "2007-03-29", "2011-01-30","#fb8c00",
                                      "Comissões", "Casa Origem", "2011-02-16", "2015-01-30","#fb8c00",
                                      "Comissões", "Casa Origem", "2015-02-10", "2018-06-12","#fb8c00")

pl_490_2007_data_comissoes <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                           "CDHM", "Comissões", "2007-03-29", "2007-11-12","#c8e6c9",
                                           "CAPADR", "Comissões", "2007-11-19", "2008-07-02","#fb8c00",
                                           "CDHM", "Comissões", "2008-07-07", "2009-08-05","#1565c0",
                                           "CCJC", "Comissões", "2009-08-07", "2018-06-12","#90caf9")

pl_490_2007_data_comissao <- frame_data(~ event, ~ group, ~ start, ~ end, ~ color, 
                                         "Receb.", "Comissão", "2007-03-29", "2007-04-10","#c8e6c9",
                                         "Análise do Relator", "Comissão", "2007-04-11", "2007-08-26","#fb8c00",
                                         "Discussão e Votação", "Comissão", "2007-08-27", "2007-11-12","#a5d6a7",
                                         "Receb.", "Comissão", "2007-11-19", "2007-11-27","#c8e6c9",
                                         "Análise do Relator", "Comissão", "2007-11-28", "2008-05-12","#fb8c00",
                                         "Discussão e Votação", "Comissão", "2008-05-13", "2008-07-02","#a5d6a7",
                                         "Receb.", "Comissão", "2008-07-07", "2008-08-03","#c8e6c9",
                                         "Análise do Relator", "Comissão", "2008-08-04", "2009-06-21","#fb8c00",
                                         "Discussão e Votação", "Comissão", "2009-06-22", "2009-08-05","#a5d6a7",
                                         "Receb.", "Comissão", "2009-08-07", "2010-03-03","#c8e6c9",
                                         "Análise do Relator", "Comissão", "2010-03-04", "2011-01-31","#fb8c00",
                                         "Receb.", "Comissão", "2011-02-10", "2013-05-13","#c8e6c9",
                                         "Análise do Relator", "Comissão", "2013-05-14", "2015-01-31","#fb8c00",
                                         "Receb.", "Comissão", "2015-02-10", "2018-01-31","#c8e6c9",
                                         "Análise do Relator", "Comissão", "2018-02-01", "2018-04-09","#fb8c00",
                                         "Discussão e Votação", "Comissão", "2018-04-10", "2018-06-12","#a5d6a7")

pl_490_2007_data_general <- bind_rows(pl_490_2007_data_estado, 
                              pl_490_2007_data_global, 
                              pl_490_2007_data_casa_origem,
                              pl_490_2007_data_comissoes)



vistime(pl_490_2007_data_general)

pl_490_2007_data_detailed <- bind_rows(pl_490_2007_data_comissoes,
                                       pl_490_2007_data_comissao)

vistime(pl_490_2007_data_detailed)