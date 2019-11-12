# Obter Tipos de Requerimentos da Câmara

eventos_list <- jsonlite::fromJSON('https://dadosabertos.camara.leg.br/api/v2/referencias/tiposProposicao', simplifyVector = TRUE)[[1]] %>% 
  as.data.frame()

reqs_camara <- eventos_list %>% 
  dplyr::filter(startsWith(nome, "Requerimento")) %>% 
  dplyr::arrange(nome)

readr::write_csv(reqs_camara, 'data/tipos_reqs_camara.csv')

# O Senado não possui endpoint análoga nem dado estruturado sobre tipo de requerimento.



         