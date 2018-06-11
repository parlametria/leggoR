get_nome_ementa_Camara <- function(bill_id) {
  require(dplyr)
  require(rcongresso)
  fetch_proposicao(pl6726_id) %>% select(ementa, siglaTipo, numero)
}

tail_descricao_despacho_Camara <- function(df, qtd=1) {
  df[(nrow(df)-(qtd-1)):nrow(df), c('data_hora', 'descricao_tramitacao', 'despacho')]
}