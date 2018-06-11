get_nome_ementa_Camara <- function(bill_id) {
  require(dplyr)
  require(rcongresso)
  fetch_proposicao(pl6726_id) %>% select(ementa, siglaTipo, numero)
}

