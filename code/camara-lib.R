get_nome_ementa_Camara <- function(bill_id) {
  require(dplyr)
  require(rcongresso)
  fetch_proposicao(pl6726_id) %>% select(ementa, siglaTipo, numero)
}

tail_descricao_despacho_Camara <- function(df, qtd=1) {
  require(dplyr)
  df %>% 
    arrange(data_hora) %>% 
    tail(qtd) %>% 
    select(data_hora, descricao_tramitacao, despacho)
}

extract_relator_Camara <- function(df) {
  require(dplyr)
  require(stringr)
  
  df %>% 
    mutate(relator = case_when(str_detect(tolower(despacho), '^designad. relat.r') ~ str_extract(despacho, regex('dep.+', ignore_case=TRUE))))
}
