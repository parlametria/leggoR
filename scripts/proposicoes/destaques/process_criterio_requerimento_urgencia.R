
#' @title Processa o critério de proposições com requerimento de urgência apresentado e/ou aprovado;
#' @description 
#' @param proposicoes_datapath Datapath de proposições
#' @param trams_datapath Datapath de tramitações
#' @return Dataframe contendo as proposições que passam no critério.

process_criterio_requerimento_urgencia <- function(trams_datapath = here::here("leggo_data/trams.csv"),
                                                   props_datapath = here::here("leggo_data/proposicoes.csv") ){
  trams <- read_csv(trams_datapath) %>% 
    select(evento, id_ext, casa)
  
  props <- read_csv(props_datapath) %>%
    select(id_ext, casa, id_leggo)
  
  proposicoes_requerimento_urgencia <- trams %>% 
  left_join(props, by = c("id_ext", "casa")) %>%  
  filter(evento == "requerimento_urgencia_aprovado", evento == "requerimento_urgencia_apresentado")
  
}

teste_csv_regex <- function(trams_datapath = here::here("leggo_data/trams.csv")){
  tramitacoes <- read_csv(trams_datapath)
  tramitacao_df <- tramitacoes
  
  camara_env <- jsonlite::fromJSON(here::here("R/config/environment_senado.json"))
  eventos_camara <- camara_env$eventos %>% dplyr::select(evento, regex)
  df <- tramitacao_df %>%
    dplyr::mutate(texto_lower = tolower(stringr::str_trim(
      stringr::str_replace_all(texto_tramitacao,'[\r\n]', '')))) %>%
    mutate(evento_original = evento) %>%
    fuzzyjoin::regex_left_join(eventos_camara, by = c(texto_lower = "regex")) %>%
    dplyr::select(-texto_lower, -regex)
  
  df %>%
    select(id_ext, texto_tramitacao, evento.x, evento.y) %>%
    distinct(texto_tramitacao, .keep_all = T) %>%
    view()
}
