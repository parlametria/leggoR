library(tidyverse)
library(stringr)
source(here::here("R/senado-lib.R"))
source(here::here("Controller/fetcher.R"))

#' @title Processa dados de uma proposição do senado.
#' @description Recebido um bill_id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/Senado.
#' @param bill_id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' process_proposicao(91341)
#' @export
process_proposicao <- function(bill_id){
  bill_passage <- read_csv(paste0(here::here("data/Senado/"), bill_id, "-tramitacao-senado.csv")) %>% arrange(data_tramitacao)

  phase_one <- c('^Este processo contém')
  recebimento_phase <- 'recebido na|nesta comissão'
  phase_two <- c(91)
  phase_three <- c(42, 14, 78, 90)
  encaminhamento_phase <- c(89, 158, 159, 160, 161, 162, 163)
  phase_four <- c(52)

  bill_passage <- 
    extract_fase_Senado(bill_passage, phase_one, recebimento_phase, phase_two, phase_three, encaminhamento_phase, phase_four) %>% 
    arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    fill(fase) 

  bill_passage$situacao_descricao_situacao <- 
    to_underscore(bill_passage$situacao_descricao_situacao) %>% 
    str_replace_all("\\s+","_")
  
  bill_passage <- 
    extract_fase_casa_Senado(bill_passage, phase_one) %>% 
    arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    fill(casa) %>%
    filter(!is.na(casa))

  important_events <- frame_data(~ evento, ~ situacao_codigo_situacao,
            "aprovacao_audiencia_publica", 110,
            "aprovacao_parecer", 89,
            "aprovacao_substitutivo", 113,
            "pedido_vista", 90,
            "aprovacao_projeto", 25,
            "devolvido", 87)


  bill_passage <- extract_evento_Senado(bill_passage, important_phases) %>%
    extract_devolvidos_event_Senado()
  index_of_camara <- ifelse(length(which(bill_passage$situacao_codigo_situacao == 52)) == 0, 
                             nrow(bill_passage),
                             which(bill_passage$situacao_codigo_situacao == 52)[1])
  bill_passage <- 
    bill_passage[1:index_of_camara, ] %>% 
    extract_locais() %>%
    extract_fase_global(bill_id) %>%
    filter(!is.na(fase))
  
  bill_passage %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-fases-tramitacao-senado.csv"))

  bill_passage_visualization <- 
    bill_passage %>%
    select(data_tramitacao, local, fase, evento, casa, global)

  # Print evento freq table
  bill_passage_visualization %>% select(evento) %>% group_by(evento) %>%
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>%
    arrange(-frequência)


  bill_passage_visualization %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-visualizacao-tramitacao-senado.csv"))
}

extract_devolvidos_event_Senado <- function(df){
  devolvido_regex <- '^devolvido pel. redistribu.'
  df %>%
    mutate(
      evento = case_when(stringr::str_detect(tolower(texto_tramitacao), regex(devolvido_regex), ignore_case = TRUE)) ~ "devolvido")
}
