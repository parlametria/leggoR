library(tidyverse)
source(here::here("code/senado-lib.R"))

#' @title Processa dados de uma proposição do senado.
#' @description Recebido um bill_id a função recupera informações sobre uma proposição
#' e sua tramitação e as salva em data/Senado.
#' @param bill_id Identificador da proposição que pode ser recuperado no site da câmara.
#' @examples
#' process_proposicao(91341)
#' @export
process_proposicao <- function(bill_id){
  bill_passage <- read_csv(paste0(here::here("data/Senado/"), bill_id, "-passage-senado.csv")) %>% arrange(data_tramitacao)

  phase_one <- c('^Este processo contém')
  phase_two <- c(91, 99)
  phase_three <- c(42, 110, 88)
  phase_four <- c(52)

  bill_passage <- 
    extract_fase_Senado(bill_passage, phase_one, phase_two, phase_three, phase_four) %>% 
    arrange(data_tramitacao, numero_ordem_tramitacao) %>%
    fill(fase) %>%
    filter(!is.na(fase))

  bill_passage$situacao_descricao_situacao <- 
    to_underscore(bill_passage$situacao_descricao_situacao) %>% 
    str_replace_all("\\s+","_")

  important_phases <- frame_data(~ evento, ~ situacao_codigo_situacao,
            "aprovacao_audiencia_publica", 110,
            "aprovacao_parecer", 89,
            "aprovacao_substitutivo", 113,
            "pedido_vista", 90,
            "aprovacao_projeto", 25)


  bill_passage <- extract_evento_Senado(bill_passage, important_phases)
  bill_passage %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-bill-passage-phases-senado.csv"))

  bill_passage_visualization <- 
    bill_passage %>%
    select(data_tramitacao, local = origem_tramitacao_local_sigla_local, fase, evento)

  # Print evento freq table
  bill_passage_visualization %>% select(evento) %>% group_by(evento) %>%
    filter(!is.na(evento)) %>% summarise(frequência = n()) %>%
    arrange(-frequência)


  bill_passage_visualization %>%
    write_csv(paste0(here::here("data/Senado/"), bill_id, "-bill-passage-visualization-senado.csv"))
}
