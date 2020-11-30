library(tidyverse)

process_proposicoes_destaques <- function(
  proposicoes_datapath = here::here("leggo_data/proposicoes.csv"),
  progressos_datapath = here::here("leggo_data/progressos.csv"),
  tramitacoes_datapath = here::here("leggo_data/trams.csv"),
  interesses_datapath = here::here("leggo_data/interesses.csv"),
  pressao_datapath = here::here("leggo_data/pressao.csv")) {
  library(lubridate)

  source(here::here("scripts/proposicoes/destaques/process_criterio_aprovada_em_uma_casa.R"))
  source(here::here("scripts/proposicoes/destaques/process_criterio_parecer_aprovado_comissao.R"))
  source(here::here("scripts/proposicoes/destaques/process_criterio_pressao_alta.R"))

  interesses <- read_csv(interesses_datapath) %>%
    group_by(id_leggo) %>%
    summarise(agendas = paste(interesse, collapse = ";")) %>%
    ungroup()

  proposicoes <- read_csv(proposicoes_datapath,
                          col_types = cols(id_ext = col_character())) %>%
    filter(status == "Ativa") %>%
    mutate(sigla = paste0(sigla_tipo, " ", numero, "/", year(ymd_hms(data_apresentacao)))) %>%
    select(id_leggo, id_ext, casa, sigla) %>%
    distinct()

  proposicoes_criterio_aprovada_em_uma_casa <-
    process_criterio_aprovada_em_uma_casa(proposicoes_datapath,
                                          progressos_datapath) %>%
    mutate(criterio_aprovada_em_uma_casa = T) %>%
    select(id_leggo, criterio_aprovada_em_uma_casa, fase_global, local, local_casa, data_inicio, data_fim)

  proposicoes_criterio_parecer_aprovado_comissao <-
    process_criterio_parecer_aprovado_comissao(proposicoes_datapath,
                                                 tramitacoes_datapath) %>%
    mutate(criterio_parecer_aprovado_comissao = T) %>%
    select(id_leggo, id_ext, casa, criterio_parecer_aprovado_comissao, comissoes_aprovadas)

  proposicoes_pressao_alta <-
    process_criterio_pressao_alta(pressao_datapath) %>%
    mutate(criterio_pressao_alta = T) %>%
    select(id_leggo, criterio_pressao_alta, maximo_pressao_periodo)

  proposicoes_destaques <- proposicoes %>%
    left_join(proposicoes_criterio_aprovada_em_uma_casa,
               by = c("id_leggo")) %>%
    left_join(proposicoes_criterio_parecer_aprovado_comissao,
              by = c("id_leggo", "id_ext", "casa")) %>%
    left_join(proposicoes_pressao_alta,
              by = c("id_leggo")) %>%
    mutate(criterio_aprovada_em_uma_casa = !is.na(criterio_aprovada_em_uma_casa),
           criterio_parecer_aprovado_comissao = !is.na(criterio_parecer_aprovado_comissao),
           criterio_pressao_alta = !is.na(criterio_pressao_alta)) %>%
    left_join(interesses, by = c("id_leggo"))

  return(proposicoes_destaques)

}
