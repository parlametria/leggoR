#!/usr/bin/env Rscript
library(magrittr)

.help <- "
Usage:
Rscript build_semanario.R <data_inicio> <data_fim> <input_base_folderpath> <pops_base_folderpath>
"

is_package_installed <- function(pkg_name, rep_url)
{
  if (!require(pkg_name,character.only = TRUE))
  {
    install.packages(pkg_name,dep=TRUE, repos = rep_url)
    if(!require(pkg_name,character.only = TRUE)) stop("Package not found")
  }
}

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 4
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", .help, sep = "\n"))
}
data_inicio <- args[1]
data_fim <- args[2]
input_base_folderpath <- args[3]
pops_base_folderpath <- args[4]

#devtools::install()

is_package_installed(pkg_name = "ggchicklet", rep_url = "https://cinc.rud.is")

num_semanas_passadas <<- 12
MIN_NUM_DOCS <<- 5

#Lê dados básicos sobre as proposições e temperatura
proposicoes <- readr::read_csv(paste0(input_base_folderpath,'/','proposicoes.csv'))
trams <- readr::read_csv(paste0(input_base_folderpath, '/', 'trams.csv')) 
temperaturas <- readr::read_csv(paste0(input_base_folderpath,'/','hists_temperatura.csv'))
leggo_ids <- proposicoes %>% dplyr::select(id_leggo, id_ext, apelido, tema)

#Define semanas a serem analisadas
semana_alvo <- lubridate::floor_date(as.Date(data_inicio), unit = "weeks", week_start = 1)
fim_semana_alvo <- semana_alvo + lubridate::days(4)
semana_anterior <- semana_alvo - lubridate::weeks(x=1)
periodo_inicial <- semana_alvo - lubridate::weeks(x=24)

#Calcula variacao das temperaturas
get_variacoes_temperatura <- function(temperaturas) {
  variacoes_temp <- temperaturas %>%
    dplyr::mutate(variacao_absoluta = dplyr::if_else(dplyr::lag(id_leggo) == id_leggo, temperatura_recente - dplyr::lag(temperatura_recente), 0)) %>%
    dplyr::mutate(variacao_percentual = dplyr::if_else((dplyr::lag(id_leggo) == id_leggo & temperatura_recente > 0), ((temperatura_recente - dplyr::lag(temperatura_recente))/temperatura_recente) * 100, 0)) %>%
    dplyr::mutate(variacao_percentual = dplyr::if_else(is.na(variacao_percentual), 0, variacao_percentual)) %>%
    dplyr::filter(periodo == semana_alvo) %>%
    dplyr::mutate(z_score = (variacao_absoluta - mean(variacao_absoluta))/sd(variacao_absoluta))
}

variacoes_temperatura <- get_variacoes_temperatura(temperaturas)

#Filtra pls de interesse de acordo com a variação da temperatura
get_pls_interesse <- function(variacoes_temperatura, z_score_lim = 1.5) {
  variacoes_temperatura %>%
    dplyr::filter(z_score > z_score_lim)
}

pls_de_interesse <- get_pls_interesse(variacoes_temperatura, 0.4)

if (!('id_leggo' %in% names(trams))) {
  trams <- trams %>% 
    dplyr::inner_join(leggo_ids %>% dplyr::select(id_leggo, id_ext), by='id_ext') %>% 
    dplyr::select(id_leggo, dplyr::everything())
}

eventos <-
  trams %>% 
  dplyr::filter(data >= lubridate::ymd(data_inicio), 
         data <= lubridate::ymd(data_fim), 
         id_leggo %in% pls_de_interesse$id_leggo,
         !is.na(titulo_evento)) %>% 
  dplyr::select(id_leggo, data, titulo_evento, texto_tramitacao, sigla_local)

#Temperaturas filtradas
temperatura_pls_filtradas <- temperaturas %>%
  dplyr::filter(id_leggo %in% pls_de_interesse$id_leggo) %>% 
  dplyr::filter(periodo <= semana_alvo) %>% 
  dplyr::select(-temperatura_periodo) %>% 
  dplyr::rename(temperatura = temperatura_recente) %>% 
  dplyr::mutate(periodo = as.Date(periodo))

get_pressao_pls_interesse <- function(id_leggo, pops_folderpath) {
  pop <- readr::read_csv(paste0(pops_folderpath,"/pop_", id_leggo, ".csv"))
  pressao_pl_interesse <- pop %>%
    dplyr::mutate(id_leggo = as.numeric(id_leggo)) %>% 
    dplyr::group_by(id_leggo, id_ext, casa, date) %>% 
    dplyr::summarise(pressao = as.numeric(max(maximo_geral))) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(id_leggo, periodo = date, pressao) %>% 
    dplyr::mutate(periodo = as.Date(periodo))
}

#Pressões filtradas
pressao_pls_filtradas <- purrr::map_df(pls_de_interesse$id_leggo, ~ get_pressao_pls_interesse(.x, pops_base_folderpath))

join_temp_pressao_pl <- function(id_leggo_pl, temperatura_df, pressao_df, periodo_inicial) {
  temp_pressao_pl <- tibble::tibble()
  
  temperatura_pl <- temperatura_df %>% 
    dplyr::filter(id_leggo == id_leggo_pl,
                  periodo >= periodo_inicial)
  
  pressao_pl <- pressao_df %>% 
    dplyr::filter(id_leggo == id_leggo_pl,
                  periodo >= periodo_inicial)
  
  temp_pressao_pl <- dplyr::inner_join(temperatura_pl, pressao_pl, by =c("id_leggo","periodo")) %>% 
    dplyr::select(id_leggo, periodo, temperatura, pressao) %>% 
    dplyr::mutate(pressao = dplyr::if_else(pressao == 0, 0.1, pressao))
  
  if (nrow(temp_pressao_pl) == 0) {
    temp_pressao_pl <- temperatura_pl %>% 
      dplyr::mutate(pressao = 0.1)
  }
  
  return(temp_pressao_pl)
}

temp_pressao_periodo <- tibble::tibble()

if (nrow(pls_de_interesse) != 0) {
  temp_pressao_periodo <- purrr::map_df(pls_de_interesse$id_leggo, 
                                        ~ join_temp_pressao_pl(.x, 
                                                               temperatura_pls_filtradas, 
                                                               pressao_pls_filtradas,
                                                               periodo_inicial))  
}

#Documentos e autores
camara_docs <- agoradigital::read_current_docs_camara(paste0(input_base_folderpath, "/camara/documentos.csv"))
camara_autores <- agoradigital::read_current_autores_camara(paste0(input_base_folderpath, "/camara/autores.csv"))
senado_docs <- agoradigital::read_current_docs_senado(paste0(input_base_folderpath, "/senado/documentos.csv"))
senado_autores <- agoradigital::read_current_autores_senado(paste0(input_base_folderpath, "/senado/autores.csv"))

print(paste("Gerando tabela de atores a partir de dados atualizados de documentos e autores..."))

#Gerando atores para semana de interesse (ou faixa de tempo de interesse)
atores_camara <-
  agoradigital::create_tabela_atores_camara(camara_docs, camara_autores, data_inicio, data_fim)
atores_senado <- agoradigital::create_tabela_atores_senado(senado_docs, senado_autores, data_inicio, data_fim)

atores_df <- dplyr::bind_rows(atores_camara, atores_senado) %>% 
  dplyr::left_join(leggo_ids, by="id_ext")

atores_por_pl <- atores_df %>% 
  dplyr::group_by(id_leggo) %>% 
  dplyr::summarise(num_docs = sum(num_documentos)) %>% 
  dplyr::arrange(desc(num_docs))

pls_de_interesse_atores <- atores_por_pl %>% 
  dplyr::filter(num_docs >= MIN_NUM_DOCS)

atores_pls_filtradas <- atores_df %>% 
  dplyr::filter(id_leggo %in% pls_de_interesse_atores$id_leggo)

leggo_ids_selecionados <- 
  dplyr::bind_rows(dplyr::select(temp_pressao_periodo, id_leggo),
                   dplyr::select(atores_pls_filtradas, id_leggo)) %>% 
  dplyr::distinct()

proposicoes_filtradas <-
  proposicoes %>% 
  dplyr::filter(id_leggo %in% leggo_ids_selecionados$id_leggo) %>% 
  dplyr::mutate(ano = format(as.Date(data_apresentacao), "%Y")) %>% 
  dplyr::mutate(nome_formal = paste0(sigla_tipo, ' ', numero, '/', as.character(ano))) %>% 
  dplyr::mutate(nome_formal_completo = paste0(nome_formal, ' (', tools::toTitleCase(ifelse(casa == "camara","Câmara",casa)), ')'))

oposicao <- c("PT", "PSOL", "PSB", "PCdoB", "PDT", "REDE")

#Gerando datasets de atores para temas e bancadas
atores_geral <-
  atores_pls_filtradas %>% 
  dplyr::mutate(partido = dplyr::if_else(is.na(partido), "-", partido), 
         uf = dplyr::if_else(is.na(uf), "-", uf),
         nome_completo = paste0(nome_autor, " (",partido,"/",uf,")")) %>% 
  dplyr::left_join(proposicoes_filtradas %>% 
                     dplyr::select(id_leggo, id_ext, casa, nome_formal, nome_formal_completo, sigla_tipo), by=c("id_leggo","id_ext","casa")) %>% 
                     dplyr::mutate(bancada = dplyr::if_else(partido %in% oposicao, "Oposição", "Governo"))

atores_pls <- atores_geral %>% 
  dplyr::group_by(id_leggo, id_ext, casa, nome_autor, id_autor, tipo_autor, partido, nome_completo,
                  uf, tipo_generico, sigla_local, is_important, bancada, nome_formal, nome_formal_completo, sigla_tipo, tema) %>% 
  dplyr::summarise(peso_total_documentos = sum(peso_total_documentos)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(tema = stringr::str_replace_all(tema, ';',' &\n'))

#' @title Exporta dados de Proposições
#' @description Captura e escreve todos os dados referentes a proposiçoes
#' @param output_filepath
#' @param proposicoes
#' @param temperatura_pls_filtradas
#' @param pressao_pls_filtradas
#' @param atores_df
#' @param data_inicio
#' @param data_fim
#' @export
build_semanario <- function(template_filepath,output_filepath, proposicoes, temp_pressao_periodo,
                            eventos, atores_periodo, data_inicio, data_fim, num_semanas_passadas) {
  rmarkdown::render(input = template_filepath, 
                    output_file = output_filepath,
                    params = list(
                              proposicoes = proposicoes,
                              temp_pressao_periodo = temp_pressao_periodo,
                              eventos = eventos,
                              atores = atores_periodo,
                              date_init = data_inicio,
                              date_end = data_fim,
                              num_semanas_passadas = num_semanas_passadas
                             ))
}

template_filepath <- "reports/semanario/semanario_template.Rmd"
report_filepath <- paste0("semanario_",semana_alvo,"_",fim_semana_alvo,".html")

build_semanario(template_filepath, report_filepath, proposicoes_filtradas, temp_pressao_periodo,
                eventos, atores_pls, data_inicio, data_fim, num_semanas_passadas)
