#!/usr/bin/env Rscript
library(magrittr)

help <- "
Usage:
Rscript fetch_autorias_bancada.R <bancada_ids_filepath> <ano> <output_filepath>
"

## Process args
args <- commandArgs(trailingOnly = TRUE)
min_num_args <- 3
if (length(args) < min_num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

bancada_ids_filepath <- args[1]
ano <- args[2]
output_filepath <- args[3]

ano_atual <- lubridate::year(lubridate::today())

if (ano < 2001 | ano > ano_atual) {
  stop(paste0("Ano inválido: ",ano,". Deve estar entre 2001 e ",ano_atual))
}

bancada <- readr::read_csv(bancada_ids_filepath,
                           col_types = list(
                             .default = readr::col_character(),
                             id_deputado = readr::col_double()
                           ))
bancada_ids <- bancada %>% dplyr::select(id_deputado) %>% dplyr::distinct()

props_autorias <- readr::read_csv2(
  paste0('http://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-',ano,'.csv'),
  col_types = list(
    .default = readr::col_character(),
    idProposicao = readr::col_double(),
    idDeputadoAutor = readr::col_double(),
    codTipoAutor = readr::col_double(),
    ordemAssinatura = readr::col_double(),
    proponente = readr::col_double()
  )) %>% 
  dplyr::filter(!is.na(idDeputadoAutor),
                proponente == 1)

props_bancada <- props_autorias %>% 
  dplyr::inner_join(bancada_ids, by=c("idDeputadoAutor"="id_deputado")) %>% 
  dplyr::select(id_prop = idProposicao, id_dep = idDeputadoAutor, nome_dep = nomeAutor, 
                partido = siglaPartidoAutor, uf = siglaUFAutor, ordem_assinatura = ordemAssinatura)

props_2019 <- readr::read_csv2(
  paste0('http://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-',ano,'.csv'),
  col_types = list(
    .default = readr::col_character(),
    id = readr::col_double(),
    numero = readr::col_double(),
    ano = readr::col_double(),
    codTipo = readr::col_double(),
    dataApresentacao = readr::col_datetime(format = ""),
    ultimoStatus_dataHora = readr::col_datetime(format = ""),
    ultimoStatus_sequencia = readr::col_double(),
    ultimoStatus_idOrgao = readr::col_double(),
    ultimoStatus_idTipoTramitacao = readr::col_double(),
    ultimoStatus_idSituacao = readr::col_double()
  )) %>%
  agoradigital::rename_df_columns() %>% 
  dplyr::select(id_documento = id, sigla_tipo, numero, ano, data_apresentacao, ementa, 
                ementa_detalhada, keywords, link_documento = url_inteiro_teor) %>% 
  dplyr::filter(sigla_tipo %in% c("PL","PEC"))


props_bancada_final <- props_2019 %>%
  dplyr::inner_join(props_bancada, by=c("id_documento"="id_prop")) %>% 
  dplyr::mutate(nome_formal_doc = paste0(sigla_tipo," ",numero,ifelse(ano > 0,paste0("/",ano),"")),
                nome_formal_autor = paste0(nome_dep,
                                           " - ",
                                           ifelse(!is.na(partido),paste0(" ",partido),""),
                                           ifelse(!is.na(uf),paste0("/",uf),""))) %>% 
  dplyr::select(id_autor = id_dep, nome_formal_autor, id_documento, nome_formal_doc, ordem_assinatura,
                data_apresentacao, ementa, ementa_detalhada, keywords, link_documento) %>% 
  dplyr::arrange(nome_formal_autor, nome_formal_doc, data_apresentacao)
  
readr::write_csv(props_bancada_final, output_filepath)
