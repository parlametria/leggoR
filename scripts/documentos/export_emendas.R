#' @title Exporta dados de emendas para análise
#' @description Processa e escreve os dados de emendas
#' @param camara_docs documentos da camara
#' @param senados_docs documentos do senado
#' @param camara_autores autores da camara
#' @param senado_autores autores do senado
#' @param output_path pasta para onde exportar os dados
#' @return dataframe com novas emendas a serem analisadas
export_emendas <- function(camara_docs, camara_autores, senado_docs, senado_autores, output_path) {
  print(paste("Gerando tabela de emendas para análise a partir de dados atualizados de documentos e autores..."))
  
  emendas_camara <- camara_docs %>% dplyr::filter(sigla_tipo %in% agoradigital::get_envvars_camara()$tipos_emendas$sigla) %>% 
    dplyr::inner_join(camara_autores %>% 
                        mutate(id_principal = as.numeric(id_principal)), 
                      by=c('id_principal', 'id_documento', 'casa')) %>% 
    dplyr::mutate(nome_partido_uf = paste0(nome, ' ', partido, '/', uf),
                  data_apresentacao = as.Date(data_apresentacao)) %>% 
    dplyr::group_by(id_ext = id_principal, 
                    codigo_emenda = id_documento, 
                    data_apresentacao, 
                    numero, 
                    local = status_proposicao_sigla_orgao, 
                    casa, 
                    tipo_documento = sigla_tipo,
                    inteiro_teor = url_inteiro_teor) %>% 
    dplyr::summarise(autor = paste(nome_partido_uf, collapse = ', ')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(id_ext, codigo_emenda, data_apresentacao, numero, local, autor, casa, tipo_documento, inteiro_teor)
  
  emendas_senado <- senado_docs %>% dplyr::filter(descricao_tipo_texto %in% agoradigital::get_envvars_senado()$tipos_emendas$descricao_tipo_texto) %>% 
    dplyr::inner_join(senado_autores, by=c('id_principal','id_documento','casa')) %>% 
    dplyr::mutate(nome_partido_uf = paste0(nome_autor, ' ', partido, '/', uf),
                  numero = as.integer(numero_emenda),
                  data_apresentacao = lubridate::ymd(data_texto)) %>% 
    dplyr::group_by(id_ext = id_principal, 
                    codigo_emenda = id_documento, 
                    data_apresentacao, 
                    numero, 
                    local = identificacao_comissao_sigla_comissao, 
                    casa, 
                    tipo_documento = descricao_tipo_texto,
                    inteiro_teor = url_texto) %>% 
    dplyr::summarise(autor = paste(nome_partido_uf, collapse = ', ')) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(id_ext, codigo_emenda, data_apresentacao, numero, local, autor, casa, tipo_documento, inteiro_teor)
  
  emendas <- dplyr::bind_rows(emendas_camara, emendas_senado)
  
  novas_emendas <- agoradigital::update_emendas_files(emendas, output_path)
}

#' @title Exporta dados de avulsos iniciais para análise
#' @description Processa e escreve os dados de avulsos iniciais
#' @param camara_docs documentos da camara
#' @param senados_docs documentos do senado
#' @param novas_emendas novas emendas a serem analisadas
#' @param output_path pasta para onde exportar os dados
export_avulsos_iniciais <- function(camara_docs, senado_docs, novas_emendas, output_path) {
  print(paste("Gerando tabela de avulsos iniciais dos PLs para análise a partir de dados atualizados de documentos..."))
  
  av_iniciais_camara <- camara_docs %>% dplyr::filter(id_documento == id_principal) %>% 
    dplyr::mutate(data = as.Date(data_apresentacao)) %>% 
    dplyr::select(id_proposicao = id_principal,
                  casa,
                  codigo_texto = id_documento,
                  data,
                  tipo_texto = descricao_tipo_documento,
                  inteiro_teor = url_inteiro_teor) %>% 
    dplyr::distinct()
  
  
  av_iniciais_senado <- senado_docs %>% dplyr::filter(stringr::str_detect(tolower(descricao_texto), 'avulso inicial da mat.ria')) %>% 
    dplyr::select(id_proposicao = id_principal,
                  casa,
                  codigo_texto = id_documento,
                  data = data_texto,
                  tipo_texto = descricao_tipo_texto,
                  inteiro_teor = url_texto)
  
  av_iniciais <- dplyr::bind_rows(av_iniciais_camara, av_iniciais_senado)
  
  av_iniciais_novas_emendas <- av_iniciais %>% 
    dplyr::inner_join(novas_emendas %>% 
                        dplyr::select(id_proposicao = id_ext, casa) %>% 
                        dplyr::distinct(), 
                      by=c('id_proposicao','casa'))
  
  
  readr::write_csv(av_iniciais, paste0(output_path, '/', 'avulsos_iniciais.csv'))
  readr::write_csv(av_iniciais_novas_emendas, paste0(output_path, '/', 'avulsos_iniciais_novas_emendas.csv'))
}
