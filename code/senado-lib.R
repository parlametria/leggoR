source(here::here("code/congresso-lib.R"))

fetch_voting <- function(bill_id){
    
    url_base_voting <- "http://legis.senado.leg.br/dadosabertos/materia/votacoes/"
    
    url <- paste0(url_base_voting, bill_id)
    json_voting <- jsonlite::fromJSON(url, flatten = T)
    voting_data <- 
      json_voting %>% 
      magrittr::extract2("VotacaoMateria") %>% 
      magrittr::extract2("Materia")
    voting_ids <-
        voting_data %>% 
        magrittr::extract2("IdentificacaoMateria") %>% 
        tibble::as.tibble()
    voting_df <-
        voting_data %>% 
        magrittr::extract2("Votacoes") %>% 
        purrr::map_df(~ .) %>% 
        tidyr::unnest()
    
    voting_df <-
      voting_df %>%
        dplyr::select(
            -c(
                SessaoPlenaria.NomeCasaSessao,
                Tramitacao.IdentificacaoTramitacao.DestinoTramitacao.Local.NomeCasaLocal,
                Tramitacao.IdentificacaoTramitacao.OrigemTramitacao.Local.NomeLocal,
                Tramitacao.IdentificacaoTramitacao.DestinoTramitacao.Local.NomeLocal,
                Tramitacao.IdentificacaoTramitacao.OrigemTramitacao.Local.NomeCasaLocal,
                Tramitacao.IdentificacaoTramitacao.Situacao.SiglaSituacao,
                IdentificacaoParlamentar.EmailParlamentar,
                IdentificacaoParlamentar.NomeCompletoParlamentar,
                IdentificacaoParlamentar.FormaTratamento,
                IdentificacaoParlamentar.UrlFotoParlamentar,
                IdentificacaoParlamentar.UrlPaginaParlamentar,
                IdentificacaoParlamentar.EmailParlamentar
            )
        ) %>% 
      tibble::add_column(!!! voting_ids)
    
    rename_voting_df(voting_df)
}

fetch_passage <- function(bill_id){
    
    url_base_passage <- "http://legis.senado.leg.br/dadosabertos/materia/movimentacoes/"
    
    url <- paste0(url_base_passage, bill_id, sep = "")
    json_passage <- jsonlite::fromJSON(url, flatten = T)
    passage_data <- 
      json_passage %>% 
      magrittr::extract2("MovimentacaoMateria") %>% 
      magrittr::extract2("Materia")
    passage_ids <-
      passage_data %>% 
      magrittr::extract2("IdentificacaoMateria") %>% 
      tibble::as.tibble()
    passage_actual_situation <-
      passage_data %>% 
      magrittr::extract2("SituacaoAtual") %>% 
      magrittr::extract2("Autuacoes") %>% 
      magrittr::extract2("Autuacao") %>% 
      magrittr::extract2("Situacao") %>% 
      tibble::as.tibble()
    bill_passages_df <- 
      passage_data %>%
      magrittr::extract2("Tramitacoes") %>%
      magrittr::extract2("Tramitacao") %>%
      tibble::as.tibble()
  
    bill_passages_df <-
      bill_passages_df %>%
      dplyr::select(
          -c(
             IdentificacaoTramitacao.OrigemTramitacao.Local.NomeCasaLocal,
             IdentificacaoTramitacao.OrigemTramitacao.Local.NomeLocal,
             IdentificacaoTramitacao.DestinoTramitacao.Local.NomeCasaLocal,
             IdentificacaoTramitacao.DestinoTramitacao.Local.NomeLocal,
             IdentificacaoTramitacao.Situacao.SiglaSituacao,
             #,Textos.Texto,
             Publicacoes.Publicacao
            )
          ) %>%
          tibble::add_column(!!! passage_ids)
    
    rename_passage_df(bill_passages_df)
}

fetch_bill <- function(bill_id){
  
  url_base_bill <- "http://legis.senado.leg.br/dadosabertos/materia/"
  
  url <- paste0(url_base_bill, bill_id)
  json_bill <- jsonlite::fromJSON(url, flatten = T)
  bill_data <- 
    json_bill$DetalheMateria$Materia
  bill_ids <-
    bill_data$IdentificacaoMateria %>%
    tibble::as.tibble()  %>%
    dplyr::select(CodigoMateria, SiglaSubtipoMateria, NumeroMateria)
  bill_basic_data <-
    bill_data$DadosBasicosMateria %>%
    tibble::as.tibble()
  bill_author <-
    bill_data$Autoria$Autor %>%
    tibble::as.tibble() %>%
    dplyr::select(NomeAutor)
  bill_specific_subject <-
    bill_data$Assunto$AssuntoEspecifico %>%
    tibble::as.tibble() %>%
    dplyr::rename(assunto_especifico = Descricao, codigo_assunto_especifico = Codigo)
  bill_general_subject <-
    bill_data$Assunto$AssuntoGeral %>%
    tibble::as.tibble() %>%
    dplyr::rename(assunto_geral = Descricao, codigo_assunto_geral = Codigo)
  bill_source <-
    bill_data$OrigemMateria %>%
    tibble::as.tibble()
  bill_anexadas <- 
    bill_data$MateriasAnexadas$MateriaAnexada$IdentificacaoMateria.CodigoMateria %>%
    paste(collapse = ' ')
  
  bill_complete <-
    bill_basic_data %>%
    tibble::add_column(!!! bill_ids, !!! bill_author,
               !!! bill_specific_subject, !!! bill_general_subject, !!! bill_source)
  
  bill_complete$proposicoes_apensadas <- bill_anexadas
  
  rename_bill_df(bill_complete)
}

fetch_relatorias <- function(bill_id) {
  
  url_relatorias <- "http://legis.senado.leg.br/dadosabertos/materia/relatorias/"
  
  url <- paste0(url_relatorias, bill_id)
  json_relatorias <- jsonlite::fromJSON(url, flatten = T)
  
  #extract relatores objects
  relatorias_data <-
    json_relatorias %>%
    magrittr::extract2("RelatoriaMateria") %>%
    magrittr::extract2("Materia") %>%
    magrittr::extract2("HistoricoRelatoria")
  
 
  relatorias_df <-
    relatorias_data %>% 
    magrittr::extract2("Relator") %>% 
    as.data.frame() %>%
    purrr::map_df(~ .) %>% 
    tidyr::unnest()
  
  #select columns
  relatorias_df <-
    relatorias_df %>%
    dplyr::select(
      DataDesignacao,
      DataDestituicao,
      DescricaoMotivoDestituicao,
      IdentificacaoParlamentar.CodigoParlamentar,
      IdentificacaoParlamentar.NomeParlamentar,
      IdentificacaoParlamentar.SiglaPartidoParlamentar,
      IdentificacaoComissao.NomeComissao,
      IdentificacaoComissao.SiglaComissao,
      IdentificacaoComissao.CodigoComissao
    ) %>% 
    
    tibble::add_column()
  
  rename_relatorias_df(relatorias_df)
}

fetch_current_relatoria <- function(bill_id) {
  
  url_relatorias <- "http://legis.senado.leg.br/dadosabertos/materia/relatorias/"
  
  url <- paste0(url_relatorias, bill_id)
  json_relatorias <- jsonlite::fromJSON(url, flatten = T)
  
  #extract relatores objects
  relatorias_data <-
    json_relatorias %>%
    magrittr::extract2("RelatoriaMateria") %>%
    magrittr::extract2("Materia")
  
  current_relatoria_df <-
    relatorias_data %>% 
    magrittr::extract2("HistoricoRelatoria") %>%
    magrittr::extract2("Relator") %>% 
    purrr::map_df(~ .) %>% 
    tidyr::unnest()

  
  #fixing bug when api repeats relatorias
  current_relatoria_df <- current_relatoria_df[1,]
  # print(colnames(current_relatoria_df))
  
  #verify if relator atual exists
  if(ncol(current_relatoria_df) == 0){
    return(rename_relatoria(data.frame(matrix(ncol = 7, nrow = 1))))
  }
  
 
  #select columns
  current_relatoria_df <-
    current_relatoria_df %>%
    dplyr::select(
      DataDesignacao,
      IdentificacaoParlamentar.CodigoParlamentar,
      IdentificacaoParlamentar.NomeParlamentar,
      IdentificacaoParlamentar.SiglaPartidoParlamentar,
      IdentificacaoComissao.NomeComissao,
      IdentificacaoComissao.SiglaComissao,
      IdentificacaoComissao.CodigoComissao
    ) %>% 
    
    tibble::add_column()
  
  rename_relatoria(current_relatoria_df)
}

fetch_last_relatoria <- function(bill_id) {
  relatoria <- fetch_relatorias(bill_id)
  relatoria <- relatoria[1,]
  
  relatoria

}

rename_relatorias_df <- function(df) {
  names(df) <- c("data_designacao", "data_destituicao", "descricao_motivo_destituicao", "codigo_parlamentar",
                 "nome_parlamentar", "partido", "comissao", "sigla_comissao", "codigo_comissao")
  df
}

rename_relatoria <- function(df) {
  names(df) <- c("data_designacao", "codigo_parlamentar", "nome_parlamentar", "partido", 
                 "comissao", "sigla_comissao", "codigo_comissao")
  df
}

rename_voting_df <- function(df) {
  new_names = names(df) %>% 
    to_underscore() %>% 
    stringr::str_replace("sessao_plenaria_|tramitacao_identificacao_tramitacao_|identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

rename_passage_df <- function(df) {
  new_names = names(df) %>% 
    to_underscore() %>% 
    stringr::str_replace("identificacao_tramitacao_|
                identificacao_tramitacao_origem_tramitacao_local_|
                identificacao_tramitacao_destino_tramitacao_local_|
                identificacao_tramitacao_situacao_", "")
  
  names(df) <- new_names
  
  df
}

rename_bill_df <- function(df) {
  new_names = names(df) %>% 
    to_underscore() %>% 
    stringr::str_replace("identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

get_nome_ementa_Senado <- function(bill_id) {
  
  bill <- fetch_bill(bill_id)
  bill %>%
    dplyr::select(ementa_materia, sigla_subtipo_materia, numero_materia) %>%
    unique
}

tail_descricao_despacho_Senado <- function(df, qtd=1) {
  
  df %>% 
    dplyr::arrange(data_tramitacao) %>% 
    tail(qtd) %>% 
      dplyr::select(data_tramitacao, situacao_descricao_situacao, texto_tramitacao)
}

extract_phase_Senado <- function(dataframe, phase_one, phase_two, phase_three, phase_four) {
  
  dataframe %>%
    dplyr::mutate(fase = dplyr::case_when( grepl(phase_one, texto_tramitacao) ~ 'iniciativa',
                             detect_phase(situacao_codigo_situacao, phase_two) ~ 'relatoria',
                             detect_phase(situacao_codigo_situacao, phase_three) ~ 'discussao_deliberacao',
                             detect_phase(situacao_codigo_situacao, phase_four) ~ 'virada_de_casa')) 
}

extract_event_Senado <- function(tramitacao_df, phases_df) {
  
  dplyr::left_join(tramitacao_df, phases_df, by = "situacao_codigo_situacao")
}

extract_n_last_events_Senado <- function(df, num) {
  
  df %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::arrange(data_tramitacao) %>%
    tail(n = num) %>%
    dplyr::select(data_tramitacao, evento)
}