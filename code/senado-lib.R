fetch_voting <- function(bill_id){
    require(tidyverse)
    require(magrittr)
    library(jsonlite)
    
    url_base_voting <- "http://legis.senado.leg.br/dadosabertos/materia/votacoes/"
    
    url <- paste0(url_base_voting, bill_id)
    json_voting <- fromJSON(url, flatten = T)
    voting_data <- 
        json_voting %>% 
        extract2("VotacaoMateria") %>% 
        extract2("Materia")
    voting_ids <-
        voting_data %>% 
        extract2("IdentificacaoMateria") %>% 
        as.tibble()
    voting_df <-
        voting_data %>% 
        extract2("Votacoes") %>% 
        map_df(~ .) %>% 
        unnest()
    
    voting_df <-
      voting_df %>%
        select(
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
        add_column(!!! voting_ids)
    
    rename_voting_df(voting_df)
}

fetch_passage <- function(bill_id){
    require(tidyverse)
    require(magrittr)
    library(jsonlite)
    
    url_base_passage <- "http://legis.senado.leg.br/dadosabertos/materia/movimentacoes/"
    
    url <- paste0(url_base_passage, bill_id, sep = "")
    json_passage <- fromJSON(url, flatten = T)
    passage_data <- 
      json_passage %>% 
      extract2("MovimentacaoMateria") %>% 
      extract2("Materia")
    passage_ids <-
      passage_data %>% 
      extract2("IdentificacaoMateria") %>% 
      as.tibble()
    passage_actual_situation <-
      passage_data %>% 
      extract2("SituacaoAtual") %>% 
      extract2("Autuacoes") %>% 
      extract2("Autuacao") %>% 
      extract2("Situacao") %>% 
      as.tibble()
    bill_passages_df <- 
      passage_data %>%
      extract2("Tramitacoes") %>%
      extract2("Tramitacao") %>%
      as.tibble()
  
    bill_passages_df <-
      bill_passages_df %>%
      select(
          -c(
             IdentificacaoTramitacao.OrigemTramitacao.Local.NomeCasaLocal,
             IdentificacaoTramitacao.OrigemTramitacao.Local.NomeLocal,
             IdentificacaoTramitacao.DestinoTramitacao.Local.NomeCasaLocal,
             IdentificacaoTramitacao.DestinoTramitacao.Local.NomeLocal,
             IdentificacaoTramitacao.Situacao.SiglaSituacao,
             Textos.Texto,
             Publicacoes.Publicacao
            )
          ) %>%
          add_column(!!! passage_ids)
    
    rename_passage_df(bill_passages_df)
}

fetch_bill <- function(bill_id){
  require(tidyverse)
  require(magrittr)
  library(jsonlite)
  
  url_base_bill <- "http://legis.senado.leg.br/dadosabertos/materia/"
  
  url <- paste0(url_base_bill, bill_id, sep = "")
  json_bill <- fromJSON(url, flatten = T)
  bill_data <- 
    json_bill %>% 
    extract2("DetalheMateria") %>% 
    extract2("Materia")
  bill_ids <-
    bill_data %>% 
    extract2("IdentificacaoMateria") %>% 
    as.tibble()  %>%
    select(CodigoMateria, SiglaSubtipoMateria, NumeroMateria)
  bill_basic_data <-
    bill_data %>%
    extract2("DadosBasicosMateria") %>%
    as.tibble()
  bill_author <-
    bill_data %>%
    extract2("Autoria") %>%
    extract2("Autor") %>%
    as.tibble() %>%
    select(NomeAutor, UfAutor, IdentificacaoParlamentar.CodigoParlamentar, IdentificacaoParlamentar.SiglaPartidoParlamentar)
  bill_specific_subject <-
    bill_data %>%
    extract2("Assunto") %>% 
    extract2("AssuntoEspecifico") %>%
    as.tibble() %>%
    rename(assunto_especifico = Descricao, codigo_assunto_especifico = Codigo)
  bill_general_subject <-
    bill_data %>%
    extract2("Assunto") %>% 
    extract2("AssuntoGeral") %>%
    as.tibble() %>%
    rename(assunto_geral = Descricao, codigo_assunto_geral = Codigo)
  bill_source <-
    bill_data %>%
    extract2("OrigemMateria") %>%
    as.tibble()
  
  bill_complete <- 
    bill_basic_data %>%
    add_column(!!! bill_ids, !!! bill_author, 
               !!! bill_specific_subject, !!! bill_general_subject, !!! bill_source)
  
  rename_bill_df(bill_complete)
}

fetch_relatorias <- function(bill_id) {
  require(tidyverse)
  require(magrittr)
  library(jsonlite)
  
  url_relatorias <- "http://legis.senado.leg.br/dadosabertos/materia/relatorias/"
  
  url <- paste0(url_relatorias, bill_id)
  json_relatorias <- fromJSON(url, flatten = T)
  
  #extract relatores objects
  relatorias_data <-
    json_relatorias %>%
    extract2("RelatoriaMateria") %>%
    extract2("Materia") %>%
    extract2("HistoricoRelatoria")
  
  
  relatorias_df <-
    relatorias_data %>% 
    extract2("Relator") %>% 
    map_df(~ .) %>% 
    unnest()
  
  #select columns
  relatorias_df <-
    relatorias_df %>%
    select(
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
    
    add_column()
  
  rename_relatorias_df(relatorias_df)
}

fetch_current_relatoria <- function(bill_id) {
  require(tidyverse)
  require(magrittr)
  library(jsonlite)
  
  url_relatorias <- "http://legis.senado.leg.br/dadosabertos/materia/relatorias/"
  
  url <- paste0(url_relatorias, bill_id)
  json_relatorias <- fromJSON(url, flatten = T)
  
  #extract relatores objects
  relatorias_data <-
    json_relatorias %>%
    extract2("RelatoriaMateria") %>%
    extract2("Materia")
  
  current_relatoria_df <-
    relatorias_data %>% 
    extract2("RelatoriaAtual") %>%
    extract2("Relator") %>% 
    map_df(~ .) %>% 
    unnest()
  
  #fixing bug when api repeats relatorias
  current_relatoria_df <- current_relatoria_df[1,]
  print(colnames(current_relatoria_df))
  
  #verify if relator atual exists
  if(ncol(current_relatoria_df) == 0){
    return(rename_relatoria(data.frame(matrix(ncol = 7, nrow = 1))))
  }
  
 
  #select columns
  current_relatoria_df <-
    current_relatoria_df %>%
    select(
      DataDesignacao,
      IdentificacaoParlamentar.CodigoParlamentar,
      IdentificacaoParlamentar.NomeParlamentar,
      IdentificacaoParlamentar.SiglaPartidoParlamentar,
      IdentificacaoComissao.NomeComissao,
      IdentificacaoComissao.SiglaComissao,
      IdentificacaoComissao.CodigoComissao
    ) %>% 
    
    add_column()
  
  rename_relatoria(current_relatoria_df)
}

to_underscore <- function(x) {
  x2 <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x3 <- gsub(".", "_", x2, fixed = TRUE)
  x4 <- gsub("([a-z])([A-Z])", "\\1_\\2", x3)
  x5 <- tolower(x4)
  x5
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
    str_replace("sessao_plenaria_|tramitacao_identificacao_tramitacao_|identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

rename_passage_df <- function(df) {
  new_names = names(df) %>% 
    to_underscore() %>% 
    str_replace("identificacao_tramitacao_|
                identificacao_tramitacao_origem_tramitacao_local_|
                identificacao_tramitacao_destino_tramitacao_local_|
                identificacao_tramitacao_situacao_", "")
  
  names(df) <- new_names
  
  df
}

rename_bill_df <- function(df) {
  new_names = names(df) %>% 
    to_underscore() %>% 
    str_replace("identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

get_nome_ementa_Senado <- function(bill_id) {
  bill <- fetch_bill(bill_id_Senado)
  bill %>% select(ementa_materia, sigla_subtipo_materia, numero_materia)
}

tail_descricao_despacho_Senado <- function(df, qtd=1) {
  df %>% 
  arrange(data_tramitacao) %>% 
  tail(qtd) %>% 
    select(data_tramitacao, situacao_descricao_situacao, texto_tramitacao)
}
