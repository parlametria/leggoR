library(here)
source(here("code/congresso-lib.R"))

#' @title Busca votações de uma proposição no Senado
#' @description Retorna dataframe com os dados das votações de uma proposição no Senado
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as votações de uma proposição no Senado
#' @examples
#' fetch_voting(91341)
#' @export
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

#' @title Busca a movimentação da proposição
#' @description Retorna dataframe com os dados da movimentação da proposição, incluindo tramitação, prazos, despachos e situação
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre a movimentação de uma proposição no Senado
#' @examples
#' fetch_passage(91341)
#' @export
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
        #,Textos.Texto,
        Publicacoes.Publicacao
      )
    ) %>%
    add_column(!!! passage_ids)
  
  rename_passage_df(bill_passages_df)
}

#' @title Recupera os deatlhes de uma proposição no Senado
#' @description Retorna dataframe com os dados detalhados da proposição, incluindo número, ementa, tipo e data de apresentação
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações detalhadas de uma proposição no Senado
#' @examples
#' fetch_bill(91341)
#' @export
fetch_bill <- function(bill_id){
  require(tidyverse)
  require(magrittr)
  library(jsonlite)
  
  url_base_bill <- "http://legis.senado.leg.br/dadosabertos/materia/"
  
  url <- paste0(url_base_bill, bill_id)
  json_bill <- fromJSON(url, flatten = T)
  bill_data <- 
    json_bill$DetalheMateria$Materia
  bill_ids <-
    bill_data$IdentificacaoMateria %>%
    as.tibble  %>%
    select(CodigoMateria, SiglaSubtipoMateria, NumeroMateria)
  bill_basic_data <-
    bill_data$DadosBasicosMateria %>%
    as.tibble
  bill_author <-
    bill_data$Autoria$Autor %>%
    as.tibble %>%
    select(NomeAutor)
  bill_specific_subject <-
    bill_data$Assunto$AssuntoEspecifico %>%
    as.tibble %>%
    rename(assunto_especifico = Descricao, codigo_assunto_especifico = Codigo)
  bill_general_subject <-
    bill_data$Assunto$AssuntoGeral %>%
    as.tibble %>%
    rename(assunto_geral = Descricao, codigo_assunto_geral = Codigo)
  bill_source <-
    bill_data$OrigemMateria %>%
    as.tibble
  bill_anexadas <- 
    bill_data$MateriasAnexadas$MateriaAnexada$IdentificacaoMateria.CodigoMateria %>%
    paste(collapse = ' ')
  
  bill_complete <-
    bill_basic_data %>%
    add_column(!!! bill_ids, !!! bill_author,
               !!! bill_specific_subject, !!! bill_general_subject, !!! bill_source)
  
  bill_complete$proposicoes_apensadas <- bill_anexadas
  
  rename_bill_df(bill_complete)
}

#' @title Recupera o histórico de relatorias de uma proposição no Senado
#' @description Retorna dataframe com o histórico de relatorias detalhado de uma proposição no Senado, incluindo a data
#' de designação e destituição, o relator e seu partido e a comissão
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações detalhadas do histórico de relatorias de uma proposição no Senado
#' @examples
#' fetch_relatorias(91341)
#' @export
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
    as.data.frame() %>%
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

#' @title Recupera a relatoria atual no Senado
#' @description Retorna dataframe com a relatoria atual no senado, contendo a data de designação, o relator e seu partido e a comissão
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações da relatoria atual no Senado
#' @examples
#' fetch_current_relatoria(91341)
#' @export
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
    as.data.frame() %>%
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

#' @title Recupera a última relatoria de uma proposição no Senado
#' @description Retorna dataframe com a última relatoria de uma proposição no Senado, incluindo a data
#' de designação e destituição, o relator e seu partido e a comissão
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações da última relatoria de uma proposição no Senado
#' @examples
#' fetch_relatorias(91341)
#' @export
fetch_last_relatoria <- function(bill_id) {
  relatoria <- fetch_relatorias(bill_id)
  relatoria <- relatoria[1,]
  
  relatoria
  
}

#' @title Renomeia as colunas do dataframe do histórico de relatorias no Senado
#' @description Renomeia as colunas do dataframe do histórico de relatorias no Senado usando o padrão 
#' de underscore e letras minúsculas
#' @param df Dataframe do histórico de relatorias
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_relatorias_df()
#' @export
rename_relatorias_df <- function(df) {
  names(df) <- c("data_designacao", "data_destituicao", "descricao_motivo_destituicao", "codigo_parlamentar",
                 "nome_parlamentar", "partido", "comissao", "sigla_comissao", "codigo_comissao")
  df
}

#' @title Renomeia as colunas do dataframe de relatoria atual no Senado
#' @description Renomeia as colunas do dataframe de relatoria atual no Senado usando o padrão 
#' de underscore e letras minúsculas
#' @param df Dataframe da relatoria atual no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_relatoria()
#' @export
rename_relatoria <- function(df) {
  names(df) <- c("data_designacao", "codigo_parlamentar", "nome_parlamentar", "partido", 
                 "comissao", "sigla_comissao", "codigo_comissao")
  df
}

#' @title Renomeia as colunas do dataframe de votação no Senado
#' @description Renomeia as colunas do dataframe de votação no Senado usando o padrão 
#' de underscore e letras minúsculas
#' @param df Dataframe da votação no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_voting_df()
#' @export
rename_voting_df <- function(df) {
  new_names = names(df) %>% 
    to_underscore() %>% 
    str_replace("sessao_plenaria_|tramitacao_identificacao_tramitacao_|identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

#' @title Renomeia as colunas do dataframe de movimentação no Senado
#' @description Renomeia as colunas do dataframe de movimentação no Senado usando o padrão 
#' de underscore e letras minúsculas
#' @param df Dataframe da votação no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_passage_df()
#' @export
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

#' @title Renomeia as colunas do dataframe dos detalhes da proposição no Senado
#' @description Renomeia as colunas do dataframe dos detalhes da proposição no Senado usando o padrão 
#' de underscore e letras minúsculas
#' @param df Dataframe dos detalhes da proposição no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' df %>% rename_bill_df()
#' @export
rename_bill_df <- function(df) {
  new_names = names(df) %>% 
    to_underscore() %>% 
    str_replace("identificacao_parlamentar_", "")
  
  names(df) <- new_names
  
  df
}

#' @title Recupera o numero e a ementa de uma proposição no Senado
#' @description Retona um dataframe de uma proposição no senado contendo o número, a ementa e o tipo da proposição
#' @param df ID da proposição no Senado
#' @return Dataframe com as colunas renomeadas
#' @examples
#' get_nome_ementa_Senado(91341)
#' @export
get_nome_ementa_Senado <- function(bill_id) {
  require(tidyverse)
  
  bill <- fetch_bill(bill_id)
  bill %>%
    select(ementa_materia, sigla_subtipo_materia, numero_materia) %>%
    unique
}

#' @title Recupera os n últimos despachos no Senado
#' @description Retorna um dataframe das últimas n tramitações no Senado contendo a data, a descrição e o despacho 
#' @param df Dataframe da tramitação no Senado
#' @param qtd  (opcional) Quantidade de eventos a serem recuperados
#' @return Dataframe com as últimas n tramitações no Senado.
#' @examples
#' tramitacao %>% tail_descricao_despacho_Senado()
#' tramitacao %>% tail_descricao_despacho_Senado(4)
#' @export
tail_descricao_despacho_Senado <- function(df, qtd=1) {
  require(tidyverse)
  
  df %>% 
    arrange(data_tramitacao) %>% 
    tail(qtd) %>% 
    select(data_tramitacao, situacao_descricao_situacao, texto_tramitacao)
}

#' @title Cria coluna com as fases da tramitação no Senado
#' @description Cria uma nova coluna com as fases no Senado
#' @param df Dataframe da tramitação no Senado
#' @return Dataframe com a coluna "fase" adicionada.
#' @examples
#' tramitacao %>% extract_phase_Senado()
#' @export
extract_phase_Senado <- function(dataframe, phase_one, phase_two, phase_three, phase_four) {
  require(tidyverse)
  
  dataframe %>%
    mutate(fase = case_when( grepl(phase_one, texto_tramitacao) ~ 'iniciativa',
                             detect_phase(situacao_codigo_situacao, phase_two) ~ 'relatoria',
                             detect_phase(situacao_codigo_situacao, phase_three) ~ 'discussao_deliberacao',
                             detect_phase(situacao_codigo_situacao, phase_four) ~ 'virada_de_casa')) 
}

#' @title Extrai os eventos importantes que aconteceram no Senado
#' @description Adiciona coluna ao dataframe com os eventos mais importantes que aconteceram no Senado
#' @param tramitacao_df Dataframe da tramitação no Senado
#' @param events_df Dataframe com os eventos contendo as colunas "evento" e "regex"
#' @return Dataframe com a coluna "evento" adicionada.
#' @examples
#' df %>% extract_event_Senado(importants_events)
#' @export
extract_event_Senado <- function(tramitacao_df, phases_df) {
  require(tidyverse)
  
  left_join(tramitacao_df, phases_df, by = "situacao_codigo_situacao")
}

#' @title Recupera os n últimos eventos importantes que aconteceram no Senado
#' @description Retona dataframe contendo os n últimos eventos importantes que aconteceram no Senado
#' @param tramitacao_df Dataframe da tramitação no Senado
#' @param num Quantidade de eventos a serem recuperados
#' @return Dataframe contendo os n últimos eventos importantes que aconteceram no Senado
#' @examples
#' df %>% extract_n_last_events_Senado(4)
#' @export
extract_n_last_events_Senado <- function(df, num) {
  require(tidyverse)
  
  df %>%
    filter(!is.na(evento)) %>%
    arrange(data_tramitacao) %>%
    tail(n = num) %>%
    select(data_tramitacao, evento)
}