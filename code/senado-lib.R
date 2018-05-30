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
}