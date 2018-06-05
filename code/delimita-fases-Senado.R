library(dplyr)

bill_passage_91341 <- read.csv("data/91341-passage-senado.csv") %>% arrange(data_tramitacao)

phase_one <- c('^Este processo contém')
phase_two <- c(91, 99)
phase_three <- c(42, 110)
phase_four <- 52

extract_phase <- function(dataframe) {
  dataframe <- dataframe %>%
        mutate(fase = case_when( (grepl(phase_one, texto_tramitacao)) ~ 'iniciativa',
                                 (situacao_codigo_situacao %in% phase_two) ~ 'relatoria',
                                 (situacao_codigo_situacao %in% phase_three) ~ 'discussao_deliberacao',
                                 (situacao_codigo_situacao == phase_four) ~ 'virada_de_casa'))
}

replace_na_with_last <-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

bill_passage_91341 <- extract_phase(bill_passage_91341) %>% arrange(data_tramitacao, numero_ordem_tramitacao)
bill_passage_91341$fase <- replace_na_with_last(bill_passage_91341$fase) 

phase_aprovacao_audiencia <- 110
phase_aprovacao_parecer <- 89
phase_aprovacao_substitutivo <- 113

extract_event <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(evento = case_when( (situacao_codigo_situacao == phase_aprovacao_audiencia) ~ 'aprovacao_audiencia_publica',
                               (situacao_codigo_situacao == phase_aprovacao_parecer) ~ 'aprovacao_parecer',
                               (situacao_codigo_situacao == phase_aprovacao_substitutivo) ~ 'aprovacao_substitutivo'))
}

bill_passage_91341 <- extract_event(bill_passage_91341)
