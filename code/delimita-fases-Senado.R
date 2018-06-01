library(dplyr)

bill_passage_91341 <- read.csv("91341-passage-senado.csv") %>% arrange(data_tramitacao)

phase_one <- c('^Este processo contém')
phase_two <- 91
phase_three <- 42
phase_four <- 52

extract_phase <- function(dataframe) {
  dataframe <- dataframe %>%
        mutate(fase = case_when( (grepl(phase_one, texto_tramitacao)) ~ 1,
                                 (situacao_codigo_situacao == phase_two) ~ 2,
                                 (situacao_codigo_situacao == phase_three) ~ 3,
                                 (situacao_codigo_situacao == phase_four) ~ 4))
}

replace_na_with_last <-function(x,a=!is.na(x)){
  x[which(a)[c(1,1:sum(a))][cumsum(a)+1]]
}

bill_passage_91341 <- extract_phase(bill_passage_91341) %>% arrange(data_tramitacao, numero_ordem_tramitacao)
bill_passage_91341$fase <- replace_na_with_last(bill_passage_91341$fase)
