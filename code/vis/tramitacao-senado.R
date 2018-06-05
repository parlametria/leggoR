# SETUP
install.packages("vistime")

library(vistime)
library(dplyr)
library(purrr)
library(lubridate)
data <- read.csv("data/bill_passage_91341_visualization")

data <- data %>%
  mutate(
    fase = case_when(
      fase == "iniciativa" ~ "Iniciativa",
      fase == "relatoria" ~ "Relatoria",
      fase == "discussao_deliberacao" ~ "Discussão e Deliberação",
      fase == "virada_de_casa" ~ "Troca de Casa"
    )
  )

data_fixed <- data %>%
  mutate(origem_tramitacao_local_sigla_local = as.character(origem_tramitacao_local_sigla_local)) %>%
  mutate(group_local = 2) 
  
  
data_fixed <- data_fixed %>% 
                  rowwise() %>%
                  do()
    mutate(group_local = ifelse(lag(fase,default='NULL') != fase,
                              lag(group_local,default=0) + 1,
                              lag(group_local)))
  #mutate(group_local = lag(origem_tramitacao_local_sigla_local,default='NULL'))
  
  #rowwise() %>%
  







datavis <- data.frame(Fase = import_data$fase,
                      Events = import_data$evento,
                      start = import_data$data_tramitacao,
                      end = import_data$data_tramitacao)
vistime(datavis, events="Events", groups="Fase", title="Fases da tramitação", lineInterval=60*60*24*365, showLabels=FALSE)




######################### TARGET ############################################

tramitacao_data <- data.frame(level = rep("LOCAL",4),
                              start = c("2017-12-3", "2017-12-4", "2018-3-5", "2018-4-12"),
                              end = c("2017-12-4", "2018-3-5", "2018-4-12", "2018-4-13"),
                              event = c("SEC", "CCJ", "CAE", "PLEN"))
vistime(tramitacao_data, events = "event", groups="local")
