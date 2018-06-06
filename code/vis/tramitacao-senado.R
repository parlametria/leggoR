# SETUP
# install.packages("vistime")

library(vistime)
library(dplyr)
library(purrr)

data <- read_csv("data/bill_passage_91341_visualization")

# Create data frame to display phases inline
format_phase_data <- function(df) {
  df <- 
    # Improve the phases names and convert data_tramitacao to Date
    df %>%
    mutate(
      # fase = case_when(
      #   fase == "iniciativa" ~ "Iniciativa",
      #   fase == "relatoria" ~ "Relatoria",
      #   fase == "discussao_deliberacao" ~ "Discussão e Deliberação",
      #   fase == "virada_de_casa" ~ "Troca de Casa"
      # ),
      data_tramitacao = as.Date(data_tramitacao),
      local = as.character(origem_tramitacao_local_sigla_local)
    )
  
    df %>%
      mutate(z = cumsum(local != lag(local, default='NULL')), 
             end_data = lead(data_tramitacao)) %>%
      group_by(local, sequence = data.table::rleid(z)) %>%
      summarize(start = min(data_tramitacao),
                end = max(end_data),
                time_interval = end - start) %>%
      ungroup() %>% 
      arrange(sequence) %>% 
      select(-sequence) %>%
      filter(time_interval > 0)
    #%>%
      # mutate(
      #   line = "Local",
      #   color = case_when(
      #     fase == "Iniciativa" ~ "#7fc97f",
      #     fase == "Relatoria" ~ "#beaed4",
      #     fase == "Discussão e Deliberação" ~ "#fdc086",
      #     fase == "Troca de Casa" ~ "#ffff99"
      #   ))
}

fase_rows <- format_phase_data(data)

# Create chart with all tasks.
vistime(fase_rows, events="local", colors="color", groups="line", title="Fases da tramitação", showLabels=FALSE)


######################### TARGET ############################################

tramitacao_data <- data.frame(level = rep("LOCAL",4),
                              start = c("2017-12-3", "2017-12-4", "2018-3-5", "2018-4-12"),
                              end = c("2017-12-4", "2018-3-5", "2018-4-12", "2018-4-13"),
                              event = c("SEC", "CCJ", "CAE", "PLEN"))
vistime(tramitacao_data, events = "event", groups="local")

