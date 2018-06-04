# SETUP
install.packages("vistime")

library("vistime")
import_data <- read.csv(file="MyData.csv")
import_data["color"] <- NA

import_data$color <- c(map(import_data$fase, function(fase) case_when(
    fase == "iniciativa" ~ "#7fc97f",
    fase == "relatoria" ~ "#beaed4",
    fase == "discussao_deliberacao" ~ "#fdc086",
    fase == "virada_de_casa" ~ "#ffff99"
  ))
)

import_data <- import_data%>%
  mutate(
    fase = case_when(
      fase == "iniciativa" ~ "Iniciativa",
      fase == "relatoria" ~ "Relatoria",
      fase == "discussao_deliberacao" ~ "Discussão e Deliberação",
      fase == "virada_de_casa" ~ "Troca de Casa"
    )
  )

datavis <- data.frame(Fase = import_data$fase,
                   Events = import_data$texto_tramitacao,
                   start = import_data$data_tramitacao,
                   end = import_data$data_tramitacao,
                   color = import_data$color)
vistime(datavis, events="Events", groups="Fase", title="Fases da tramitação", colors="color", lineInterval=60*60*24*365, showLabels=FALSE)
