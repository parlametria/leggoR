# SETUP
# install.packages("vistime")

library(vistime)
library(tidyverse)

create_chart <- function(bill_id, house){
  # Create chart with all tasks.
  bill_id <- 2121442
  house <- 'camara'
  data <- read_csv(paste0("data/vis/tramitacao/",bill_id,"-data-", house, ".csv")) 
  
  
  # Custom tooltip
  data$tooltip <- ifelse(data$end == ymd(Sys.Date()), 
                         paste0("<b>", data$label, 
                                "</b> \n Início: ", data$start,
                                "<b>\n Em andamento </b>"),
                         paste0("<b>", data$label, 
                                "</b> \n Início: ", data$start,
                                "\n Fim: ", data$end))
  
  vistime(data, events="label", groups="group", title="Fases da tramitação", tooltips = "tooltip", colors = "color", showLabels=FALSE)
}
