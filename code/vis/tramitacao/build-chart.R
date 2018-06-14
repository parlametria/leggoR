# SETUP
# install.packages("vistime")
args = commandArgs(trailingOnly=TRUE)
library(vistime)
library(tidyverse)
library(lubridate)


# Received the project id and the home and returns a graph with the timeline of the process.
create_chart <- function(bill_id = 2121442, house = 'camara'){
    
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

if(length(args) == 2){
  create_chart(args[1], args[2])
} 
