# SETUP
# install.packages("vistime")

library(vistime)
library(tidyverse)


# Create chart with all tasks.
data <- read_csv("data/vis/tramitacao/data-camara.csv") 

# Custom tooltip
data$tooltip <- paste0("<b>", data$label, 
                       "</b> \n Início: ", data$start,
                       "\n Fim: ", data$end)

vistime(data, events="label", groups="group", title="Fases da tramitação", tooltips = "tooltip", colors = "color", showLabels=FALSE)


######################### TARGET ############################################
# 
# tramitacao_data <- data.frame(level = rep("LOCAL",4),
#                               start = c("2017-12-3", "2017-12-4", "2018-3-5", "2018-4-12"),
#                               end = c("2017-12-4", "2018-3-5", "2018-4-12", "2018-4-13"),
#                               event = c("SEC", "CCJ", "CAE", "PLEN"))
# vistime(tramitacao_data, events = "event", groups="local")
