# SETUP
# install.packages("vistime")

library(vistime)
library(tidyverse)


# Create chart with all tasks.
bill_id <- 91341
read_csv(paste0("data/vis/tramitacao/",bill_id,"-data-senado.csv")) %>%
vistime(events="label", groups="group", title="Fases da tramitação", showLabels=FALSE)


######################### TARGET ############################################
# 
# tramitacao_data <- data.frame(level = rep("LOCAL",4),
#                               start = c("2017-12-3", "2017-12-4", "2018-3-5", "2018-4-12"),
#                               end = c("2017-12-4", "2018-3-5", "2018-4-12", "2018-4-13"),
#                               event = c("SEC", "CCJ", "CAE", "PLEN"))
# vistime(tramitacao_data, events = "event", groups="local")
