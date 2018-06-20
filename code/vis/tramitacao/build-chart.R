# SETUP
# install.packages("vistime")
library(vistime)
library(tidyverse)
library(lubridate)


#' @title Cria gráfico para demonstração da tramitação de uma proposição.
#' @description Recebido um id e uma casa a função pesquisa o csv de visualização correspondente
#' em data/vis/tramitacao e cria a visualização em timeline da tramitação da proposição.
#' @param bill_id Identificador da proposição.
#' @param house Casa a que pertence essa proposição.
#' @examples
#' create_chart(91341, "senado")
#' @export
create_chart <- function(bill_id, house){
    
  data <- read_csv(paste0(here::here("data/vis/tramitacao/"), bill_id,"-data-", tolower(house), ".csv")) 
  
  # Custom tooltip
  data$tooltip <- ifelse(data$end == ymd(Sys.Date()), 
                         paste0("<b>", data$label, 
                                "</b> \n Início: ", data$start,
                                "<b>\n Em andamento </b>"),
                         paste0("<b>", data$label, 
                                "</b> \n Início: ", data$start,
                                "\n Fim: ", data$end))
  
  vistime(data, events="label", groups="group", title=paste0("Tramitação em ", house), tooltips = "tooltip", colors = "color", showLabels=FALSE)
}
