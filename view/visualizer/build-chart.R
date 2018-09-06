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
  data <- read_csv(paste0(here::here("data/vis/tramitacao/"), bill_id, "-data-", tolower(house), ".csv"))

  data$tooltip <- format_tooltip(data)
  
  create_vistime(paste0("Tramitação em ", house), data)
}

#' @title Cria gráfico para demonstração da tramitação de uma proposição na camara e no senado.
#' @description Recebido um id do senado e um id da camara a função pesquisa os csv de visualização correspondentes
#' em data/vis/tramitacao e cria a visualização em timeline da tramitação da proposição.
#' @param bill_id_senado Identificador da proposição no senado.
#' @param bill_id_camara Identificador da proposição na camara.
#' @examples
#' create_chart(91341, 2088990)
#' @export
create_chart_camara_senado <- function(bill_id_senado, bill_id_camara){
  data_senado <- 
    read_csv(paste0(here::here("data/vis/tramitacao/"), bill_id_senado, "-data-senado.csv")) %>%
    mutate(start = as.POSIXct(start),
           end = as.POSIXct(end))
  data_camara <-
    read_csv(paste0(here::here("data/vis/tramitacao/"), bill_id_camara, "-data-camara.csv"))
  data <- bind_rows(data_senado, data_camara)
  
  data$tooltip <- format_tooltip(data)
  
  create_vistime("Tramitação", data)
}

#' @title Chama a função vistime com os parametros corretos
#' @description Recebe o título do gráfico e o dataframe formatado e chama o vistime
#' @param title Título do gráfico
#' @param data Dataframe com dados formatados para visualização do vistime
#' @examples
#' create_vistime("Tramitação", data)
#' @export
create_vistime <- function(title, data) {
  vistime(data, events="label", groups="group", title=title, tooltips = "tooltip", colors = "color", showLabels=FALSE)
}

#' @title Formata o tooltip do vistime
#' @description Deixa o tooltip mais significativo
#' @param data Dataframe com dados formatados para visualização do vistime
#' @examples
#' data$tooltip <- format_tooltip(data)
#' @export
format_tooltip <- function(data) {
  # Custom tooltip
 ifelse(data$start == ymd(Sys.Date()),
                         paste0("<b>", data$label,
                                "</b> \n Futuro"),
                         paste0("<b>", data$label,
                                "</b> \n Início: ", data$start,
                                "\n Fim: ", data$end))
}
