#! /usr/bin/env Rscript

library(tidyverse)
library(rmarkdown)
library(here)
library(purrr)
library(magrittr)
source(here::here("scripts/build-data.R"))
source(here::here("scripts/renderReport.R"))

output_dir <- here::here("docs/reports")

# Create dirs when needed
c("data/senado", "data/camara", "data/vis/tramitacao", "docs/reports") %>%
  lapply(dir.create, recursive = TRUE, showWarnings = FALSE)

# Store data
all_pls <- readr::read_csv("data/tabela_geral_ids_casa.csv",
                           col_types = cols(id = readr::col_integer(),
                                            casa = readr::col_character(),
                                            apelido = readr::col_character(),
                                            tema = readr::col_character()))
pls_senado_camara <- readr::read_csv("data/tabela_ids_senado_camara.csv",
                                     col_types = cols(id_camara = readr::col_integer(),
                                                      id_senado = readr::col_integer()))
all_pls %>% build_all_csvs(output_folder = "data/", pls_senado_camara)
pls_senado_camara %>% build_all_csvs(output_folder = "data/", pls_senado_camara)

# Build reports
all_pls %>% render_house_df_reports()
pls_senado_camara %>% render_reports_camara_senado()

# Build tabela e gabaritos
c(
  "reports/tabela-proposicoes.Rmd",
  "reports/tabela-demo.Rmd",
  "reports/gabarito/pls-559-2013-timeline.Rmd",
  "reports/gabarito/pls-559-2013-timeline-short.Rmd",
  "reports/gabarito/pl-3729-2004-timeline.Rmd",
  "reports/gabarito/pl-3729-2004-timeline-short.Rmd",
  "reports/gabarito/pl-490-2007-timeline.Rmd",
  "reports/gabarito/pl-490-2007-timeline-short.Rmd",
  "reports/gabarito/pls-229-2009-timeline.Rmd",
  "reports/gabarito/pls-229-2009-timeline-short.Rmd"
) %>% {
  lapply(here::here(.), render, "html_document", output_dir = output_dir)
}
