#! /usr/bin/env Rscript

library(tidyverse)
library(rmarkdown)
library(here)
library(purrr)
library(magrittr)
source(here::here('scripts/build-data.R'))
source(here::here('scripts/renderReport.R'))
source(here::here('R/data-formatter-ficha.R'))

output_dir <- here::here('docs/reports')

# Create dirs when needed
c('data/senado', 'data/camara', 'data/vis/tramitacao', 'docs/reports') %>%
  lapply(dir.create, recursive=TRUE, showWarnings = FALSE)

# Store data
all_pls <- readr::read_csv('data/tabela_ids_camara.csv')
pls_senado_camara <- readr::read_csv('data/tabela_ids_senado_camara.csv')
all_pls %>% build_all_csvs(output_folder='data/', pls_senado_camara)
pls_senado_camara %>% build_all_csvs(output_folder='data/', pls_senado_camara)

# Build reports
all_pls %>% render_house_df_reports()
pls_senado_camara %>% render_reports_camara_senado()

# Build tabela e gabaritos
c(
  'reports/tabela-proposicoes.Rmd',
  'reports/tabela-demo.Rmd',
  'reports/gabarito/pls-559-2013-timeline.Rmd',
  'reports/gabarito/pls-559-2013-timeline-short.Rmd',
  'reports/gabarito/pl-3729-2004-timeline.Rmd',
  'reports/gabarito/pl-3729-2004-timeline-short.Rmd',
  'reports/gabarito/pl-490-2007-timeline.Rmd',
  'reports/gabarito/pl-490-2007-timeline-short.Rmd',
  'reports/gabarito/pls-229-2009-timeline.Rmd',
  'reports/gabarito/pls-229-2009-timeline-short.Rmd'
) %>% {
  lapply(here::here(.), render, 'html_document', output_dir = output_dir)
}
