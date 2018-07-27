#! /usr/bin/env Rscript

library(tidyverse)
library(rmarkdown)
library(here)
source(here::here('scripts/vis/tramitacao/build-data.R'))
source(here::here('scripts/renderReport.R'))
source(here::here('scripts/vis/tramitacao/extract-informations.R'))

output_dir <- here::here('docs/reports')

# Create dirs when needed
c('data/Senado', 'data/camara', 'data/vis/tramitacao', 'docs/reports') %>%
  lapply(dir.create, recursive=TRUE, showWarnings = FALSE)

# Store data
all_pls <- readr::read_csv('data/tabela_geral_ids_casa.csv')
all_pls %>% build_all_csvs()

# Build reports
all_pls %>% render_house_df_reports()

# Build tabela e gabaritos
c(
  'reports/tabela-proposicoes.Rmd',
  'reports/tabela-demo.Rmd',
  'reports/gabarito/pls-559-2013-timeline.Rmd',
  'reports/gabarito/pl-3729-2004-timeline.Rmd',
  'reports/gabarito/pl-490-2007-timeline.Rmd',
  'reports/gabarito/pls-229-2009-timeline.Rmd'
) %>% {
  lapply(here::here(.), render, 'html_document', output_dir = output_dir)
}
