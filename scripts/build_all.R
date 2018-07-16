#! /usr/bin/env Rscript

library(tidyverse)
library(rmarkdown)
library(here)
source(here::here('scripts/vis/tramitacao/build-data.R'))
source(here::here('scripts/renderReport.R'))

output_dir = here::here('docs/reports')

# Create dirs when needed
c('data/Senado', 'data/camara', 'data/vis/tramitacao', 'docs/reports') %>%
  lapply(dir.create, recursive=TRUE, showWarnings = FALSE)

# Store data
readr::read_csv('data/tabela_geral_ids_casa.csv') %>% build_all_csvs()

# Build reports
render_all_reports()

# Build tabela e gabaritos
c(
  'reports/tabela-proposicoes.Rmd',
  'reports/gabarito/pl-3729-2004-timeline.Rmd',
  'reports/gabarito/pl-490-2007-timeline.Rmd'
) %>%
  {lapply(here::here(.), render, 'html_document', output_dir = output_dir)}
