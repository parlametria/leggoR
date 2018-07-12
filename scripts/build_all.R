#! /usr/bin/env Rscript

library(tidyverse)
library(rmarkdown)
library(here)
source(here::here('scripts/vis/tramitacao/build-data.R'))
source(here::here('scripts/renderReport.R'))

# Create dirs when needed
c('data/Senado', 'data/camara', 'data/vis/tramitacao', 'docs/reports') %>%
  lapply(dir.create, recursive=TRUE, showWarnings = FALSE)

# Store data
readr::read_csv('data/tabela_geral_ids_casa.csv') %>% build_all_csvs()

# Build reports
render_all_reports()

# Build reports table
render(
  here::here('reports/tabela-proposicoes.Rmd'),
  'html_notebook',
  output_dir = output_dir,
)
