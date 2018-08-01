library(rmarkdown)
library(here)

output_dir <- here::here('docs/reports')

render_plot_single_house <- function(param_id, param_casa) {
  render(
    here::here('scripts/gera-relatorio-projeto-por-casa.Rmd'),
    'html_document',
    output_dir = output_dir,
    output_file = paste0('report_', param_id, '_', param_casa, '.html'),
    params = list(id = param_id, casa = param_casa)
  ) %>% as.tibble()
  # TODO: tirar esse tibble de cima e fazer o render_house_df_reports funcionar.
}

render_plot_all <- function(senado_id, camara_id) {
  render(
    here::here('scripts/gera-relatorio-projeto-todas-as-casas.Rmd'),
    'html_document',
    output_dir = output_dir,
    output_file = paste0('report_', senado_id, '_senado_', camara_id, '_camara', '.html'),
    params = list(senado_id = senado_id, camara_id = camara_id)
  )
}

render_house_df_reports <- function(df) {
  df %>%
    rowwise() %>%
    do(render_plot_single_house(param_id = .$id, param_casa = .$casa))
}

render_all_reports <- function() {
  pls_168_senado <- 132865
  pl_3729_camara <-257161
  pl_490_camara <- 345311
  pl_3751_camara <- 2056568
  pl_6670_camara <- 2120775

  lei_teto_remuneratorio_senado <- 127753
  lei_teto_remuneratorio_camara <- 2121442

  render_plot_single_house(pls_168_senado, 'senado')
  render_plot_single_house(pl_3729_camara, 'camara')
  render_plot_single_house(pl_490_camara, 'camara')
  render_plot_single_house(pl_3751_camara, 'camara')
  render_plot_single_house(pl_6670_camara, 'camara')
  render_plot_all(lei_teto_remuneratorio_senado, lei_teto_remuneratorio_camara)
}
