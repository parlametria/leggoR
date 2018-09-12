library(ggplot2)

pl_id <- 2121442
proc_tram_camara <- readr::read_csv(paste0("/local/tarciso/workspace/agora-digital/data/camara/",pl_id,"-fases-tramitacao-camara.csv"))

start_date = dplyr::last(proc_tram_camara$data_hora) - lubridate::days(90)
hist_energia <- get_historico_energia_recente(proc_tram_camara) %>%
  dplyr::filter(data_hora >= start_date) %>%
  dplyr::mutate(semana = lubridate::week(data_hora)) %>%
  dplyr::group_by(semana) %>%
  dplyr::summarize(energia_recente = median(energia_total))

ggplot(hist_energia, aes(x=semana, y=energia_recente)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 10), se = FALSE) +
  geom_ribbon(aes(ymin = 0,ymax = predict(lm(formula = energia_recente ~ splines::bs(semana,10)))),
              alpha = 0.3,fill = 'red') +
  theme_minimal()

  # ggplot(hist_energia, aes(x=semana, y=energia_recente)) +
#   geom_point() +
#   geom_line()