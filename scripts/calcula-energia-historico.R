library(ggplot2)

pl_id <- 46249
proc_tram_camara <- readr::read_csv(paste0("/local/tarciso/workspace/agora-digital/data/camara/",pl_id,"-fases-tramitacao-camara.csv"))

start_date <- dplyr::last(proc_tram_camara$data_hora) - lubridate::days(90)
hist_energia <- get_historico_energia_recente(proc_tram_camara, granularidade = 's') %>%
  dplyr::filter(periodo >= start_date)

ggplot(hist_energia, aes(x=periodo, y=energia_recente)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()


