pl_id <- 46249
proc_tram_camara <- readr::read_csv(paste0("/local/tarciso/workspace/agora-digital/data/camara/",pl_id,"-fases-tramitacao-camara.csv"))

start_date <- dplyr::last(proc_tram_camara$data_hora) - lubridate::days(90)
hist_temperatura <- get_historico_temperatura_recente(proc_tram_camara, granularidade = 'm', tamanho_janela = 2, decaimento = 0.1) %>%
  dplyr::filter(periodo >= start_date)

library(ggplot2)
ggplot(hist_temperatura, aes(x=periodo, y=temperatura_recente)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

