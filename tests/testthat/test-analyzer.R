testthat::context('test-analyzer.R')

test_that('get_historico_energia_recente() has correct function passing the parameter day', {
  data <- data.frame(data_hora = c(rep(lubridate::ymd('2018-09-07'), 4),
                                   rep(lubridate::ymd('2018-09-10'), 2),
                                   rep(lubridate::ymd('2018-09-11'), 1),
                                   rep(lubridate::ymd('2018-09-12'), 5),
                                   rep(lubridate::ymd('2018-09-13'), 3),
                                   rep(lubridate::ymd('2018-09-14'), 3)))

  data <- data %>% dplyr::mutate(evento = "x") 
  
  decaimento = 0.1
  r <- 1 - decaimento
  result <- c(
    # 2018-09-7
    sum(4 * r^0),
    # 2018-09-10
    sum(2 * r^0, 4 * r^1),
    # 2018-09-11
    sum(1 * r^0, 2 * r^1, 4 * r^2),
    # 2018-09-12
    sum(5 * r^0, 1 * r^1, 2 * r^2, 4 * r^3),
    # 2018-09-13
    sum(3 * r^0, 5 * r^1, 1 * r^2, 2 * r^3, 4 * r^4),
    # 2018-09-14
    sum(3 * r^0, 3 * r^1, 5 * r^2, 1 * r^3, 2 * r^4, 4 * r^5)
  )
  
  result <- round(result, digits = 3)
  
  energy_df <- get_historico_energia_recente(data, granularidade = 'd', decaimento = decaimento)
  
  expect_true(all(energy_df$energia_recente == result))
})

test_that('get_historico_energia_recente() has correct function passing the parameter week', {
  data <- data.frame(data_hora = seq(lubridate::ymd("2018-09-07"), lubridate::ymd("2018-09-28"), by = "1 day"))
  
  data <- data %>% dplyr::mutate(evento = "x") 
  
  decaimento = 0.1
  r <- 1 - decaimento
  result <- c(
    # semana 1
    sum(1 * r^0),
    # semana 2
    sum(5 * r^0, 1 * r^1),
    # 2018-09-11
    sum(5 * r^0, 5 * r^1, 1 * r^2),
    # 2018-09-12
    sum(5 * r^0, 5 * r^1, 5 * r^2, 1 * r^3)
  )
  
  result <- round(result, digits = 3)
  
  energy_df <- get_historico_energia_recente(data, granularidade = 's', decaimento = decaimento)
  
  expect_true(all(energy_df$energia_recente == result))
})
