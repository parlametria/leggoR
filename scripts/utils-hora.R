#' @title Calcula hora
#' @description Calcula oo tempo decorrido entre dois instantes de tempo
#' @param time_init Instante de tempo inicial
#' @param current_time Instante de tempo final
#' @return Tempo decorrido em segundos/minutos
calcula_hora <- function(time_init, current_time) {
  diff <- round(difftime(current_time, time_init, units = 'secs'), 3)
  if(diff > 60) {
    diff <- round(difftime(current_time, time_init, units = 'mins'), 3)
    texto <- stringr::str_glue(diff, ' minutos')
    return(texto)
  }
  texto <- stringr::str_glue(diff, ' segundos')
  return(texto)
}