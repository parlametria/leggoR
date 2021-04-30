calcula_hora <- function(time_init, current_time) {
  diff <- round(difftime(current_time, time_init, units = 'secs'), 3)
  if(diff > 60) {
    diff <- round(difftime(current_time, time_init, units = 'mins'), 3)
    texto <- str_glue(diff, ' minutos')
    return(texto)
  }
  texto <- str_glue(diff, ' segundos')
  return(texto)
}