send_log_to_bot <- function(message_log) {
  library(httr)
  
  log <- list(
    message = message_log
  )
  endpoint <- Sys.getenv("APP_SECRET")
  
  res <- POST(paste0("https://voz-ativa-bot.herokuapp.com/", endpoint), body = log, encode = "json", verbose())
  
  return("Mensagem Enviada!")
}

get_log_error <- function(error, message_header) {
  log <- paste0(message_header, "\n", "Aqui está a mensagem de erro:\n", 
                error, "\n")
  return(log)
}