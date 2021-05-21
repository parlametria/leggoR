#' @title Envia log para o bot
#' @description Envia mensagem para o bot do Voz Ativa
#' @param message_log mensagem que será enviada ao bot
#' @return Mensagem indicando que a mensagem foi enviada com sucesso
#' @examples
#' send_log_to_bot('processamento completo')
send_log_to_bot <- function(message_log) {
  library(httr)
  
  log <- list(
    message = message_log
  )

  endpoint <- Sys.getenv("APP_SECRET")

  res <- POST(paste0("https://voz-ativa-bot.herokuapp.com/", endpoint), body = log, encode = "json", verbose())
  
  return("Mensagem Enviada!")
}