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

#' @title Cria log de mensagem das apensadas
#' @description Cria mensagem das apensadas que será enviado do bot
#' @param props_apensadas_nao_monitoradas_filepath caminho para csv das props apensadas
#' @return Mensagem de log
#' @examples
#' create_log('props_apensadas_nao_monitoradas.csv')
create_log <- function(props_apensadas_nao_monitoradas_filepath){
  
  props <- read.csv(props_apensadas_nao_monitoradas_filepath)
  
  df <- props %>% 
    mutate(original = id_ext_prop_principal_raiz) %>% 
    mutate(apensada=id_ext_prop_principal) %>% 
    mutate(log = paste0("casa: ", casa, 
                        " original: ", original, 
                        " apensada: ", apensada, 
                        " interesse: ", interesse)) %>% 
    select(log)
  
  string_log <- paste0(df$log, collapse = "\n ")
  
  return (string_log)
  
}
