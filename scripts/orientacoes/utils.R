read_orientacoes <- function(orientacoes_datapath) {
  if(!is.null(orientacoes_datapath) && file.exists(orientacoes_datapath)) {
    orientacoes_atuais <-
      read_csv(orientacoes_datapath, col_types = cols(orientacao = "i",
                                                      .default = "c"))      
    
  } else {
    orientacoes_atuais <- tibble(
      id_votacao = character(),
      orientacao = integer(),
      tipo_lideranca = character(),
      partido_bloco = character(),
      casa = character()
    )
  }
  
  return(orientacoes_atuais)
}