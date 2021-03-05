#' @title Lê o csv de votações se existir ou retorna um dataframe vazio
read_votacoes <- function(votacoes_datapath) {
  if (!is.null(votacoes_datapath) && file.exists(votacoes_datapath)) {
    votacoes_atuais <-
      read_csv(votacoes_datapath,
               col_types = cols(is_nominal = "l",
                                .default = "c"))
  } else {
    votacoes_atuais <- tibble(
      id_leggo = character(),
      id_votacao = character(),
      id_proposicao = character(),
      data = character(),
      obj_votacao = character(),
      casa = character(),
      is_nominal = logical()
    )
  }
  
  return(votacoes_atuais)
}

#' @title Lê o csv de votos se existir ou retorna um dataframe vazio
read_votos <- function(votos_datapath) {
  if (!is.null(votos_datapath) && file.exists(votos_datapath)) {
    votos_atuais <-
      read_csv(
        votos_datapath,
        col_types = cols(
          id_parlamentar = "i",
          id_parlamentar_parlametria =
            "i",
          voto = "i",
          .default = "c"
        )
      )
  } else {
    votos_atuais <- tibble(
      id_votacao = character(),
      id_parlamentar = integer(),
      id_parlamentar_parlametria = integer(),
      partido = character(),
      voto = integer(),
      casa = character()
    )
  }
  
  return(votos_atuais)
}