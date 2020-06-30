.mapeia_nome_autor_relator_para_id <-
  function(proposicoes_df, parlamentares_df) {
    library(tidyverse)
    
    parlamentares_df <- parlamentares_df %>%
      mutate(nome_parlamentar = .padroniza_nome(nome_eleitoral)) %>%
      select(nome_parlamentar, id_parlamentar, casa)
    
    proposicoes_df <- proposicoes_df %>%
      mutate(nome_relator_padronizado = .padroniza_nome(relator_nome)) %>%
      left_join(parlamentares_df,
                by = c("nome_relator_padronizado" = "nome_parlamentar", "casa")) %>%
      rename(id_relator = id_parlamentar) %>%
      mutate(enum_casa = if_else(casa == "camara", 1, 2)) %>%
      mutate(id_relator_parlametria = if_else(
        !is.na(id_relator),
        paste0(enum_casa, id_relator),
        id_relator
      ))
    
    
    proposicoes_df <- proposicoes_df %>%
      mutate(nome_autor_padronizado = .padroniza_nome(autor_nome)) %>%
      left_join(parlamentares_df,
                by = c("nome_autor_padronizado" = "nome_parlamentar", "casa")) %>%
      rename(id_autor = id_parlamentar) %>%
      mutate(id_autor_parlametria = if_else(!is.na(id_autor),
                                            paste0(enum_casa, id_autor),
                                            id_autor))
    
    proposicoes_df <- proposicoes_df %>%
      select(
        id_ext,
        sigla_tipo,
        numero,
        ementa,
        data_apresentacao,
        casa,
        casa_origem,
        id_autor,
        id_autor_parlametria,
        autor_nome,
        autor_uf,
        autor_partido,
        regime_tramitacao,
        forma_apreciacao,
        id_relator,
        id_relator_parlametria,
        relator_nome,
        id_leggo
      )
    
    return(proposicoes_df)
  }

.padroniza_nome <- function(nome) {
  nome_processado <- str_to_title(nome) %>%
    str_remove("Dep. |Sen. ") %>%
    str_remove(" \\(.*")
  
  nome_processado <- if_else(
    str_detect(nome_processado, "/"),
    str_remove(nome_processado, " /.*") %>%
      str_remove(" [^ ]+$"),
    nome_processado
  )
  
  return(nome_processado)
}

.bind_parlamentares <- function(export_path) {
  library(tidyverse)
  if (!str_detect(export_path, "\\/$")) {
    export_path <- paste0(export_path, "/") 
  }
  
  deputados <-
    read_csv(paste0(export_path, "camara/parlamentares.csv"),
             col_types = cols(.default = "c")) %>%
    mutate(casa = "camara") %>%
    select(
      casa,
      id_parlamentar = id,
      nome_completo = nome_civil,
      nome_eleitoral = ultimo_status_nome_eleitoral,
      genero = sexo,
      partido = ultimo_status_sigla_partido,
      uf = ultimo_status_sigla_uf
    )
  
  senadores <-
    read_csv(paste0(export_path, "senado/parlamentares.csv"),
             col_types = cols(.default = "c"))
  
  parlamentares <- bind_rows(deputados, senadores)
  
  return(parlamentares)
}
