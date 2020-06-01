library(tidyverse)

#' @title Processa a casa do Congresso
#' @description Recebe uma string de uma casa (câmara dos deputados, 
#' senado federal ou congresso nacional), retira pontuações, 
#' letras maiúsculas e deixa só o primeiro nome.
#' @param casa Casa do congresso a ser processada
#' @return Palavra padronizada
#' @examples
#' .process_casa("câmara Dos deputados")
.process_casa <- function(casa) {
  source(here::here("R/utils.R"))
  proc_casa <- str_split(padroniza_string(casa), " ")[[1]][1]
  return(proc_casa)
}

#' @title Separa o nome formal em sigla, número e ano.
#' @description Recebe o nome formal de uma proposição e retorna
#' uma lista com sigla, numero e ano.
#' @param nome_formal Nome a ser processado. 
#' Formato: (SF/CD/CN) <sigla> <numero>/<ano>
#' @return Lista com sigla, numero e ano
#' @examples
#' .process_inputs("CD PEC 6/2019")
.process_inputs <- function(nome_formal) {
  
  
  lista_att <- str_split(nome_formal, " ")[[1]]
  
  sigla <- if_else(length(lista_att) > 2,
                   lista_att[2],
                   sigla <- lista_att[1])
  
  lista_att <- str_split(lista_att[length(lista_att)], "/")[[1]]
  print(lista_att)
  
  if (length(lista_att) == 2) {
    numero = lista_att[1]
    ano = lista_att[2]
    
  }
  
  return(list(
    sigla = sigla,
    numero = numero,
    ano = ano
  ))
}

#' @title Mapeia o nome formal e casa para o id correspondente
#' @description Recebe o nome formal de uma proposição e sua casa e
#' retorna o id cadastrado na api correspondente. No caso da casa ser
#' "Congresso Nacional", ele buscará o id em ambas as casas senado e 
#' câmara.
#' @param nome_formal Nome formal da proposição
#' Formato: (SF/CD/CN) <sigla> <numero>/<ano>
#' @param casa Casa da proposição
#' @return id da proposiçao.
#' @examples
#' fetch_id_by_nome_formal("CD PEC 6/2019", "Câmara dos Deputados")
fetch_id_by_nome_formal <- function(nome_formal, casa) {
  print(paste0("Baixando id a partir de nome formal ", nome_formal, " na casa ", casa, "\n..."))
  library(rcongresso)
  
  constants <-
    jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))$constants
  
  params <- .process_inputs(nome_formal)
  
  casa <- .process_casa(casa)
  id <- NA
  
  if (constants$camara_label == casa) {
    prop <- fetch_proposicao_camara(
      siglaTipo = params$sigla,
      numero = params$numero,
      ano = params$ano
    )
    
    if (nrow(prop) > 0) {
      id <- prop %>% 
        pull(id)
    }
    
  } else if (constants$senado_label == casa) {
    prop <-
      fetch_proposicao_senado_sigla(params$sigla, params$numero, params$ano)
    
    if (nrow(prop) > 0) {
      id <- prop %>% 
        pull(codigo_materia)
    }
    
  } else if (constants$congresso_label == casa) {
    prop <-
      fetch_proposicao_senado_sigla(params$sigla, params$numero, params$ano)
    
    if (nrow(prop) == 0) {
      prop <-
        fetch_proposicao_camara(
          siglaTipo = params$sigla,
          numero = params$numero,
          ano = params$ano
        ) 
      if (nrow(prop) > 0) {
        id <- prop %>% 
          pull(id)
      }
    } else {
      id <- prop %>% 
        pull(codigo_materia)
    }
  }
  return(id)
  
}

#' @title Extrai o id de uma url de proposição
#' @description Recebe um link para a proposição e 
#' extrai o id dessa proposição.
#' @param url_proposicao Url da proposição
#' @return id da proposiçao.
#' @examples
#' extract_id_from_link("http://www25.senado.leg.br/web/atividade/materias/-/materia/142053")
extract_id_from_link <- function(url_proposicao) {
  print(paste0("Extraindo id da url ", url_proposicao, "\n..."))
  constants <-
    jsonlite::fromJSON(here::here("R/config/environment_congresso.json"))$constants
  
  sep <- if_else(str_detect(url_proposicao, constants$camara_label),
                 "=",
                 "/")
  
  sep_url <- str_split(url_proposicao, sep)[[1]]
  
  id <- sep_url[length(sep_url)]
  
  return(id)
  
}