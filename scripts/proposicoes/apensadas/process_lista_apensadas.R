library(tidyverse)
library(here)
library(futile.logger)

#' @title Processa lista de apensadas com árvore partindo das apensadas monitoradas até chegar na proposição raiz.
#' @description Recupera uma lista que contém as ligações entre as proposições apensadas monitoradas e suas
#' proposições principais para as duas casas: câmara e senado.
#' @param proposicoes_apensadas Dataframe com lista de proposições apensadas e suas proposições principais.
#' É necessário pelo menos 3 colunas: id_ext, casa, id_prop_principal.
#' @param fresh_execution Se FALSE então um arquivo com a lista de apensadas executada anteriormente será utilizada.
#' TRUE caso uma execução nova seja o objetivo.
#' @param export_folder Caminho do diretório para salvar lista de apensadas. Com / no final.
#' @return Lista com árvore de apensados.
process_lista_apensadas <- function(proposicoes_apensadas, fresh_execution = FALSE, export_folder) {
  flog.info("Processando árvore de apensadas para Câmara")
  lista_proposicoes_apensadas_camara <-
    process_lista_apensadas_por_casa(
      proposicoes_apensadas,
      "camara",
      fresh_execution,
      export_filepath = str_glue(export_folder, "lista_apensadas_camara.csv")
    )

  flog.info("Processando árvore de apensadas para Senado")
  lista_proposicoes_apensadas_senado <-
    process_lista_apensadas_por_casa(
      proposicoes_apensadas,
      "senado",
      fresh_execution,
      export_filepath = str_glue(export_folder, "lista_apensadas_senado.csv")
    )

  flog.info("Processamento de árvore de apensadas concluído!")
  return(list(lista_proposicoes_apensadas_camara, lista_proposicoes_apensadas_senado))
}

#' @title Processa lista de apensadas com árvore partindo das apensadas monitoradas até chegar na proposição raiz.
#' @description Recupera uma lista que contém as ligações entre as proposições apensadas monitoradas e suas
#' proposições principais para uma casa de origem.
#' @param proposicoes_apensadas Dataframe com lista de proposições apensadas e suas proposições principais.
#' É necessário pelo menos 3 colunas: id_ext, casa, id_prop_principal.
#' @param casa_origem Casa de origem das proposições. Parâmetro usado para recuperar dados da uriPrincipal.
#' @param fresh_execution Se FALSE então um arquivo com a lista de apensadas executada anteriormente será utilizada.
#' TRUE caso uma execução nova seja o objetivo.
#' @param export_filepath Caminho para salvar lista de apensadas
#' @param save_result TRUE se o resultado da lista deve ser salvo em csv no caminho do export_filepath
#' @return Lista com árvore de apensados.
process_lista_apensadas_por_casa <- function(proposicoes_apensadas,
                                             casa_origem = "camara",
                                             fresh_execution = FALSE,
                                             export_filepath = "lista_apensadas.csv",
                                             save_result = TRUE) {

  if (!fresh_execution) {
    if (file.exists(export_filepath)) {
      execucao_anterior_lista <- read_csv(export_filepath, col_types = cols(.default = "c")) %>%
        select(id_ext = key, id_prop_principal = value)
    } else {
      execucao_anterior_lista <- tibble()
    }
  } else {
    execucao_anterior_lista <- tibble()
  }

  props_apensadas <- proposicoes_apensadas %>%
    filter(casa == casa_origem)

  # Lista inicial com as proposições apensadas e suas proposições principais
  lista_data <- bind_rows(props_apensadas, execucao_anterior_lista) %>%
    distinct(id_ext, id_prop_principal) %>%
    mutate(id_prop_principal = ifelse(is.na(id_prop_principal), "raiz", id_prop_principal))

  lista <<- setNames(as.list(lista_data$id_prop_principal), lista_data$id_ext)

  # Recuperando lista com árvore de apensadas
  execucao_lista <- lista %>%
    lmap(~fetch_proposicao_raiz(.x, casa = casa_origem))

  # Removendo variáveis não utilizadas
  rm(lista_data, execucao_lista, execucao_anterior_lista)

  # Escrevendo dados da lista de apensadas
  lista_asdataframe <- stack(lista) %>%
    select(key = ind, value = values)

  if (save_result) {
    write_csv(lista_asdataframe, export_filepath)
  }

  return(lista)
}

#' @title Atualiza lista com proposições principais a partir de uma proposição apensada como parâmetro.
#' @description Checa se uma proposição está apensada a outra e caminha pela árvore de apensadas
#' até chegar na proposição raiz. Salva na lista (objeto no environment global) as proposições apensadas (chave) e
#' as prosições principais (valor). Este código não considera outras subárvores da árvore de apensadas da proposição
#' passada como parâmetro.
#' Na lista usada como estrutura de entrada: key é a proposição possivelmente apensada e value é a proposição principal
#' Para o caso de proposições já monitoradas que não são apensadas o value é 'raiz'.
#' @param x Lista de um elemento (ou string) com o id da proposição para pesquisar
#' @param casa Casa de origem da proposição
#' @param verbose TRUE para exibir mensagens sobre a execução da proposição atual, FALSE caso contrário.
#' @return Elemento x passado como parâmetro
fetch_proposicao_raiz <- function(x, casa, verbose = TRUE) {
  if (verbose) {
    print(str_glue("key: {names(x)} value: {x[[1]]} casa: {casa}"))
  }

  id <- x[[1]]
  if (is.null(lista[[id]]) && id != "raiz") {
    uri_prop_principal <- agoradigital::fetch_proposicao(id, casa) %>% pull(uri_prop_principal)
    if (is.na(uri_prop_principal)) {
      return(x)
    } else {
      id_prop_principal <- .extract_id_prop(uri_prop_principal)
      lista[[id]] <<- id_prop_principal
      fetch_proposicao_raiz(id_prop_principal, casa)
    }
  }
  return(x)
}

#' @title Extrai id da URI da proposição na API da respectiva casa
#' @param uri URI da proposição
#' @return Id da proposição extraído a partir da URI
.extract_id_prop <- function(uri) {
  id_prop <- case_when(
        str_detect(uri, "dadosabertos.camara.leg.br") ~
          uri %>% str_extract("proposicoes/[0-9]+") %>% str_extract("[0-9]+"),
        str_detect(uri, "legis.senado.leg.br") ~
          uri %>% str_extract("materia/[0-9]+") %>% str_extract("[0-9]+")
      )

  return(id_prop)
}
