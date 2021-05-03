library(tidyverse)
library(agoradigital)
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
    agoradigital::process_lista_apensadas_por_casa(
      proposicoes_apensadas,
      "camara",
      fresh_execution,
      export_filepath = str_glue(export_folder, "lista_apensadas_camara.csv")
    )

  flog.info("Processando árvore de apensadas para Senado")
  lista_proposicoes_apensadas_senado <-
    agoradigital::process_lista_apensadas_por_casa(
      proposicoes_apensadas,
      "senado",
      fresh_execution,
      export_filepath = str_glue(export_folder, "lista_apensadas_senado.csv")
    )

  flog.info("Processamento de árvore de apensadas concluído!")
  return(list(lista_proposicoes_apensadas_camara, lista_proposicoes_apensadas_senado))
}
