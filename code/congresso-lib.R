#' @title Renomeia um vetor com o padrão de underscores e minúsculas
#' @description Renomeia cada item do vetor com o padrão: separado por underscore e letras minúsculas
#' @param x Vetor de strings
#' @return Vetor contendo as strings renomeadas.
#' @examples
#' to_underscore(c("testName", "TESTNAME"))
#' @export
to_underscore <- function(x) {
  x2 <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x3 <- gsub(".", "_", x2, fixed = TRUE)
  x4 <- gsub("([a-z])([A-Z])", "\\1_\\2", x3)
  x5 <- tolower(x4)
  x5
}

#' @title Verfica se um elemento está contido em um vetor
#' @description Verfica se um elemento está contido em um vetor
#' @param element Elemento que pode estar contido
#' @param set Vetor de elementos
#' @return Valor booleano que indica se o elemento está contido no vetor.
#' @export
detect_fase <- function(element, set) {
  element %in% set
}