#' @title Verfica se um elemento está contido em um vetor
#' @description Verfica se um elemento está contido em um vetor
#' @param element Elemento que pode estar contido
#' @param set Vetor de elementos
#' @return Valor booleano que indica se o elemento está contido no vetor.
#' @export
detect_fase <- function(element, set) {
  element %in% set
}
