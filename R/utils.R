#' @title Regex left match
#' @description Identifica eventos por regex e adiciona uma nova coluna
#' @param df dataframe a ser adicionado nova coluna
#' @param regex_df dataframe dos regex
#' @param new_column nova coluna
#' @return Dataframe com os eventos detectados pelo regex
#' @importFrom magrittr %<>%
#' @export
regex_left_match <- function(df, regex_df, new_column) {
  columns <-
    names(regex_df)[names(regex_df) != new_column] %>% sapply(function(x) {
      paste0(x, "X")
    }
    , USE.NAMES = TRUE)
  names(regex_df) %<>% sapply(function(column) {
    if (column == new_column)
      column
    else
      paste0(column, "X")
  }
  , USE.NAMES = TRUE)
  
  regex_df[is.na(regex_df)] <- ".*"
  
  df %>%
    dplyr::mutate(sort = row_number()) %>%
    fuzzyjoin::regex_left_join(regex_df, by = columns, ignore_case = TRUE) %>%
     group_by(sort) %>%
     ungroup() %>%
    dplyr::select(-tidyselect::ends_with("X"), -sort)

}


#' @title Renomeia as colunas do dataframe
#' @description Renomeia as colunas do dataframe usando o padrão de letras minúsculas e underscore
#' @param df Dataframe
#' @return Dataframe com as colunas renomeadas.
#' @importFrom magrittr %<>%
#' @export
rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}
