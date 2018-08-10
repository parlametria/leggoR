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
    mutate(sort = row_number()) %>%
    fuzzyjoin::regex_left_join(regex_df, by = columns, ignore_case = TRUE) %>%
    group_by(sort) %>%
    filter(rank(sort, ties.method = "first") == 1) %>%
    ungroup() %>%
    select(-ends_with("X"), -sort)
}


#' @title Renomeia as colunas do dataframe
#' @description Renomeia as colunas do dataframe usando o padrão de letras minúsculas e underscore
#' @param df Dataframe
#' @return Dataframe com as colunas renomeadas.
#' @export
rename_df_columns <- function(df) {
  names(df) %<>% to_underscore
  df
}
