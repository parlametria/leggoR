regex_left_match <- function(df, regex_df, new_column) {
  # names(regex_df)[names(regex_df) != new_column] %>% sapply(function(x) {
  #   df[x] %<>% 
  #     sapply(function(row) {
  #       str_replace_all(row, '\r|\n', '')
  #     })
  # })

  columns <-
    names(regex_df)[names(regex_df) != new_column] %>% sapply(function(x) {
      paste0(x, "X")
    }, USE.NAMES = TRUE)
  names(regex_df) %<>% sapply(function(column) {
    if (column == new_column)
      column
    else
      paste0(column, "X")
  }, USE.NAMES = TRUE)
  
  regex_df[is.na(regex_df)] <- ".*"
  
  df %>%
    mutate(sort = row_number()) %>%
    fuzzyjoin::regex_left_join(regex_df, by = columns, ignore_case = TRUE) %>%
    group_by(sort) %>%
    filter(rank(sort, ties.method = "first") == 1) %>%
    ungroup() %>%
    select(-ends_with("X"), -sort)
}