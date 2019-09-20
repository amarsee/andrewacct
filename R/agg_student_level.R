#' Aggregate Student Level to School/District/State
#'
#' This function takes a student level dataframe and aggregates it to the school/district/state level.
#' Provides sums and means for desired columns.
#' @param df Dataframe. Used first to be friendly to the \%>\% operator.
#' @param sum_cols A character vector of the columns to be summed.
#' @param mean_cols A character vector of the columns to find the mean. Default is no columns.
#' @param ... The columns to aggregate at (e.g. system, system_name, school, school_name)
#' @keywords aggregate, group, collapse
#' @export
#' @examples
#' agg_student_level(student_level_df, c('enrolled', 'tested', 'valid tests'), c(), system, system_name)

agg_student_level <- function(df, sum_cols, mean_cols = c(), ...){
  if (length(sum_cols) > 0){
  sum_df <- df %>%
    group_by(...) %>%
    summarise_at(
      .vars = sum_cols,
      .funs = ~sum(., na.rm = TRUE)
    ) %>%
    ungroup()
  } else {
    sum_df <- data.frame()
  }
  if(length(mean_cols) > 0){
  mean_df <- df %>%
    group_by(...) %>%
    summarise_at(
      .vars = mean_cols,
      .funs = ~round(mean(., na.rm = TRUE) + 1e-10, 1)
    ) %>%
    ungroup()
  } else {
    mean_df <- data.frame()
  }
  if (length(sum_df) > 0 & length(mean_df) > 0){
  return(sum_df %>%
           left_join(mean_df, by = ))
  } else if (length(mean_df) > 0) {
    return(mean_df)
  } else {
    return(sum_df)
  }
}
