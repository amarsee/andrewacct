#' Aggregate Student Level to School/District/State
#'
#' This function takes a student level dataframe and aggregates it to the school/district/state level.
#' Provides sums and means for desired columns.
#' @param df Dataframe. Used first to be friendly to the \%>\% operator.
#' @param op_list A named list where the name is the operation and the value is a character vector
#' of column names to apply the operation to
#' @param ... The columns to aggregate at (e.g. system, system_name, school, school_name)
#' @keywords aggregate, group, collapse
#' @examples
#' agg_student_level(student_level_df,op_list = list(sum = c('enrolled', 'tested', 'valid tests')),
#' system, school, subgroup)
#' @export


agg_student_level <- function(df, op_list = list(), ...){
  if (typeof(op_list) != 'list'){
    stop('Invalid argument for op_list. Please use list of operations (e.g. list(sum = c("enrolled") ) )')
  }
  if (length(dplyr::setdiff( names(op_list), c('mean', 'sum', 'median', 'sd', 'var') ) ) != 0){
    stop("Invalid argument in op_list. Please use one of the following operations c('mean', 'sum', 'median', 'sd', 'var').")
  }
  if(length(op_list) == 0){
    return(df)
  } else {
    if ('sum' %in% names(op_list)) {
      sum_df <- df %>%
        group_by(...) %>%
        summarise_at(
          .vars = op_list$sum,
          .funs = ~sum(., na.rm = TRUE)
        ) %>%
        ungroup()
    } else {
      sum_df <- data.frame()
    }
    if ('mean' %in% names(op_list)) {
      mean_df <- df %>%
        group_by(...) %>%
        summarise_at(
          .vars = op_list$mean,
          .funs = ~round(mean(., na.rm = TRUE) + 1e-10, 1)
        ) %>%
        ungroup()
    } else {
      mean_df <- data.frame()
    }
    if ('median' %in% names(op_list)) {
      median_df <- df %>%
        group_by(...) %>%
        summarise_at(
          .vars = op_list$median,
          .funs = ~round(median(., na.rm = TRUE) + 1e-10, 1)
        ) %>%
        ungroup()
    } else {
      median_df <- data.frame()
    }
    if ('sd' %in% names(op_list)) {
      sd_df <- df %>%
        group_by(...) %>%
        summarise_at(
          .vars = op_list$sd,
          .funs = ~round(sd(., na.rm = TRUE) + 1e-10, 1)
        ) %>%
        ungroup()
    } else {
      sd_df <- data.frame()
    }
    if ('var' %in% names(op_list)) {
      var_df <- df %>%
        group_by(...) %>%
        summarise_at(
          .vars = op_list$var,
          .funs = ~round(var(., na.rm = TRUE) + 1e-10, 1)
        ) %>%
        ungroup()
    } else {
      var_df <- data.frame()
    }
    for (df in list(sum_df, mean_df, median_df, sd_df, var_df)) {
      if(length(df > 0)){
        base_df <- df
        break
      }
    }
    base_suffix <- case_when(
      identical(base_df, sum_df) ~ '_sum',
      identical(base_df, mean_df) ~ '_avg',
      identical(base_df, median_df) ~ '_median',
      identical(base_df, sd_df) ~ '_sd',
      identical(base_df, var_df) ~ '_var'
    )
    out_df <- base_df
    for (df in list(sum_df, mean_df, median_df, sd_df, var_df)) {
      if(length(df > 0)){
        agg_suffix <- case_when(
          identical(df, sum_df) ~ '_sum',
          identical(df, mean_df) ~ '_avg',
          identical(df, median_df) ~ '_median',
          identical(df, sd_df) ~ '_sd',
          identical(df, var_df) ~ '_var'
        )
        if(!identical(df, base_df)){
          if(length( as.character(substitute(...() ) ) ) > 0){
            out_df <- left_join(out_df, df, by = as.character(substitute(...() ) ), suffix = c(base_suffix , agg_suffix) )
          } else {
            out_df <- bind_cols(out_df, df)
          }
        }
      }
    }
    return(out_df)
  }
}
