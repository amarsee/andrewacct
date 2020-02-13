#' Calculates Percentages
#'
#' This function takes a dataframe and a list of columns to find a percentage for, along with a single column to use
#' in the denominator. It returns a dataframe
#' @param df Dataframe. Used first to be friendly to the \%>\% operator.
#' @param numerator_cols A character vector of columns to be used in the numerator.
#' @param denom_col A single string of the column name to be used in the denominator.
#' @keywords percent, percentage, calculate
#'
#' @examples
#' \dontrun{
#' calc_pcts(system_level, c('on_track', 'mastered'), 'valid_tests')
#' }
#'
#' @export


calc_pcts <- function(df, numerator_cols, denom_col){
  out_df <- df %>%
    dplyr::mutate_at(
      .vars = numerator_cols,
      .funs = list(pct = ~ round(. / (!!as.name(denom_col)) * 100 + 1e-10, 1))
    ) %>%
    dplyr::rename_at( vars( contains( "_pct") ), list( ~paste("pct", gsub("_pct|n_", "", .), sep = "_") ) )
  return(out_df)
}
