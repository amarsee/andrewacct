#' A Confidence Interval Function
#'
#' This function creates a new column with the desired confidence interval bound.
#' @param df Dataframe. Used first to be friendly to the \%>\% operator.
#' @param metric_column The column name desired for confidence interval. Defaults to metric
#' @param denom_column Column name of the denominator used. Defaults to n_count
#' @param interval The percent confidence for the interval. Defaults to 95.
#' @param bound The side of the confidence interval. Defaults to upper. Choices are upper or lower.
#' @param out_column The name for the resulting column. Defaults to ci_bound
#' @keywords confidence interval, confidence, interval, ci
#' @export
#' @examples
#' confidence_interval()

confidence_interval <- function(df, metric_column = 'metric', denom_column = 'n_count', interval = 95, bound = 'upper', out_column = 'ci_bound') {
  ci_fraction <- 1 - ((1 - interval / 100) / 2)
  if (bound == 'upper') {
  # Upper Bound
  out_df <- df %>%
    mutate(
      out_column = round(100 * ((!!as.name(denom_column))/((!!as.name(denom_column)) + (qnorm(ci_fraction)^2)))*(((!!as.name(metric_column))/100) + ((qnorm(ci_fraction)^2)/(2*(!!as.name(denom_column))))  +
                                                                       qnorm(ci_fraction)* sqrt( ((((!!as.name(metric_column))/100) * (1 - ((!!as.name(metric_column))/100)))/ (!!as.name(denom_column))) + ((qnorm(ci_fraction)^2) / (4* (!!as.name(denom_column))^2)))) + 1e-10,1)
    )
  return(out_df)
  } else if (bound == 'lower') {
  # Lower Bound
  out_df <- df %>%
    mutate(
      out_column = round(100 * (denom_column/(denom_column + (qnorm(ci_fraction)^2)))*((metric_column/100) + ((qnorm(ci_fraction)^2)/(2*denom_column))  -
                                                                       qnorm(ci_fraction)* sqrt( (((metric_column/100) * (1 - (metric_column/100)))/ denom_column) + ((qnorm(ci_fraction)^2) / (4* denom_column^2)))) + 1e-10,1)
    )
  return(out_df)
  } else {
    stop('Invalid value for "bound": Please use "lower" or "upper"')
  }
}
