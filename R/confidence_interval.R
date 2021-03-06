#' A Confidence Interval Function
#'
#' This function creates a new column with the desired confidence interval bound. Names the new column ci_bound.
#'
#' @param df Dataframe. Used first to be friendly to the \%>\% operator.
#' @param metric_column The column name desired for confidence interval. Defaults to metric
#' @param denom_column Column name of the denominator used. Defaults to n_count
#' @param interval The percent confidence for the interval. Defaults to 95.
#' @param bound The side of the confidence interval. Defaults to upper. Choices are upper or lower.
#' @keywords confidence interval, confidence, interval, ci
#' @importFrom stats qnorm
#'
#' @examples
#' \dontrun{
#' confidence_interval()
#' }
#'@export

confidence_interval <- function(df, metric_column = 'metric', denom_column = 'n_count', interval = 95, bound = 'upper') {
  ci_fraction <- 1 - ((1 - interval / 100) / 2)
  if (bound == 'upper') {
  # Upper Bound
  out_df <- df %>%
    dplyr::mutate(
      ci_bound = round(100 * ((!!as.name(denom_column))/((!!as.name(denom_column)) + (stats::qnorm(ci_fraction)^2)))*(((!!as.name(metric_column))/100) + ((stats::qnorm(ci_fraction)^2)/(2*(!!as.name(denom_column))))  +
                                                                       stats::qnorm(ci_fraction)* sqrt( ((((!!as.name(metric_column))/100) * (1 - ((!!as.name(metric_column))/100)))/ (!!as.name(denom_column))) + ((stats::qnorm(ci_fraction)^2) / (4* (!!as.name(denom_column))^2)))) + 1e-10,1)
    )
  return(out_df)
  } else if (bound == 'lower') {
  # Lower Bound
    out_df <- df %>%
      dplyr::mutate(
        ci_bound = round(100 * ((!!as.name(denom_column))/((!!as.name(denom_column)) + (stats::qnorm(ci_fraction)^2)))*(((!!as.name(metric_column))/100) + ((stats::qnorm(ci_fraction)^2)/(2*(!!as.name(denom_column))))  -
                                                                                                                                  stats::qnorm(ci_fraction)* sqrt( ((((!!as.name(metric_column))/100) * (1 - ((!!as.name(metric_column))/100)))/ (!!as.name(denom_column))) + ((stats::qnorm(ci_fraction)^2) / (4* (!!as.name(denom_column))^2)))) + 1e-10,1)
      )
  return(out_df)
  } else {
    stop('Invalid value for "bound": Please use "lower" or "upper"')
  }
}
