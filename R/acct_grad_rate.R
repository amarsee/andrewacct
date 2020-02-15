#' Accountability Achievement Calculations
#'
#' This function creates Accountability dataframe for the Graduation Rate Indicator
#'
#' @param school_grad_rate_path Path to the School Level Gradutation Rate file
#' @param grade_pools_path Path to the Grade Pools file
#' @param school_names_path Path to the file with School/System names
#' @param grad_amo_path Path to the AMO file for the Graduation Rate Indicator
#' @param a_cut Cut score for 'A' in Absolute Pathway
#' @param b_cut Cut score for 'B' in Absolute Pathway
#' @param c_cut Cut score for 'C' in Absolute Pathway
#' @param d_cut Cut score for 'D' in Absolute Pathway
#' @param min_n_count Minimum N Count needed to receive score
#'
#' @keywords graduation rate, indicator, accountability
#'
#' @examples
#' \dontrun{
#' acct_grad_rate("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv",
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' "N:/ORP_accountability/projects/2019_amo/grad_school.csv",
#' 95, 90, 80, 67, min_n_count= 30)
#' }
#'
#' @export


acct_grad_rate <- function(school_grad_rate_path, grade_pools_path, school_names_path,
                             grad_amo_path,
                             a_cut = 95, b_cut = 90, c_cut = 80, d_cut = 67, min_n_count = 30){

  grade_pools <- readr::read_csv(grade_pools_path) %>%
    dplyr::select(.data$system, .data$school, .data$pool, .data$designation_ineligible)

  school_df <- readr::read_csv(school_names_path)

  amo_grad <- readr::read_csv(grad_amo_path) %>%
    dplyr::filter(!grepl("Non-", .data$subgroup)) %>%
    dplyr::transmute(
      .data$system, .data$school,
      subgroup = dplyr::case_when(
        .data$subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ .data$subgroup
      ),
      metric_prior = dplyr::if_else(.data$grad_cohort >= min_n_count, .data$grad_rate, NA_real_),
      .data$AMO_target, .data$AMO_target_double
    ) # %>%
  # filter(!(subgroup == "Native Hawaiian or Other Pacific Islander" & n_count == 0))

  grad <- readr::read_csv(school_grad_rate_path) %>%
    dplyr::filter(.data$school !=0, .data$system != 0, !grepl("Non-", .data$subgroup), !(.data$subgroup %in% c('Male', 'Female', 'Homeless', 'Migrant')),
           !(.data$system == 90 & .data$school == 7)) %>%
    dplyr::transmute(
      .data$system, .data$system_name, .data$school, .data$school_name, indicator = "Graduation Rate",
      subgroup = dplyr::case_when(
        .data$subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ .data$subgroup
      ),
      n_count = dplyr::if_else(.data$grad_cohort >= min_n_count, .data$grad_cohort, 0),
      # n_count = if_else(grad_cohort >= 20, grad_cohort, 0),
      metric = dplyr::if_else(.data$n_count > 0, .data$grad_rate, NA_real_)
    ) %>%
    andrewacct::confidence_interval() %>%
    dplyr::left_join(amo_grad, by = c('system', 'school', 'subgroup')) %>%
    dplyr::mutate(
      score_abs = dplyr::case_when(
        .data$metric >= a_cut ~ 4,
        .data$metric >= b_cut ~ 3,
        .data$metric >= c_cut ~ 2,
        .data$metric >= d_cut ~ 1,
        .data$metric >= 0 ~ 0,
        TRUE ~ NA_real_
      ),
      score_target = dplyr::case_when(
        .data$metric >= .data$AMO_target_double ~ 4,
        .data$metric >= .data$AMO_target ~ 3,
        .data$ci_bound >= .data$AMO_target ~ 2,
        .data$ci_bound > .data$metric_prior ~ 1,
        .data$ci_bound <= .data$metric_prior ~ 0,
        TRUE ~ NA_real_
      ),
      score = pmax(.data$score_abs, .data$score_target),
      grade = dplyr::case_when(
        .data$score == 4 ~ 'A',
        .data$score == 3 ~ 'B',
        .data$score == 2 ~ 'C',
        .data$score == 1 ~ 'D',
        .data$score == 0 ~ 'F',
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::left_join(grade_pools, by = c("system", "school")) %>%
    dplyr::select(.data$system, .data$school, .data$indicator, .data$subgroup:.data$designation_ineligible) %>%
    dplyr::left_join(school_df, by = c('system', 'school')) %>%
    dplyr::transmute(.data$system, .data$system_name, .data$school, .data$school_name,
                     .data$pool, .data$designation_ineligible, .data$indicator,
                     .data$subgroup, .data$n_count, .data$metric,
                     .data$ci_bound, .data$metric_prior, .data$AMO_target, .data$AMO_target_double,
                     .data$score_abs, .data$score_target, .data$score, .data$grade)

  return(grad)

}
