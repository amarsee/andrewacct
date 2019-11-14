#' Accountability Achievement Calculations
#'
#' This function creates Accountability dataframe for the Graduation Rate Indicator
#' @param school_grad_rate_path Path to the School Level Gradutation Rate file
#' @param grade_pools_path Path to the Grade Pools file
#' @param school_names_path Path to the file with School/System names
#' @param grad_amo_path Path to the AMO file for the Graduation Rate Indicator
#' @param a_cut Cut score for 'A' in Absolute Pathway
#' @param b_cut Cut score for 'B' in Absolute Pathway
#' @param c_cut Cut score for 'C' in Absolute Pathway
#' @param d_cut Cut score for 'D' in Absolute Pathway
#' @param min_n_count Minimum N Count needed to receive score
#' @keywords graduation rate, indicator, accountability
#' @examples
#' acct_grad_rate("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv",
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' "N:/ORP_accountability/projects/2019_amo/grad_school.csv",
#' 95, 90, 80, 67, min_n_count= 30)
#' @export


acct_grad_rate <- function(school_grad_rate_path, grade_pools_path, school_names_path,
                             grad_amo_path,
                             a_cut = 95, b_cut = 90, c_cut = 80, d_cut = 67, min_n_count = 30){

  grade_pools <- read_csv(grade_pools_path) %>%
    select(system, school, pool, designation_ineligible)

  school_df <- read_csv(school_names_path)

  amo_grad <- read_csv(grad_amo_path) %>%
    filter(!grepl("Non-", subgroup)) %>%
    transmute(
      system, school,
      subgroup = case_when(
        subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ subgroup
      ),
      metric_prior = if_else(grad_cohort >= min_n_count, grad_rate, NA_real_),
      AMO_target, AMO_target_double
    ) # %>%
  # filter(!(subgroup == "Native Hawaiian or Other Pacific Islander" & n_count == 0))

  grad <- read_csv(school_grad_rate_path) %>%
    filter(school !=0, system != 0, !grepl("Non-", subgroup), !subgroup %in% c('Male', 'Female', 'Homeless', 'Migrant'),
           !(system == 90 & school == 7)) %>%
    transmute(
      system, system_name, school, school_name, indicator = "Graduation Rate",
      subgroup = case_when(
        subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ subgroup
      ),
      n_count = if_else(grad_cohort >= min_n_count, grad_cohort, 0),
      # n_count = if_else(grad_cohort >= 20, grad_cohort, 0),
      metric = if_else(n_count > 0, grad_rate, NA_real_)
    ) %>%
    confidence_interval() %>%
    left_join(amo_grad, by = c('system', 'school', 'subgroup')) %>%
    mutate(
      score_abs = case_when(
        metric >= a_cut ~ 4,
        metric >= b_cut ~ 3,
        metric >= c_cut ~ 2,
        metric >= d_cut ~ 1,
        metric >= 0 ~ 0,
        TRUE ~ NA_real_
      ),
      score_target = case_when(
        metric >= AMO_target_double ~ 4,
        metric >= AMO_target ~ 3,
        ci_bound >= AMO_target ~ 2,
        ci_bound > metric_prior ~ 1,
        ci_bound <= metric_prior ~ 0,
        TRUE ~ NA_real_
      ),
      score = pmax(score_abs, score_target),
      grade = case_when(
        score == 4 ~ 'A',
        score == 3 ~ 'B',
        score == 2 ~ 'C',
        score == 1 ~ 'D',
        score == 0 ~ 'F',
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(grade_pools, by = c("system", "school")) %>%
    select(system, school, indicator, subgroup:designation_ineligible) %>%
    left_join(school_df, by = c('system', 'school')) %>%
    transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, n_count, metric,
              ci_bound, metric_prior, AMO_target, AMO_target_double, score_abs, score_target, score, grade)

  return(grad)

}
