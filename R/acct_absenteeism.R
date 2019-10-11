#' Accountability Chronic Absenteeism Calculations
#'
#' This function creates Accountability dataframe for the Chronic Absenteeism Indicator
#' @param absenteeism_student_level_path Path to the Student Level Chronic Absenteeism file
#' @param grade_pools_path Path to the Grade Pools file
#' @param school_names_path Path to the file with School/System names
#' @param absenteeism_amo_path Path to the AMO file for the Chronic Absenteeism Indicator
#' @param a_cut_k8 Cut score for 'A' in Absolute Pathway, K8 pool
#' @param b_cut_k8 Cut score for 'B' in Absolute Pathway, K8 pool
#' @param c_cut_k8 Cut score for 'C' in Absolute Pathway, K8 pool
#' @param d_cut_k8 Cut score for 'D' in Absolute Pathway, K8 pool
#' @param a_cut_hs Cut score for 'A' in Absolute Pathway, HS pool
#' @param b_cut_hs Cut score for 'B' in Absolute Pathway, HS pool
#' @param c_cut_hs Cut score for 'C' in Absolute Pathway, HS pool
#' @param d_cut_hs Cut score for 'D' in Absolute Pathway, HS pool
#' @param min_n_count Minimum N Count needed to receive score
#' @keywords absenteeism, indicator, accountability
#' @examples
#' acct_absenteeism("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11.csv",
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' "N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school_primary_enrollment.csv",
#' a_cut_k8 = 6, b_cut_k8 = 9, c_cut_k8 = 13, d_cut_k8 = 20,
#' a_cut_hs = 10, b_cut_hs = 14, c_cut_hs = 20, d_cut_hs = 30,
#' min_n_count = 30)
#' @export


acct_absenteeism <- function(absenteeism_student_level_path, grade_pools_path, school_names_path,
                           absenteeism_amo_path,
                           a_cut_k8 = 6, b_cut_k8 = 9, c_cut_k8 = 13, d_cut_k8 = 20,
                           a_cut_hs = 10, b_cut_hs = 14, c_cut_hs = 20, d_cut_hs = 30,
                           min_n_count = 30){

  grade_pools <- read_csv(grade_pools_path) %>%
    select(system, school, pool, designation_ineligible)

  school_df <- read_csv(school_names_path)

  amo_absenteeism <- read_csv(absenteeism_amo_path,
                              col_types = "iicicccninnn") %>%
    transmute(
      system, school,
      subgroup = case_when(
        subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ subgroup
      ),
      metric_prior = if_else(n_students >= min_n_count, pct_chronically_absent, NA_real_),
      AMO_target = AMO_target, AMO_target_double = AMO_target_double
    )

  absenteeism <- read_csv(absenteeism_student_level_path,
                          col_types = "icicccnnn") %>%
    filter(school !=0, system != 0) %>%
    transmute(
      system, school, indicator = 'Chronic Absenteeism',
      subgroup = case_when(
        subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ subgroup
      ),
      n_count = ifelse(n_students >= min_n_count, n_students, 0),
      metric = ifelse(n_count > 0, pct_chronically_absent, NA_real_)
    ) %>%
    confidence_interval(bound = 'lower') %>%
    left_join(amo_absenteeism, by = c('system', 'school', 'subgroup')) %>%
    left_join(grade_pools, by = c("system", "school"))  %>%
    mutate(
      score_abs = case_when(
        pool == "K8" & metric <= a_cut_k8 ~ 4,
        pool == "K8" & metric <= b_cut_k8 ~ 3,
        pool == "K8" & metric <= c_cut_k8 ~ 2,
        pool == "K8" & metric <= d_cut_k8 ~ 1,
        pool == "K8" & metric > d_cut_k8 ~ 0,
        pool == "HS" & metric <= a_cut_hs ~ 4,
        pool == "HS" & metric <= b_cut_hs ~ 3,
        pool == "HS" & metric <= c_cut_hs ~ 2,
        pool == "HS" & metric <= d_cut_hs ~ 1,
        pool == "HS" & metric > d_cut_hs ~ 0,
        TRUE ~ NA_real_
      ),
      score_target = case_when(
        metric <= AMO_target_double ~ 4,
        metric <= AMO_target ~ 3,
        metric < metric_prior ~ 2,
        metric == metric_prior ~ 1,
        metric > metric_prior ~ 0,
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
    left_join(school_df, by = c('system', 'school')) %>%
    transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, n_count, metric,
              ci_bound, metric_prior, AMO_target, AMO_target_double, score_abs, score_target, score, grade)

  return(absenteeism)

}
