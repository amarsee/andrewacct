#' Accountability Chronic Absenteeism Calculations
#'
#' This function creates Accountability dataframe for the Chronic Absenteeism Indicator
#' with 2020 cuts by default
#'
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
#'
#' @keywords absenteeism, indicator, accountability
#'
#' @examples
#' \dontrun{
#' acct_absenteeism("N:/ORP_accountability/data/2019_chronic_absenteeism/
#' school_chronic_absenteeism_Jul11.csv",
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' "N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school_primary_enrollment.csv",
#' a_cut_k8 = 6, b_cut_k8 = 9, c_cut_k8 = 13, d_cut_k8 = 20,
#' a_cut_hs = 10, b_cut_hs = 14, c_cut_hs = 20, d_cut_hs = 30,
#' min_n_count = 30)
#' }
#'
#' @export


acct_absenteeism <- function(absenteeism_student_level_path, grade_pools_path, school_names_path,
                           absenteeism_amo_path,
                           a_cut_k8 = 6, b_cut_k8 = 9, c_cut_k8 = 13, d_cut_k8 = 20,
                           a_cut_hs = 10, b_cut_hs = 14, c_cut_hs = 20, d_cut_hs = 30,
                           min_n_count = 30){

  grade_pools <- readr::read_csv(grade_pools_path) %>%
    dplyr::select(.data$system, .data$school, .data$pool, .data$designation_ineligible)

  school_df <- readr::read_csv(school_names_path)

  amo_absenteeism <- readr::read_csv(absenteeism_amo_path,
                              col_types = "iicicccninnn") %>%
    dplyr::transmute(
      .data$system, .data$school,
      subgroup = dplyr::case_when(
        .data$subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ subgroup
      ),
      metric_prior = dplyr::if_else(.data$n_students >= min_n_count, .data$pct_chronically_absent, NA_real_),
      AMO_target = .data$AMO_target, AMO_target_double = .data$AMO_target_double
    )

  absenteeism <- readr::read_csv(absenteeism_student_level_path,
                          col_types = "icicccnnn") %>%
    dplyr::filter(.data$school !=0, .data$system != 0) %>%
    dplyr::transmute(
      .data$system, .data$school, indicator = 'Chronic Absenteeism',
      subgroup = dplyr::case_when(
        .data$subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ .data$subgroup
      ),
      n_count = ifelse(.data$n_students >= min_n_count, .data$n_students, 0),
      metric = ifelse(.data$n_count > 0, .data$pct_chronically_absent, NA_real_)
    ) %>%
    andrewacct::confidence_interval(bound = 'lower') %>%
    dplyr::left_join(amo_absenteeism, by = c('system', 'school', 'subgroup')) %>%
    dplyr::left_join(grade_pools, by = c("system", "school"))  %>%
    dplyr::mutate(
      score_abs = dplyr::case_when(
        .data$pool == "K8" & .data$metric <= a_cut_k8 ~ 4,
        .data$pool == "K8" & .data$metric <= b_cut_k8 ~ 3,
        .data$pool == "K8" & .data$metric <= c_cut_k8 ~ 2,
        .data$pool == "K8" & .data$metric <= d_cut_k8 ~ 1,
        .data$pool == "K8" & .data$metric > d_cut_k8 ~ 0,
        .data$pool == "HS" & .data$metric <= a_cut_hs ~ 4,
        .data$pool == "HS" & .data$metric <= b_cut_hs ~ 3,
        .data$pool == "HS" & .data$metric <= c_cut_hs ~ 2,
        .data$pool == "HS" & .data$metric <= d_cut_hs ~ 1,
        .data$pool == "HS" & .data$metric > d_cut_hs ~ 0,
        TRUE ~ NA_real_
      ),
      score_target = dplyr::case_when(
        .data$metric <= .data$AMO_target_double ~ 4,
        .data$metric <= .data$AMO_target ~ 3,
        .data$ci_bound <= .data$metric ~ 2,
        .data$ci_bound < .data$metric_prior ~ 1,
        .data$ci_bound >= .data$metric_prior ~ 0,
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
    dplyr::left_join(school_df, by = c('system', 'school')) %>%
    dplyr::transmute(.data$system, .data$system_name, .data$school, .data$school_name,
                     .data$pool, .data$designation_ineligible, .data$indicator,
                     .data$subgroup, .data$n_count, .data$metric,
                     .data$ci_bound, .data$metric_prior, .data$AMO_target, .data$AMO_target_double,
                     .data$score_abs, .data$score_target, .data$score, .data$grade)

  return(absenteeism)

}
