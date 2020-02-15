#' Accountability Ready Graduate Calculations
#'
#' This function creates Accountability dataframe for the Ready Graduate Indicator
#'
#' @param ready_grad_student_level_path Path to the Ready Graduate Student Level file (For Participation Rate)
#' @param grade_pools_path Path to the Grade Pools file
#' @param school_names_path Path to the file with School/System names
#' @param ready_grad_amo_path Path to the AMO file for the Ready Graduate Indicator
#' @param ready_grad_school_path Path to the school level Ready Graduate file
#' @param a_cut Cut score for 'A' in Absolute Pathway
#' @param b_cut Cut score for 'B' in Absolute Pathway
#' @param c_cut Cut score for 'C' in Absolute Pathway
#' @param d_cut Cut score for 'D' in Absolute Pathway
#' @param min_n_count Minimum N Count needed to receive score
#' @keywords achievement, indicator, accountability
#'
#' @examples
#' \dontrun{
#' acct_ready_grad('N:/ORP_accountability/projects/2019_ready_graduate/Data/
#' ready_graduate_student_level_06182019.csv',
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' "N:/ORP_accountability/projects/2019_amo/ready_grad_school.csv",
#' "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school.csv",
#' a_cut = 40, b_cut = 30, c_cut = 25, d_cut = 16, min_n_count = 30)
#' }
#'
#' @export


acct_ready_grad <- function(ready_grad_student_level_path, grade_pools_path, school_names_path,
                             ready_grad_amo_path, ready_grad_school_path,
                             a_cut = 40, b_cut = 30, c_cut = 25, d_cut = 16, min_n_count = 30){

  grade_pools <- readr::read_csv(grade_pools_path) %>%
    dplyr::select(.data$system, .data$school, .data$pool, .data$designation_ineligible)

  school_df <- readr::read_csv(school_names_path)

  # ========================== ACT/SAT Participation =============================================
  subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4",
                 "Students with Disabilities", "Super Subgroup", "American Indian or Alaska Native", "Asian", "Black or African American",
                 "Hispanic", "White")

  act_partic_concat <- function(df, subgroup_list){
    out_df_act_partic <- df %>%
      dplyr::mutate(
        subgroup = "All Students"
      )
    for (subgroup in subgroup_list){
      student_df <- df
      if (subgroup == "Black/Hispanic/Native American"){
        hist_df <- student_df %>%
          dplyr::filter(.data$race_ethnicity == 'B' | .data$race_ethnicity == 'H' |
                          .data$race_ethnicity == 'I') %>%
          dplyr::mutate(subgroup = "Black/Hispanic/Native American")
        non_hist_df <- 0
      } else if (subgroup == "Economically Disadvantaged") {
        hist_df <- student_df %>%
          dplyr::filter(.data$econ_dis == 'Y') %>%
          dplyr::mutate(subgroup = "Economically Disadvantaged")
        non_hist_df <- 0
      }else if (subgroup == "English Learners with Transitional 1-4") {
        hist_df <- student_df %>%
          dplyr::filter(.data$elb == 'Y') %>%
          dplyr::mutate(subgroup = "English Learners with Transitional 1-4")
        non_hist_df <- 0
      }else if (subgroup == "Super Subgroup"){
        hist_df <- student_df %>%
          dplyr::filter(.data$race_ethnicity == 'B' | .data$race_ethnicity == 'H' | .data$race_ethnicity == 'I' |
                          .data$elb == 'Y' | .data$econ_dis == 'Y' | .data$swd == 'Y') %>%
          dplyr::mutate(subgroup = "Super Subgroup")
        non_hist_df <- 0
      }else if (subgroup == "American Indian or Alaska Native"){
        hist_df <- student_df %>%
          dplyr::filter(.data$race_ethnicity == 'I') %>%
          dplyr::mutate(subgroup = "American Indian or Alaska Native")
        non_hist_df <- 0
      }else if (subgroup ==  "Asian"){
        hist_df <- student_df %>%
          dplyr::filter(.data$race_ethnicity == 'A') %>%
          dplyr::mutate(subgroup = "Asian")
        non_hist_df <- 0
      }else if (subgroup ==  "Black or African American"){
        hist_df <- student_df %>%
          dplyr::filter(.data$race_ethnicity == 'B') %>%
          dplyr::mutate(subgroup = "Black or African American")
        non_hist_df <- 0
      }else if (subgroup ==  "Hispanic"){
        hist_df <- student_df %>%
          dplyr::filter(.data$race_ethnicity == 'H') %>%
          dplyr::mutate(subgroup = "Hispanic")
        non_hist_df <- 0
      }else if (subgroup ==  "White"){
        hist_df <- student_df %>%
          dplyr::filter(.data$race_ethnicity == 'W') %>%
          dplyr::mutate(subgroup = "White")
        non_hist_df <- 0
      }else {
        hist_df <- student_df %>%
          dplyr::filter(.data$swd == 'Y') %>%
          dplyr::mutate(subgroup = "Students with Disabilities")
        non_hist_df <- 0
      }

      out_df_act_partic <- rbind(out_df_act_partic, hist_df)
    }
    return(out_df_act_partic)
  }

  ready_grad_participation_rate <- readr::read_csv(ready_grad_student_level_path,
                                            col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic') %>%
    dplyr::rename(system = .data$district_no, school = .data$school_no) %>%
    dplyr::mutate(
      cohort_indicator = dplyr::if_else(.data$included_in_cohort == 'Y', 1, 0),
      ready_grad_indicator = dplyr::if_else(.data$ready_graduate == 'Y', 1, 0),
      completed_act_or_sat = dplyr::if_else((.data$sat_total > 0 | .data$act_composite > 0) & .data$included_in_cohort == 'Y' & .data$completion_type %in% c(1, 11, 12, 13), 1, 0),
      on_time_grad = dplyr::if_else(.data$included_in_cohort == 'Y' & .data$completion_type %in% c(1, 11, 12, 13), 1, 0)
    ) %>%
    act_partic_concat(subgroups) %>%
    dplyr::group_by(.data$system, .data$school, .data$subgroup) %>%
    dplyr::summarise(
      n_on_time_grads = sum(.data$on_time_grad, na.rm = TRUE),
      n_completed_act_or_sat = sum(.data$completed_act_or_sat, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      participation_rate = round(.data$n_completed_act_or_sat/.data$n_on_time_grads * 100 + 1e-5, 0)
    ) %>%
    dplyr::arrange(.data$system, .data$school, .data$subgroup) %>%
    dplyr::mutate(participation_rate = dplyr::if_else(.data$n_on_time_grads < min_n_count, NA_real_, .data$participation_rate))


  # ====================== Ready Grad ========================

  amo_ready_grad <- readr::read_csv(ready_grad_amo_path) %>%
    dplyr::filter(!grepl("Non-", .data$subgroup)) %>%
    dplyr::transmute(
      .data$system, .data$school,
      subgroup = dplyr::case_when(
        .data$subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ .data$subgroup
      ),
      metric_prior = dplyr::if_else(.data$grad_cohort >= min_n_count, .data$pct_ready_grad, NA_real_),
      .data$AMO_target, .data$AMO_target_double
    )

  ready_grad <- readr::read_csv(ready_grad_school_path) %>%
    dplyr::rename(participation_rate = .data$act_participation_rate) %>%
    dplyr::filter(.data$school !=0, .data$system != 0, !grepl("Non-", .data$subgroup)) %>% #
    dplyr::transmute(
      .data$system, .data$school, indicator = 'Ready Graduates',
      subgroup = dplyr::case_when(
        .data$subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ .data$subgroup
      ), .data$participation_rate,
      n_count = ifelse(.data$n_count >= min_n_count, .data$n_count, 0),
      # n_count = ifelse(n_count >= 20, n_count, 0),
      metric = ifelse(.data$n_count > 0, .data$pct_ready_grad, NA_real_)
    ) %>%
    andrewacct::confidence_interval() %>%
    dplyr::left_join(amo_ready_grad, by = c('system', 'school', 'subgroup')) %>%
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
      score = pmax(.data$score_abs, .data$score_target)
    ) %>%
    dplyr::left_join(grade_pools, by = c("system", "school")) %>%
    dplyr::select(.data$system, .data$school, .data$indicator, .data$subgroup:.data$designation_ineligible) %>%
    dplyr::left_join(school_df, by = c('system', 'school')) %>%
    dplyr::transmute(.data$system, .data$system_name, .data$school, .data$school_name,
                     .data$pool, .data$designation_ineligible, .data$indicator,
                     .data$subgroup, .data$participation_rate, .data$n_count, .data$metric,
                     .data$ci_bound, .data$metric_prior, .data$AMO_target,
                     .data$AMO_target_double, .data$score_abs, .data$score_target,
                     .data$score) %>%
    dplyr::mutate(
      participation_rate = dplyr::if_else(.data$n_count == 0, NA_real_, .data$participation_rate),
      score_abs = dplyr::if_else(!is.na(.data$participation_rate) & .data$participation_rate < 95 & !is.na(.data$score_abs) , 0, .data$score_abs),
      score_target = dplyr::if_else(!is.na(.data$participation_rate) & .data$participation_rate < 95 & !is.na(.data$score_target), 0, .data$score_target),
      score = dplyr::if_else(!is.na(.data$participation_rate) & .data$participation_rate < 95 & !is.na(.data$score), 0, .data$score),
      grade = dplyr::case_when(
        .data$score == 4 ~ 'A',
        .data$score == 3 ~ 'B',
        .data$score == 2 ~ 'C',
        .data$score == 1 ~ 'D',
        .data$score == 0 ~ 'F',
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(.data$system, .data$system_name, .data$school, .data$school_name,
                  .data$pool, .data$designation_ineligible, .data$indicator, .data$subgroup,
                  .data$participation_rate, .data$n_count, .data$metric,
                  .data$ci_bound, .data$metric_prior, .data$AMO_target,
                  .data$AMO_target_double, .data$score_abs, .data$score_target,
                  .data$score, .data$grade)

  return(ready_grad)

}
