#' Accountability Ready Graduate Calculations
#'
#' This function creates Accountability dataframe for the Ready Graduate Indicator
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
#' @examples
#' acct_ready_grad('N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level_06182019.csv',
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' "N:/ORP_accountability/projects/2019_amo/ready_grad_school.csv",
#' "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school.csv",
#' a_cut = 40, b_cut = 30, c_cut = 25, d_cut = 16, min_n_count = 30)
#' @export


acct_ready_grad <- function(ready_grad_student_level_path, grade_pools_path, school_names_path,
                             ready_grad_amo_path, ready_grad_school_path,
                             a_cut = 40, b_cut = 30, c_cut = 25, d_cut = 16, min_n_count = 30){

  grade_pools <- read_csv(grade_pools_path) %>%
    select(system, school, pool, designation_ineligible)

  school_df <- read_csv(school_names_path)

  # ========================== ACT/SAT Participation =============================================
  subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4",
                 "Students with Disabilities", "Super Subgroup", "American Indian or Alaska Native", "Asian", "Black or African American",
                 "Hispanic", "White")

  act_partic_concat <- function(df, subgroup_list){
    out_df_act_partic <- df %>%
      mutate(
        subgroup = "All Students"
      )
    for (subgroup in subgroup_list){
      student_df <- df
      if (subgroup == "Black/Hispanic/Native American"){
        hist_df <- student_df %>%
          filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I') %>%
          mutate(subgroup = "Black/Hispanic/Native American")
        non_hist_df <- 0
      } else if (subgroup == "Economically Disadvantaged") {
        hist_df <- student_df %>%
          filter(econ_dis == 'Y') %>%
          mutate(subgroup = "Economically Disadvantaged")
        non_hist_df <- 0
      }else if (subgroup == "English Learners with Transitional 1-4") {
        hist_df <- student_df %>%
          filter(elb == 'Y') %>%
          mutate(subgroup = "English Learners with Transitional 1-4")
        non_hist_df <- 0
      }else if (subgroup == "Super Subgroup"){
        hist_df <- student_df %>%
          filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I' | elb == 'Y' | econ_dis == 'Y' | swd == 'Y') %>%
          mutate(subgroup = "Super Subgroup")
        non_hist_df <- 0
      }else if (subgroup == "American Indian or Alaska Native"){
        hist_df <- student_df %>%
          filter(race_ethnicity == 'I') %>%
          mutate(subgroup = "American Indian or Alaska Native")
        non_hist_df <- 0
      }else if (subgroup ==  "Asian"){
        hist_df <- student_df %>%
          filter(race_ethnicity == 'A') %>%
          mutate(subgroup = "Asian")
        non_hist_df <- 0
      }else if (subgroup ==  "Black or African American"){
        hist_df <- student_df %>%
          filter(race_ethnicity == 'B') %>%
          mutate(subgroup = "Black or African American")
        non_hist_df <- 0
      }else if (subgroup ==  "Hispanic"){
        hist_df <- student_df %>%
          filter(race_ethnicity == 'H') %>%
          mutate(subgroup = "Hispanic")
        non_hist_df <- 0
      }else if (subgroup ==  "White"){
        hist_df <- student_df %>%
          filter(race_ethnicity == 'W') %>%
          mutate(subgroup = "White")
        non_hist_df <- 0
      }else {
        hist_df <- student_df %>%
          filter(swd == 'Y') %>%
          mutate(subgroup = "Students with Disabilities")
        non_hist_df <- 0
      }

      out_df_act_partic <- rbind(out_df_act_partic, hist_df)
    }
    return(out_df_act_partic)
  }

  ready_grad_participation_rate <- read_csv(ready_grad_student_level_path,
                                            col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic') %>%
    rename(system = district_no, school = school_no) %>%
    mutate(
      cohort_indicator = if_else(included_in_cohort == 'Y', 1, 0),
      ready_grad_indicator = if_else(ready_graduate == 'Y', 1, 0),
      completed_act_or_sat = if_else((sat_total > 0 | act_composite > 0) & included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0),
      on_time_grad = if_else(included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0)
    ) %>%
    act_partic_concat(subgroups) %>%
    group_by(system, school, subgroup) %>%
    summarise(
      n_on_time_grads = sum(on_time_grad, na.rm = TRUE),
      n_completed_act_or_sat = sum(completed_act_or_sat, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      participation_rate = round(n_completed_act_or_sat/n_on_time_grads * 100 + 1e-5, 0)
    ) %>%
    arrange(system, school, subgroup) %>%
    mutate(participation_rate = if_else(n_on_time_grads < min_n_count, NA_real_, participation_rate))


  # ====================== Ready Grad ========================

  amo_ready_grad <- read_csv(ready_grad_amo_path) %>%
    filter(!grepl("Non-", subgroup)) %>%
    transmute(
      system, school,
      subgroup = case_when(
        subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ subgroup
      ),
      metric_prior = if_else(grad_cohort >= min_n_count, pct_ready_grad, NA_real_),
      AMO_target, AMO_target_double
    )

  ready_grad <- read_csv(ready_grad_school_path) %>%
    rename(participation_rate = act_participation_rate) %>%
    filter(school !=0, system != 0, !grepl("Non-", subgroup)) %>% #
    transmute(
      system, school, indicator = 'Ready Graduates',
      subgroup = case_when(
        subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
        TRUE ~ subgroup
      ), participation_rate,
      n_count = ifelse(n_count >= min_n_count, n_count, 0),
      # n_count = ifelse(n_count >= 20, n_count, 0),
      metric = ifelse(n_count > 0, pct_ready_grad, NA_real_)
    ) %>%
    confidence_interval() %>%
    left_join(amo_ready_grad, by = c('system', 'school', 'subgroup')) %>%
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
      score = pmax(score_abs, score_target)
    ) %>%
    left_join(grade_pools, by = c("system", "school")) %>%
    select(system, school, indicator, subgroup:designation_ineligible) %>%
    left_join(school_df, by = c('system', 'school')) %>%
    transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, participation_rate, n_count, metric,
              ci_bound, metric_prior, AMO_target, AMO_target_double, score_abs, score_target, score) %>%
    mutate(
      participation_rate = if_else(n_count == 0, NA_real_, participation_rate),
      score_abs = if_else(!is.na(participation_rate) & participation_rate < 95 & !is.na(score_abs) , 0, score_abs),
      score_target = if_else(!is.na(participation_rate) & participation_rate < 95 & !is.na(score_target), 0, score_target),
      score = if_else(!is.na(participation_rate) & participation_rate < 95 & !is.na(score), 0, score),
      grade = case_when(
        score == 4 ~ 'A',
        score == 3 ~ 'B',
        score == 2 ~ 'C',
        score == 1 ~ 'D',
        score == 0 ~ 'F',
        TRUE ~ NA_character_
      )
    ) %>%
    select(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, participation_rate,
           n_count, metric, ci_bound, metric_prior, AMO_target, AMO_target_double, score_abs, score_target, score, grade)

  return(ready_grad)

}
