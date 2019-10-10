#' Accountability Achievement Calculations
#'
#' This function creates Accountability dataframe for the Achievement Indicator
#' @param sl_path Path to the Student Level file
#' @param grade_pools_path Path to the Grade Pools file
#' @param school_names_path Path to the file with School/System names
#' @param success_amo_path Path to the AMO file for the Achievement Indicator
#' @param act_substitution_path Path to the ACT Substitution file
#' @param school_assessment_path Path to the school level Assessment file
#' @param a_cut Cut score for 'A' in Absolute Pathway
#' @param b_cut Cut score for 'B' in Absolute Pathway
#' @param c_cut Cut score for 'C' in Absolute Pathway
#' @param d_cut Cut score for 'D' in Absolute Pathway
#' @param min_n_count Minimum N Count needed to receive score
#' @keywords achievement, indicator, accountability
#' @examples
#' acct_achievement("N://ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv",
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' "N:/ORP_accountability/projects/2019_amo/success_rate_targets_school.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv",
#' 45, 35, 27.5, 20, min_n_count= 30)
#' @export


acct_achievement <- function(sl_path, grade_pools_path, school_names_path,
                              success_amo_path, act_substitution_path, school_assessment_path,
                              a_cut, b_cut, c_cut, d_cut, min_n_count = 30){
  math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
  english_eoc <- c("English I", "English II")

  grade_pools <- read_csv(grade_pools_path) %>%
    select(system, school, pool, designation_ineligible)

  student_level <- read_csv(sl_path)

  sl <- student_level %>%
    filter(!(system == 964 & school == 964 | system == 970 & school == 970)) %>%
    mutate_at("residential_facility", ~ if_else(is.na(.), 0, .)) %>%
    mutate_at("enrolled_50_pct_school", ~ if_else(is.na(.), "Y", .)) %>%
    mutate(
      original_subject = case_when(
        grade < 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II",
                                            "Integrated Math III", 'English I', 'English II', 'Biology I', 'Chemistry') ~ subject,
        TRUE ~ original_subject
      )
    )

  school_df <- read_csv(school_names_path)

  integrated_math <- student_level %>%
    filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    count(system, original_subject) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    # Systems where Integrated Math is the max between that and Algebra I
    filter(n == temp, original_subject == "Integrated Math I")
  # Vector with the sytems where that is the case
  int_math_vec <- integrated_math[['system']]

  # Previous AMO data
  amo_achievement <- read_csv(success_amo_path) %>%
    transmute(system, school, subgroup, metric_prior = success_rate_prior, AMO_target, AMO_target_double)

  # ACT score substitution
  act_sub <- read_csv(act_substitution_path) %>%
    #left_join(school_df, by = c('system', 'school')) %>%
    transmute(system, system_name, school, school_name,
              subject = case_when(
                subject == "ACT Math" & system %in% int_math_vec ~ "Integrated Math III",
                TRUE  ~ "Algebra II"
              ), grade = 11, subgroup = "All Students", valid_test = valid_tests, on_track = n_met_benchmark, mastered = 0
    )

  # ========================================== Prep for Achievement =======================================
  sl <- sl %>%
    mutate(
      on_track = case_when(
        performance_level == "Proficient" | performance_level == "On Track" ~ 1,
        TRUE                      ~ 0
      ),
      mastered = case_when(
        performance_level == "Mastered" | performance_level == "Advanced" ~ 1,
        TRUE                      ~ 0
      )
    ) %>%
    filter(residential_facility == 0, (enrolled_50_pct_school == 'Y' | (acct_system != system | school != acct_school)),  # homebound == 0, !is.na(state_student_id),grade %in% 3:12,
           original_subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>%

    # fill(system_name) %>%
    rename(subgroup = reported_race)

  state_totals <- bind_rows(sl %>% bind_rows(act_sub %>% rename(acct_system = system, acct_school = school)) %>%
                              mutate(subgroup = "All Students"),
                            sl %>%
                              filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>%
                              mutate(subgroup = "Super Subgroup"),
                            sl, # Race Subgroups
                            sl %>% filter(bhn_group > 0) %>% mutate(subgroup = "Black/Hispanic/Native American"),
                            sl %>% filter(economically_disadvantaged > 0) %>% mutate(subgroup = "Economically Disadvantaged"),
                            sl %>% filter(t1234 > 0 | el > 0) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
                            sl %>% filter(special_ed > 0) %>% mutate(subgroup = "Students with Disabilities")) %>%
    total_by_subgroup() %>%
    filter(subgroup != "Unknown") %>%
    arrange(system, school, subject, subgroup) %>%
    # rename(subject = original_subject) %>%
    mutate(
      subgroup = case_when(
        subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
        subgroup == "Hispanic/Latino" ~ "Hispanic",
        subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
        TRUE ~ subgroup
      )
    ) %>%
    group_by(acct_system, acct_school, subject, subgroup) %>%
    summarise(
      enrolled = sum(enrolled, na.rm = TRUE),
      tested = sum(tested, na.rm = TRUE),
      valid_tests = sum( valid_test),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>%
    ungroup() %>%
    rename(system = acct_system, school = acct_school)

  # ====================== TCAP Participation Rate =======================
  school_assessment <- read_csv(school_assessment_path)

  tcap_participation <- school_assessment %>%
    filter(year == 2019, grade == 'All Grades', subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>%
    group_by(system, system_name, school, school_name, subgroup) %>%
    summarise(
      n_enrolled = sum(enrolled),
      n_tested = sum(tested)
    ) %>%
    ungroup() %>%
    mutate(
      participation_rate = round(n_tested/n_enrolled * 100 + 1e-5, 0)
    )

  # saveRDS(tcap_participation, "data/tcap_participation_2018.rds")

  # ============================= Achievement =========================

  school_achievement <- state_totals %>%
    mutate(
      subject = case_when(
        subject %in% english_eoc ~ "HS English",
        subject %in% c(math_eoc, "HS Math") ~ "HS Math",
        TRUE ~ subject
      )
    ) %>%
    group_by(system, school, subject, subgroup)  %>%
    summarise(
      enrolled = sum(enrolled, na.rm = TRUE),
      tested = sum(tested, na.rm = TRUE),
      valid_tests = sum(valid_tests, na.rm = TRUE),
      n_on_track = sum(n_on_track, na.rm = TRUE),
      n_mastered = sum(n_mastered, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      valid_tests = if_else(valid_tests >= min_n_count, valid_tests, 0),
      n_on_track = if_else(valid_tests >= min_n_count, n_on_track, 0),
      n_mastered = if_else(valid_tests >= min_n_count, n_mastered, 0)
    ) %>%
    group_by(system, school, subgroup)  %>%
    summarise(
      participation_rate = round5(100 * sum(tested)/sum(enrolled)),
      n_count = sum(valid_tests),
      success_rate = round(((sum(n_on_track) + sum(n_mastered))/sum(valid_tests)) * 100 + 1e-10, 1)
    ) %>%
    ungroup() %>%
    mutate(success_rate = ifelse(is.na(success_rate), NA, success_rate)) %>%
    left_join(grade_pools, by = c('system', 'school')) %>%
    left_join(school_df, by = c('system', 'school')) %>%
    # Filtered out if No pool or school name
    # filter(!is.na(pool), !is.na(school_name)) %>%
    transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator = 'Achievement',
              subgroup,participation_rate, n_count, metric = success_rate) %>%
    # Upper bound confidence interval function
    andrewacct::confidence_interval() %>%
    # Join amo target df
    left_join(amo_achievement, by = c('system', 'school', 'subgroup')) %>%
    mutate(
      score_abs = case_when(
        metric >= a_cut ~ 4,
        metric >= b_cut ~ 3,
        metric >= c_cut ~ 2,
        metric >= d_cut ~ 1,
        metric >= 0 ~ 0
      ),
      score_target = case_when(
        metric >= AMO_target_double ~ 4,
        metric >= AMO_target ~ 3,
        ci_bound >= AMO_target ~ 2,
        ci_bound > metric_prior ~ 1,
        ci_bound <= metric_prior ~ 0
      ),
      score = pmax(score_abs, score_target)
    ) %>%
    # Join tnready participation and set to 0 if below 95
    # left_join(tcap_participation %>% select(system:subgroup, participation_rate), by = c('system','system_name', 'school', 'school_name', 'subgroup')) %>%
    mutate(
      score_abs = if_else(participation_rate < 95 & n_count > 0 & !is.na(score_abs), 0, score_abs),
      score_target = if_else(participation_rate < 95 & n_count > 0 & !is.na(score_target), 0, score_target),
      score = if_else(participation_rate < 95 & n_count > 0 & !is.na(score), 0, score),
      participation_rate = if_else(n_count == 0, NA_real_, participation_rate),
      grade = case_when(
        score == 4 ~ 'A',
        score == 3 ~ 'B',
        score == 2 ~ 'C',
        score == 1 ~ 'D',
        score == 0 ~ 'F',
        TRUE ~ NA_character_
      )
    ) %>%
    # Select desired columns
    select(system:subgroup, participation_rate, n_count:grade)

  return(school_achievement)

}
