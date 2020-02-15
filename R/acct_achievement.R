#' Accountability Achievement Calculations
#'
#' This function creates Accountability dataframe for the Achievement Indicator
#' with the 2020 achievement indicator cuts by default
#'
#' @param student_level_df Dataframe with student level achievement data
#' @param grade_pools_path Path to the Grade Pools file
#' @param school_names_path Path to the file with School/System names
#' @param success_amo_path Path to the AMO file for the Achievement Indicator
#' @param act_substitution_path Path to the ACT Substitution file
#' @param school_assessment_df Dataframe with school level Assessment data
#' @param a_cut Cut score for 'A' in Absolute Pathway
#' @param b_cut Cut score for 'B' in Absolute Pathway
#' @param c_cut Cut score for 'C' in Absolute Pathway
#' @param d_cut Cut score for 'D' in Absolute Pathway
#' @param min_n_count Minimum N Count needed to receive score
#'
#' @keywords achievement, indicator, accountability
#'
#' @examples
#' \dontrun{
#' acct_achievement(sl_df,
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' "N:/ORP_accountability/projects/2019_amo/success_rate_targets_school.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv",
#' 45, 35, 27.5, 20, min_n_count= 30)
#' }
#'
#' @export


acct_achievement <- function(student_level_df, grade_pools_path, school_names_path,
                              success_amo_path, act_substitution_path, school_assessment_df,
                              a_cut, b_cut, c_cut, d_cut, min_n_count = 30){
  # Subjects included in EOC for acct subjects (Math/ELA)
  math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
  english_eoc <- c("English I", "English II")

  grade_pools <- readr::read_csv(grade_pools_path) %>%
    dplyr::select(.data$system, .data$school, .data$pool, .data$designation_ineligible)

  student_level <- student_level_df

  sl <- student_level %>%
    dplyr::filter(!(.data$system == 964 & .data$school == 964 |
                      .data$system == 970 & .data$school == 970)) %>%
    dplyr::mutate_at("residential_facility", ~ dplyr::if_else(is.na(.), 0, .)) %>%
    dplyr::mutate_at("enrolled_50_pct_school", ~ dplyr::if_else(is.na(.), "Y", .)) %>%
    dplyr::mutate(
      original_subject = dplyr::case_when(
        .data$grade < 9 & .data$original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II",
                                            "Integrated Math III", 'English I', 'English II', 'Biology I', 'Chemistry') ~ subject,
        TRUE ~ .data$original_subject
      )
    )

  school_df <- readr::read_csv(school_names_path)

  integrated_math <- student_level %>%
    dplyr::filter(.data$original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    dplyr::count(.data$system, .data$original_subject) %>%
    dplyr::group_by(.data$system) %>%
    dplyr::mutate(temp = max(.data$n)) %>%
    # Systems where Integrated Math is the max between that and Algebra I
    dplyr::filter(.data$n == .data$temp, .data$original_subject == "Integrated Math I")
  # Vector with the sytems where that is the case
  int_math_vec <- integrated_math %>%
    dplyr::pull(.data$system)

  # Previous AMO data
  amo_achievement <- readr::read_csv(success_amo_path) %>%
    dplyr::transmute(.data$system, .data$school, .data$subgroup, metric_prior = .data$success_rate_prior,
                     .data$AMO_target, .data$AMO_target_double)

  # ACT score substitution
  act_sub <- readr::read_csv(act_substitution_path) %>%
    #left_join(school_df, by = c('system', 'school')) %>%
    dplyr::transmute(.data$system, .data$system_name, .data$school, .data$school_name,
              subject = dplyr::case_when(
                .data$subject == "ACT Math" & .data$system %in% int_math_vec ~ "Integrated Math III",
                TRUE  ~ "Algebra II"
              ), grade = 11, subgroup = "All Students", valid_test = .data$valid_tests,
              on_track = .data$n_met_benchmark, mastered = 0
    )

  # ========================================== Prep for Achievement =======================================
  sl <- sl %>%
    dplyr::mutate(
      on_track = dplyr::case_when(
        .data$performance_level == "Proficient" | .data$performance_level == "On Track" ~ 1,
        TRUE                      ~ 0
      ),
      mastered = dplyr::case_when(
        .data$performance_level == "Mastered" | .data$performance_level == "Advanced" ~ 1,
        TRUE                      ~ 0
      )
    ) %>%
    dplyr::filter(.data$residential_facility == 0, (.data$enrolled_50_pct_school == 'Y' |
                                                      (.data$acct_system != .data$system | .data$school != .data$acct_school)),  # homebound == 0, !is.na(state_student_id),grade %in% 3:12,
                  .data$original_subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>%

    # fill(system_name) %>%
    dplyr::rename(subgroup = .data$reported_race)

  state_totals <- dplyr::bind_rows(sl %>% dplyr::bind_rows(act_sub %>% dplyr::rename(acct_system = .data$system, acct_school = .data$school)) %>%
                                     dplyr::mutate(subgroup = "All Students"),
                            sl %>%
                              dplyr::filter(.data$bhn_group > 0 | .data$economically_disadvantaged > 0 |
                                              .data$t1234 > 0 | .data$el > 0 | .data$special_ed > 0) %>%
                              dplyr::mutate(subgroup = "Super Subgroup"),
                            sl, # Race Subgroups
                            sl %>% dplyr::filter(.data$bhn_group > 0) %>% dplyr::mutate(subgroup = "Black/Hispanic/Native American"),
                            sl %>% dplyr::filter(.data$economically_disadvantaged > 0) %>% dplyr::mutate(subgroup = "Economically Disadvantaged"),
                            sl %>% dplyr::filter(.data$t1234 > 0 | .data$el > 0) %>% dplyr::mutate(subgroup = "English Learners with Transitional 1-4"),
                            sl %>% dplyr::filter(.data$special_ed > 0) %>% dplyr::mutate(subgroup = "Students with Disabilities")) %>%
    dplyr::filter(.data$subgroup != "Unknown") %>%
    dplyr::arrange(.data$system, .data$school, .data$subject, .data$subgroup) %>%
    # rename(subject = original_subject) %>%
    dplyr::mutate(
      subgroup = dplyr::case_when(
        .data$subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
        .data$subgroup == "Hispanic/Latino" ~ "Hispanic",
        .data$subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
        TRUE ~ .data$subgroup
      )
    ) %>%
    dplyr::group_by(.data$acct_system, .data$acct_school, .data$subject, .data$subgroup) %>%
    dplyr::summarise(
      enrolled = sum(.data$enrolled, na.rm = TRUE),
      tested = sum(.data$tested, na.rm = TRUE),
      valid_tests = sum( .data$valid_test),
      n_on_track = sum(.data$on_track),
      n_mastered = sum(.data$mastered)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(system = .data$acct_system, school = .data$acct_school)

  # ====================== TCAP Participation Rate =======================
  school_assessment <- school_assessment_df

  tcap_participation <- school_assessment %>%
    dplyr::filter(.data$year == 2019, .data$grade == 'All Grades',
                  .data$subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>%
    dplyr::group_by(.data$system, .data$system_name, .data$school, .data$school_name, .data$subgroup) %>%
    dplyr::summarise(
      n_enrolled = sum(.data$enrolled),
      n_tested = sum(.data$tested)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      participation_rate = round(.data$n_tested/.data$n_enrolled * 100 + 1e-5, 0)
    )

  # saveRDS(tcap_participation, "data/tcap_participation_2018.rds")

  # ============================= Achievement =========================

  school_achievement <- state_totals %>%
    dplyr::mutate(
      subject = dplyr::case_when(
        .data$subject %in% english_eoc ~ "HS English",
        .data$subject %in% c(math_eoc, "HS Math") ~ "HS Math",
        TRUE ~ .data$subject
      )
    ) %>%
    dplyr::group_by(.data$system, .data$school, .data$subject, .data$subgroup)  %>%
    dplyr::summarise(
      enrolled = sum(.data$enrolled, na.rm = TRUE),
      tested = sum(.data$tested, na.rm = TRUE),
      valid_tests = sum(.data$valid_tests, na.rm = TRUE),
      n_on_track = sum(.data$n_on_track, na.rm = TRUE),
      n_mastered = sum(.data$n_mastered, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      valid_tests = dplyr::if_else(.data$valid_tests >= min_n_count, .data$valid_tests, 0),
      n_on_track = dplyr::if_else(.data$valid_tests >= min_n_count, .data$n_on_track, 0),
      n_mastered = dplyr::if_else(.data$valid_tests >= min_n_count, .data$n_mastered, 0)
    ) %>%
    dplyr::group_by(.data$system, .data$school, .data$subgroup)  %>%
    dplyr::summarise(
      participation_rate = round(100 * sum(.data$tested)/sum(.data$enrolled)),
      n_count = sum(.data$valid_tests),
      success_rate = round(((sum(.data$n_on_track) + sum(.data$n_mastered))/sum(.data$valid_tests)) * 100 + 1e-10, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(success_rate = ifelse(is.na(.data$success_rate), NA, .data$success_rate)) %>%
    dplyr::left_join(grade_pools, by = c('system', 'school')) %>%
    dplyr::left_join(school_df, by = c('system', 'school')) %>%
    # Filtered out if No pool or school name
    # filter(!is.na(pool), !is.na(school_name)) %>%
    dplyr::transmute(.data$system, .data$system_name, .data$school, .data$school_name,
                     .data$pool, .data$designation_ineligible, indicator = 'Achievement',
                     .data$subgroup,.data$participation_rate, .data$n_count, metric = .data$success_rate) %>%
    # Upper bound confidence interval function
    andrewacct::confidence_interval() %>%
    # Join amo target df
    dplyr::left_join(amo_achievement, by = c('system', 'school', 'subgroup')) %>%
    dplyr::mutate(
      score_abs = dplyr::case_when(
        .data$metric >= a_cut ~ 4,
        .data$metric >= b_cut ~ 3,
        .data$metric >= c_cut ~ 2,
        .data$metric >= d_cut ~ 1,
        .data$metric >= 0 ~ 0
      ),
      score_target = dplyr::case_when(
        .data$metric >= .data$AMO_target_double ~ 4,
        .data$metric >= .data$AMO_target ~ 3,
        .data$ci_bound >= .data$AMO_target ~ 2,
        .data$ci_bound > .data$metric_prior ~ 1,
        .data$ci_bound <= .data$metric_prior ~ 0
      ),
      score = pmax(.data$score_abs, .data$score_target)
    ) %>%
    # Join tnready participation and set to 0 if below 95
    # left_join(tcap_participation %>% select(system:subgroup, participation_rate), by = c('system','system_name', 'school', 'school_name', 'subgroup')) %>%
    dplyr::mutate(
      score_abs = dplyr::if_else(.data$participation_rate < 95 & .data$n_count > 0 &
                                   !is.na(.data$score_abs), 0, .data$score_abs),
      score_target = dplyr::if_else(.data$participation_rate < 95 & .data$n_count > 0 & !is.na(.data$score_target), 0, .data$score_target),
      score = dplyr::if_else(.data$participation_rate < 95 & .data$n_count > 0 & !is.na(.data$score), 0, .data$score),
      participation_rate = dplyr::if_else(.data$n_count == 0, NA_real_, .data$participation_rate),
      grade = dplyr::case_when(
        .data$score == 4 ~ 'A',
        .data$score == 3 ~ 'B',
        .data$score == 2 ~ 'C',
        .data$score == 1 ~ 'D',
        .data$score == 0 ~ 'F',
        TRUE ~ NA_character_
      )
    ) %>%
    # Select desired columns
    dplyr::select(.data$system:.data$subgroup, .data$participation_rate,
                  .data$n_count:.data$grade)

  return(school_achievement)

}
