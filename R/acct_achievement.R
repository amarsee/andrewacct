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
    dplyr::select(system, school, pool, designation_ineligible)

  student_level <- student_level_df

  sl <- student_level %>%
    dplyr::filter(!(system == 964 & school == 964 | system == 970 & school == 970)) %>%
    dplyr::mutate_at("residential_facility", ~ dplyr::if_else(is.na(.), 0, .)) %>%
    dplyr::mutate_at("enrolled_50_pct_school", ~ dplyr::if_else(is.na(.), "Y", .)) %>%
    dplyr::mutate(
      original_subject = dplyr::case_when(
        grade < 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II",
                                            "Integrated Math III", 'English I', 'English II', 'Biology I', 'Chemistry') ~ subject,
        TRUE ~ original_subject
      )
    )

  school_df <- readr::read_csv(school_names_path)

  integrated_math <- student_level %>%
    dplyr::filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    dplyr::count(system, original_subject) %>%
    dplyr::group_by(system) %>%
    dplyr::mutate(temp = max(n)) %>%
    # Systems where Integrated Math is the max between that and Algebra I
    dplyr::filter(n == temp, original_subject == "Integrated Math I")
  # Vector with the sytems where that is the case
  int_math_vec <- integrated_math[['system']]

  # Previous AMO data
  amo_achievement <- readr::read_csv(success_amo_path) %>%
    dplyr::transmute(system, school, subgroup, metric_prior = success_rate_prior, AMO_target, AMO_target_double)

  # ACT score substitution
  act_sub <- readr::read_csv(act_substitution_path) %>%
    #left_join(school_df, by = c('system', 'school')) %>%
    dplyr::transmute(system, system_name, school, school_name,
              subject = dplyr::case_when(
                subject == "ACT Math" & system %in% int_math_vec ~ "Integrated Math III",
                TRUE  ~ "Algebra II"
              ), grade = 11, subgroup = "All Students", valid_test = valid_tests, on_track = n_met_benchmark, mastered = 0
    )

  # ========================================== Prep for Achievement =======================================
  sl <- sl %>%
    dplyr::mutate(
      on_track = dplyr::case_when(
        performance_level == "Proficient" | performance_level == "On Track" ~ 1,
        TRUE                      ~ 0
      ),
      mastered = dplyr::case_when(
        performance_level == "Mastered" | performance_level == "Advanced" ~ 1,
        TRUE                      ~ 0
      )
    ) %>%
    dplyr::filter(residential_facility == 0, (enrolled_50_pct_school == 'Y' | (acct_system != system | school != acct_school)),  # homebound == 0, !is.na(state_student_id),grade %in% 3:12,
           original_subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>%

    # fill(system_name) %>%
    dplyr::rename(subgroup = reported_race)

  state_totals <- dplyr::bind_rows(sl %>% bind_rows(act_sub %>% dplyr::rename(acct_system = system, acct_school = school)) %>%
                                     dplyr::mutate(subgroup = "All Students"),
                            sl %>%
                              dplyr::filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>%
                              dplyr::mutate(subgroup = "Super Subgroup"),
                            sl, # Race Subgroups
                            sl %>% dplyr::filter(bhn_group > 0) %>% dplyr::mutate(subgroup = "Black/Hispanic/Native American"),
                            sl %>% dplyr::filter(economically_disadvantaged > 0) %>% dplyr::mutate(subgroup = "Economically Disadvantaged"),
                            sl %>% dplyr::filter(t1234 > 0 | el > 0) %>% dplyr::mutate(subgroup = "English Learners with Transitional 1-4"),
                            sl %>% dplyr::filter(special_ed > 0) %>% dplyr::mutate(subgroup = "Students with Disabilities")) %>%
    dplyr::filter(subgroup != "Unknown") %>%
    dplyr::arrange(system, school, subject, subgroup) %>%
    # rename(subject = original_subject) %>%
    dplyr::mutate(
      subgroup = dplyr::case_when(
        subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
        subgroup == "Hispanic/Latino" ~ "Hispanic",
        subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
        TRUE ~ subgroup
      )
    ) %>%
    dplyr::group_by(acct_system, acct_school, subject, subgroup) %>%
    dplyr::summarise(
      enrolled = sum(enrolled, na.rm = TRUE),
      tested = sum(tested, na.rm = TRUE),
      valid_tests = sum( valid_test),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(system = acct_system, school = acct_school)

  # ====================== TCAP Participation Rate =======================
  school_assessment <- school_assessment_df

  tcap_participation <- school_assessment %>%
    dplyr::filter(year == 2019, grade == 'All Grades', subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>%
    dplyr::group_by(system, system_name, school, school_name, subgroup) %>%
    dplyr::summarise(
      n_enrolled = sum(enrolled),
      n_tested = sum(tested)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      participation_rate = round(n_tested/n_enrolled * 100 + 1e-5, 0)
    )

  # saveRDS(tcap_participation, "data/tcap_participation_2018.rds")

  # ============================= Achievement =========================

  school_achievement <- state_totals %>%
    dplyr::mutate(
      subject = case_when(
        subject %in% english_eoc ~ "HS English",
        subject %in% c(math_eoc, "HS Math") ~ "HS Math",
        TRUE ~ subject
      )
    ) %>%
    dplyr::group_by(system, school, subject, subgroup)  %>%
    dplyr::summarise(
      enrolled = sum(enrolled, na.rm = TRUE),
      tested = sum(tested, na.rm = TRUE),
      valid_tests = sum(valid_tests, na.rm = TRUE),
      n_on_track = sum(n_on_track, na.rm = TRUE),
      n_mastered = sum(n_mastered, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      valid_tests = dplyr::if_else(valid_tests >= min_n_count, valid_tests, 0),
      n_on_track = dplyr::if_else(valid_tests >= min_n_count, n_on_track, 0),
      n_mastered = dplyr::if_else(valid_tests >= min_n_count, n_mastered, 0)
    ) %>%
    dplyr::group_by(system, school, subgroup)  %>%
    dplyr::summarise(
      participation_rate = round(100 * sum(tested)/sum(enrolled)),
      n_count = sum(valid_tests),
      success_rate = round(((sum(n_on_track) + sum(n_mastered))/sum(valid_tests)) * 100 + 1e-10, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(success_rate = ifelse(is.na(success_rate), NA, success_rate)) %>%
    dplyr::left_join(grade_pools, by = c('system', 'school')) %>%
    dplyr::left_join(school_df, by = c('system', 'school')) %>%
    # Filtered out if No pool or school name
    # filter(!is.na(pool), !is.na(school_name)) %>%
    dplyr::transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator = 'Achievement',
              subgroup,participation_rate, n_count, metric = success_rate) %>%
    # Upper bound confidence interval function
    andrewacct::confidence_interval() %>%
    # Join amo target df
    dplyr::left_join(amo_achievement, by = c('system', 'school', 'subgroup')) %>%
    dplyr::mutate(
      score_abs = dplyr::case_when(
        metric >= a_cut ~ 4,
        metric >= b_cut ~ 3,
        metric >= c_cut ~ 2,
        metric >= d_cut ~ 1,
        metric >= 0 ~ 0
      ),
      score_target = dplyr::case_when(
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
    dplyr::mutate(
      score_abs = dplyr::if_else(participation_rate < 95 & n_count > 0 & !is.na(score_abs), 0, score_abs),
      score_target = dplyr::if_else(participation_rate < 95 & n_count > 0 & !is.na(score_target), 0, score_target),
      score = dplyr::if_else(participation_rate < 95 & n_count > 0 & !is.na(score), 0, score),
      participation_rate = dplyr::if_else(n_count == 0, NA_real_, participation_rate),
      grade = dplyr::case_when(
        score == 4 ~ 'A',
        score == 3 ~ 'B',
        score == 2 ~ 'C',
        score == 1 ~ 'D',
        score == 0 ~ 'F',
        TRUE ~ NA_character_
      )
    ) %>%
    # Select desired columns
    dplyr::select(system:subgroup, participation_rate, n_count:grade)

  return(school_achievement)

}
