#' Accountability TVAAS (Growth) Calculations
#'
#' This function creates Accountability dataframe for the Growth Indicator
#'
#' @param tvaas_path_excel Path to the TVAAS file from SAS in Excel format
#' @param grade_pools_path Path to the Grade Pools file
#' @param school_names_path Path to the file with School/System names
#'
#' @keywords growth, indicator, accountability
#'
#' @examples
#' \dontrun{
#' acct_tvaas_growth("N:/ORP_accountability/data/2019_tvaas/
#' 2019-School-Level-Accountability-Results-EOC-TCAP.xlsx",
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv")
#' }
#'
#' @export


acct_tvaas_growth <- function(tvaas_path_excel, grade_pools_path, school_names_path){

  grade_pools <- readr::read_csv(grade_pools_path) %>%
    dplyr::select(.data$system, .data$school, .data$pool, .data$designation_ineligible)

  school_df <- readr::read_csv(school_names_path)

  tvaas <- readxl::read_excel(tvaas_path_excel) %>%
    janitor::clean_names() %>%
    dplyr::rename(system_name = .data$system, system = .data$system_number,
                  school = .data$school_number) %>%
    # Filter out school or system == 0
    dplyr::filter(.data$school !=0, .data$system != 0) %>%
    dplyr::mutate(system = as.integer(.data$system), school = as.integer(.data$school),
           subgroup = dplyr::case_when(
             .data$subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
             .data$subgroup == "Hispanic/Latino" ~ "Hispanic",
             .data$subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
             .data$subgroup == "Black" ~ "Black or African American",
             .data$subgroup == "English Learners (includes EL and T1-4)" ~ "English Learners with Transitional 1-4",
             .data$subgroup == "Hawaiian Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
             .data$subgroup == "Native American" ~ "American Indian or Alaska Native",
             TRUE ~ .data$subgroup
           )
    ) %>%
    dplyr::group_by(.data$system, .data$school, .data$subgroup) %>%
    # Select best score between All Grades and All Grades, No Grade 3
    dplyr::mutate(best = max(.data$index)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$index == .data$best) %>%
    dplyr::select(.data$system, .data$school, .data$subgroup, .data$number_of_students,
                  .data$index, .data$level, .data$best) %>%
    dplyr::group_by(.data$system, .data$school, .data$subgroup) %>%
    # Select best score between All Grades and All Grades, No Grade 3
    dplyr::mutate(most_students = max(.data$number_of_students)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$number_of_students == .data$most_students) %>%
    dplyr::distinct() %>%
    dplyr::left_join(grade_pools, by = c('system', 'school')) %>%
    dplyr::left_join(school_df, by = c('system', 'school'))  %>%
    # filter(!is.na(pool), !is.na(school_name)) %>%
    dplyr::transmute(.data$system, .data$system_name, .data$school, .data$school_name,
                     .data$pool, .data$designation_ineligible, indicator = "Growth", .data$subgroup,
              n_count = .data$number_of_students,
              metric = .data$level,
              # ci_bound = NA_real_, metric_prior = NA_real_, AMO_target = NA_real_, AMO_target_double = NA_real_, score_abs = NA_real_, score_target = NA_real_,
              score = dplyr::case_when(
                .data$metric == 5 ~ 4,
                .data$metric == 4 ~ 3,
                .data$metric == 3 ~ 2,
                .data$metric == 2 ~ 1,
                .data$metric == 1 ~ 0
              ),
              grade = dplyr::case_when(
                .data$score == 4 ~ 'A',
                .data$score == 3 ~ 'B',
                .data$score == 2 ~ 'C',
                .data$score == 1 ~ 'D',
                .data$score == 0 ~ 'F',
                TRUE ~ NA_character_
              )
    ) %>%
    dplyr::distinct()

  return(tvaas)

}
