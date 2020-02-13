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
#' acct_tvaas_growth("N:/ORP_accountability/data/2019_tvaas/2019-School-Level-Accountability-Results-EOC-TCAP.xlsx",
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv")
#' }
#'
#' @export


acct_tvaas_growth <- function(tvaas_path_excel, grade_pools_path, school_names_path){

  grade_pools <- readr::read_csv(grade_pools_path) %>%
    dplyr::select(system, school, pool, designation_ineligible)

  school_df <- readr::read_csv(school_names_path)

  tvaas <- readxl::read_excel(tvaas_path_excel) %>%
    janitor::clean_names() %>%
    dplyr::rename(system_name = system, system = system_number, school = school_number) %>%
    # Filter out school or system == 0
    dplyr::filter(school !=0, system != 0) %>%
    dplyr::mutate(system = as.integer(system), school = as.integer(school),
           subgroup = dplyr::case_when(
             subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
             subgroup == "Hispanic/Latino" ~ "Hispanic",
             subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
             subgroup == "Black" ~ "Black or African American",
             subgroup == "English Learners (includes EL and T1-4)" ~ "English Learners with Transitional 1-4",
             subgroup == "Hawaiian Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
             subgroup == "Native American" ~ "American Indian or Alaska Native",
             TRUE ~ subgroup
           )
    ) %>%
    dplyr::group_by(system, school, subgroup) %>%
    # Select best score between All Grades and All Grades, No Grade 3
    dplyr::mutate(best = max(index)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(index == best) %>%
    dplyr::select(system, school, subgroup, number_of_students, index, level, best) %>%
    dplyr::group_by(system, school, subgroup) %>%
    # Select best score between All Grades and All Grades, No Grade 3
    dplyr::mutate(most_students = max(number_of_students)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(number_of_students == most_students) %>%
    dplyr::distinct() %>%
    dplyr::left_join(grade_pools, by = c('system', 'school')) %>%
    dplyr::left_join(school_df, by = c('system', 'school'))  %>%
    # filter(!is.na(pool), !is.na(school_name)) %>%
    dplyr::transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator = "Growth", subgroup,
              n_count = number_of_students,
              metric = level,
              # ci_bound = NA_real_, metric_prior = NA_real_, AMO_target = NA_real_, AMO_target_double = NA_real_, score_abs = NA_real_, score_target = NA_real_,
              score = dplyr::case_when(
                metric == 5 ~ 4,
                metric == 4 ~ 3,
                metric == 3 ~ 2,
                metric == 2 ~ 1,
                metric == 1 ~ 0
              ),
              grade = dplyr::case_when(
                score == 4 ~ 'A',
                score == 3 ~ 'B',
                score == 2 ~ 'C',
                score == 1 ~ 'D',
                score == 0 ~ 'F',
                TRUE ~ NA_character_
              )
    ) %>%
    dplyr::distinct()

  return(tvaas)

}
