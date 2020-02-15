#' Accountability ELPA Calculations
#'
#' This function creates Accountability dataframe for the ELPA Indicator
#'
#' @param school_elpa_path Path to the School Level ELPA file
#' @param grade_pools_path Path to the Grade Pools file
#' @param school_names_path Path to the file with School/System names
#' @param a_cut Cut score for 'A' in Absolute Pathway
#' @param b_cut Cut score for 'B' in Absolute Pathway
#' @param c_cut Cut score for 'C' in Absolute Pathway
#' @param d_cut Cut score for 'D' in Absolute Pathway
#' @param min_n_count Minimum N Count needed to receive score
#'
#' @keywords elpa, el, english learner, indicator, accountability
#'
#' @examples
#' \dontrun{
#' acct_elpa("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school.csv",
#' "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
#' "N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
#' 60, 50, 40, 25, min_n_count= 10)
#' }
#'
#' @export


acct_elpa <- function(school_elpa_path, grade_pools_path, school_names_path,
                           a_cut = 60, b_cut = 50, c_cut = 40, d_cut = 25, min_n_count = 10){

  grade_pools <- readr::read_csv(grade_pools_path) %>%
    dplyr::select(.data$system, .data$school, .data$pool, .data$designation_ineligible)

  school_df <- readr::read_csv(school_names_path)

  elpa <- readr::read_csv(school_elpa_path) %>%
    dplyr::filter(.data$school !=0, .data$system != 0, !grepl("Non-", .data$subgroup))  %>%
    dplyr::mutate(subgroup = dplyr::if_else(.data$subgroup == 'English Learners', "English Learners with Transitional 1-4", .data$subgroup)) %>%
    dplyr::transmute(
      .data$system, .data$school, indicator = 'ELPA Growth Standard',
      .data$subgroup,
      n_count = ifelse(.data$growth_standard_denom >= min_n_count, .data$growth_standard_denom, 0),
      metric = ifelse(.data$n_count > 0, .data$pct_met_growth_standard, NA_real_),
      # ci_bound = NA_real_, metric_prior = NA_real_, AMO_target = NA_real_, AMO_target_double = NA_real_,
      score_abs = NA_real_, score_target = NA_real_,
      score = dplyr::case_when(
        .data$metric >= a_cut ~ 4,
        .data$metric >= b_cut ~ 3,
        .data$metric >= c_cut ~ 2,
        .data$metric >= d_cut ~ 1,
        .data$metric < 25 ~ 0
      ),
      #score = pmax(score_abs, score_target), #score_target = NA_real_, score = NA_real_,
      grade = dplyr::case_when(
        .data$score == 4 ~ 'A',
        .data$score == 3 ~ 'B',
        .data$score == 2 ~ 'C',
        .data$score == 1 ~ 'D',
        .data$score == 0 ~ 'F',
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::left_join(grade_pools, by = c("system", "school")) %>%
    dplyr::left_join(school_df, by = c('system', 'school')) %>%
    dplyr::transmute(.data$system, .data$system_name, .data$school, .data$school_name,
                     .data$pool, .data$designation_ineligible, .data$indicator,
                     .data$subgroup, .data$n_count, .data$metric,
                     .data$score_abs, .data$score_target, .data$score, .data$grade)

  return(elpa)

}
