% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acct_absenteeism.R
\name{acct_absenteeism}
\alias{acct_absenteeism}
\title{Accountability Chronic Absenteeism Calculations}
\usage{
acct_absenteeism(
  absenteeism_student_level_path,
  grade_pools_path,
  school_names_path,
  absenteeism_amo_path,
  a_cut_k8 = 6,
  b_cut_k8 = 9,
  c_cut_k8 = 13,
  d_cut_k8 = 20,
  a_cut_hs = 10,
  b_cut_hs = 14,
  c_cut_hs = 20,
  d_cut_hs = 30,
  min_n_count = 30
)
}
\arguments{
\item{absenteeism_student_level_path}{Path to the Student Level Chronic Absenteeism file}

\item{grade_pools_path}{Path to the Grade Pools file}

\item{school_names_path}{Path to the file with School/System names}

\item{absenteeism_amo_path}{Path to the AMO file for the Chronic Absenteeism Indicator}

\item{a_cut_k8}{Cut score for 'A' in Absolute Pathway, K8 pool}

\item{b_cut_k8}{Cut score for 'B' in Absolute Pathway, K8 pool}

\item{c_cut_k8}{Cut score for 'C' in Absolute Pathway, K8 pool}

\item{d_cut_k8}{Cut score for 'D' in Absolute Pathway, K8 pool}

\item{a_cut_hs}{Cut score for 'A' in Absolute Pathway, HS pool}

\item{b_cut_hs}{Cut score for 'B' in Absolute Pathway, HS pool}

\item{c_cut_hs}{Cut score for 'C' in Absolute Pathway, HS pool}

\item{d_cut_hs}{Cut score for 'D' in Absolute Pathway, HS pool}

\item{min_n_count}{Minimum N Count needed to receive score}
}
\description{
This function creates Accountability dataframe for the Chronic Absenteeism Indicator
with 2020 cuts by default
}
\examples{
\dontrun{
acct_absenteeism("N:/ORP_accountability/data/2019_chronic_absenteeism/
school_chronic_absenteeism_Jul11.csv",
"N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv",
"N:/ORP_accountability/data/2019_final_accountability_files/names.csv",
"N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school_primary_enrollment.csv",
a_cut_k8 = 6, b_cut_k8 = 9, c_cut_k8 = 13, d_cut_k8 = 20,
a_cut_hs = 10, b_cut_hs = 14, c_cut_hs = 20, d_cut_hs = 30,
min_n_count = 30)
}

}
\keyword{absenteeism,}
\keyword{accountability}
\keyword{indicator,}
