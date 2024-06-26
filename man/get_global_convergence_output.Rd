% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_global_convergence_output.R
\name{get_global_convergence_output}
\alias{get_global_convergence_output}
\title{get_global_convergence_output}
\usage{
get_global_convergence_output(
  output_nostudy,
  indicators,
  constant_overwrite_output_c,
  tref,
  start_study_year
)
}
\arguments{
\item{tref}{}
}
\value{
globalconv_output_by_country
}
\description{
Takes in estimates for country c that have gone through constant extrapolation update. For years 5+ outside study period, estimates converge back to global se and sp estimates.
For years inside study period, these are kept as is, and for years within 4 years outside study period, linearly interpolated to global convergence estimates.
Output is the global convergence updates for country c estimates.
}
