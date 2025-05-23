% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ToArray.R
\name{ToArray}
\alias{ToArray}
\title{ToArray}
\usage{
ToArray(studydat)
}
\arguments{
\item{indices.b}{}
}
\value{
output list named array_data that holds: combis.nzs, Z.n, B.n, iso_alpha_3_code.n
}
\description{
Take in list of study data, ie truematvr's and any breakdown information, and for each study element outputs array of unique combinations of boxes
that sum to meet aggregated sums using get_unique_combi function. Each study output (study array of combis) is saved into larger array
where each index n is unique array of combis for study info n. Z.n is vector of lengths of arrays, ie array n has length z.n[n].
Lastly, iso_alpha_3_code.n saved vector of iso_alpha_3_codes from each study.
}
\examples{


}
