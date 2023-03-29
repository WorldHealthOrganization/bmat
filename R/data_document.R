

#' vrdata
#' @format data.frame	2869obs.of36variables
#'\describe{
#'   \item{iso_alpha_3_code}{\emph{\sQuote{Character}} Three letter country code.}
#'   \item{year_mid}{\emph{\sQuote{Numeric}} Year at the middle of the study period}
#'   \item{year_start}{\emph{\sQuote{Numeric}} Year at start of study period.}
#'   \item{year_end}{\emph{\sQuote{Numeric}} Year at end of study period.}
#'   \item{env_mat}{\emph{\sQuote{Numeric}} Maternal deaths, timeframe: Maternal deaths < 42days. Accounts for PAHO and country consultation updates.}
#'   \item{env_mat_late}{\emph{\sQuote{Numeric}} Late maternal deaths, timeframe: 42days =< Maternal deaths < 1 year.}
#'   \item{env_mat_incl_late}{\emph{\sQuote{Numeric}} Maternal deaths + late maternal deaths.}
#'   \item{env_mat_prior_to_consultation}{\emph{\sQuote{Numeric}} Maternal deaths. The original from WHO database before country consultation update.}
#'   \item{env_total_vr_reported}{\emph{\sQuote{Numeric}} CRVS reported envelope of female deaths 15:49. Accounts for PAHO and country consultation updates.}
#'   \item{env_total_who_estimated}{\emph{\sQuote{Numeric}} CRVS reported envelope of female deaths 15:49. The original from WHO database before country consultation update.}
#'   \item{ill_defined_death_proportion}{\emph{\sQuote{Numeric}} Proportion deaths classified with “ill-defined” cause of death.}
#'   \item{ill_defined_death_proportion2}{\emph{\sQuote{Numeric}} Proportion deaths. Specific cause of death. Needs more documentation.}
#'   \item{include}{\emph{\sQuote{Logical}} FALSE if observation is to be excluded from BMAT model run.}
#'   \item{exclude_reason}{\emph{\sQuote{Logical}} Reason for exclusion if not included.}
#'   \item{icd_used}{\emph{\sQuote{Logical}} TRUE if ICD-10 used to define maternal death.}
#'   \item{icd}{\emph{\sQuote{Character}} ICD-10 code}
#'   \item{env_who}{\emph{\sQuote{Numeric}} WHO reported envelope of female deaths 15:49}
#'   \item{env_vr_who_onecountry} Floored env_who for one country run}
#'   \item{env_vr_who_global} WHO reported envelope of female deaths 15:49 adjusted for global run}
#'   \item{rho_bmis}{\emph{\sQuote{Numeric}} completeness of envelope for bmis}
#'   \item{rho_bmat}{\emph{\sQuote{Numeric}} completeness of envelope for bmat}
#'   \item{updateinmat}
#'   \item{updateinenv}
#'   \item{rho_period}{\emph{\sQuote{Numeric}}}
#'   \item{rho_period_max}{\emph{\sQuote{Numeric}}}
#'   \item{rho_period_max}{\emph{\sQuote{Numeric}}}
#'   \item{ytot_bmis} {\emph{\sQuote{Numeric}} env_total_vr_reported / completeness}
#'   \item{ytot_bmat} {\emph{\sQuote{Numeric}}}
#'   \item{usability_percentage}{\emph{\sQuote{Numeric}}}
#'   \item{include_reason}{\emph{\sQuote{Character}}}
#'}
"vrdata"


#' ssdata
#' @format data.frame 387obs.of 21variables
#' \describe{
#'   \item{iso_alpha_3_code}{\emph{\sQuote{Character}} Three letter country code}
#'   \item{year_start}{\emph{\sQuote{Numeric}} Year at start of study period.}
#'   \item{year_end}{\emph{\sQuote{Numeric}} Year at end of study period.}
#'   \item{check}{\emph{\sQuote{Character}}  "Y" if study checks all female deaths for misclassification.}
#'   \item{check_2}{\emph{\sQuote{Character}} Updated check variable based on reported data. We use this one to determine if study goes outside VR to check deaths.}
#'   \item{env_total_study_reported}{\emph{\sQuote{Numeric}} Study reported true total of female deaths.}
#'   \item{truemat_vr}{\emph{\sQuote{Numeric}} Study reported true total number of maternal deaths inside VR, ie TP+FN}
#'   \item{truemat}{\emph{\sQuote{Numeric}} Study reported true total number of maternal deaths, ie TP + FN + UP}
#'   \item{fn}{\emph{\sQuote{Numeric}} study reported number false negative deaths}
#'   \item{fp}{\emph{\sQuote{Numeric}} study reported number false positive deaths}
#'   \item{tp}{\emph{\sQuote{Numeric}} study reported number true positive deaths}
#'   \item{tn}{\emph{\sQuote{Numeric}} study reported number true negative deaths}
#'   \item{up}{\emph{\sQuote{Numeric}} study reported unregistered maternal deaths}
#'   \item{un}{\emph{\sQuote{Numeric}} study reported unregistered non maternal deaths}
#'   \item{include_bmis}{\emph{\sQuote{Logical}} TRUE if include in bmis fitting}
#'   \item{include_bmis_global}{\emph{\sQuote{Logical}} TRUE if include in bmis global fitting, used instead of variable include_bmis}
#'   \item{include_bmat}{\emph{\sQuote{Logical}} TRUE if include in bmat fitting}
#'   \item{missing_envelope_bmatrun}{\emph{\sQuote{Logical}}}
#'   \item{short_cite}{\emph{\sQuote{Numeric}} Source description}
#'}
"ssdata"
