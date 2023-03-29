#' document_data
#'
#' automatically generates roxygen skeleton for data use base r's "str" function
#'
#' @param df
#'
#' @export
document_data <- function (df) {
  title <- substitute(df)
  output <- c(paste("#'", title),
              "#' @format data.frame",
              gsub(pattern ="^", replacement = "#'", capture.output(str(df))) %>%
                gsub(pattern = "[$]", replacement = "/item{") %>%
                gsub(pattern = "[:]", replacement = "}") %>%
                gsub(pattern = "\\snum\\s", replacement = "{/emph{/sQuote{Numeric}}}", ignore.case = FALSE) %>%
                gsub(pattern = "\\slogi\\s", replacement = "{/emph{/sQuote{Logical}}}", ignore.case = FALSE) %>%
                gsub(pattern = "\\schr\\s", replacement = "{/emph{/sQuote{Character}}}", ignore.case = FALSE) %>%
                gsub(pattern = "[/]", replacement = "\\\\") %>%
                gsub(pattern = "[ ]", replacement = ""),
              dQuote(title))
  cat(output, sep="\n")
}


#' main_data
#'
#' contains data that is central to BMat
#'
#' @format data.frame':	3675obs.of33variables
#' \describe{
#'\item{iso}{\emph{\sQuote{Character}} ...... }
#'\item{year}{\emph{\sQuote{Numeric}} ...... }
#'\item{start}{\emph{\sQuote{Numeric}} ...... }
#'\item{end}{\emph{\sQuote{Numeric}} ...... }
#'\item{obs_matdeaths}{\emph{\sQuote{Numeric}} ...... }
#'\item{final_pm}{\emph{\sQuote{Numeric}} ...... }
#'\item{final_env}{\emph{\sQuote{Numeric}} ...... }
#'\item{citation_short}{\emph{\sQuote{Character}} ...... }
#'\item{modelinclude}{\emph{\sQuote{Logical}} ...... }
#'\item{modelinclude_reason}{\emph{\sQuote{Character}} ...... }
#'\item{ss_toedit}{\emph{\sQuote{Character}} ...... }
#'\item{completeness_inq}{\emph{\sQuote{Numeric}} ...... }
#'\item{obs_selogpm}{\emph{\sQuote{Numeric}} ...... }
#'\item{type}{\emph{\sQuote{Character}} ...... }
#'\item{definition}{\emph{\sQuote{Character}} ...... }
#'\item{rhovrfinal_bmat}{\emph{\sQuote{Numeric}} ...... }
#'\item{usa}{\emph{\sQuote{Numeric}} ...... }
#'\item{obs_env}{\emph{\sQuote{Numeric}} ...... }
#'\item{sens}{\emph{\sQuote{Numeric}} ...... }
#'\item{var_sens}{\emph{\sQuote{Numeric}} ...... }
#'\item{sens_sq}{\emph{\sQuote{Numeric}} ...... }
#'\item{oneminsens_sq}{\emph{\sQuote{Numeric}} ...... }
#'\item{spec}{\emph{\sQuote{Numeric}} ...... }
#'\item{var_spec}{\emph{\sQuote{Numeric}} ...... }
#'\item{spec_sq}{\emph{\sQuote{Numeric}} ...... }
#'\item{oneminspec_sq}{\emph{\sQuote{Numeric}} ...... }
#'\item{rho_sesp}{\emph{\sQuote{Numeric}} ...... }
#'\item{cov_sesp}{\emph{\sQuote{Numeric}} ...... }
#'\item{hasstudies}{\emph{\sQuote{Logical}} ...... }
#'\item{mmr_observed}{\emph{\sQuote{Numeric}} ...... }
#'\item{final_mmr}{\emph{\sQuote{Numeric}} ...... }
#'\item{final_pm_before_crisis}{\emph{\sQuote{Numeric}} ...... }
#'\item{final_mmr_before_crisis}{\emph{\sQuote{Numeric}} ...... }
#'}
#'
#'@references
#'@source
#'
"main_data"


#' bmat_mmr_studies
#' @format data.frame	387obs.of41variables
#'\item{DMname}{\emph{\sQuote{Character}}}
#'\item{obs_id}{\emph{\sQuote{Character}}}
#'\item{iso}{\emph{\sQuote{Character}}}
#'\item{startdate}{\emph{\sQuote{Numeric}}
#'\item{enddate}{\emph{\sQuote{Numeric}}}
#'\item{checkall}{\emph{\sQuote{Character}}}
#'\item{checkall2}{\emph{\sQuote{Character}}}
#'\item{env_total_study_reported}{\emph{\sQuote{Numeric}}}
#'\item{truemat.vr}{\emph{\sQuote{Numeric}}}
#'\item{truemat.tot}{\emph{\sQuote{Numeric}}}
#'\item{rhovr.t}{\emph{\sQuote{Numeric}}}
#'\item{vr.yrs.t}{\emph{\sQuote{Numeric}}
#'\item{y.tot.t}{\emph{\sQuote{Numeric}}}
#'\item{y.vr.t}{\emph{\sQuote{Numeric}}}
#'\item{mat.vr.t}{\emph{\sQuote{Numeric}}}
#'\item{nonmat.vr.t}{\emph{\sQuote{Numeric}}}
#'\item{isicd10.t}{\emph{\sQuote{Logical}}}
#'\item{FN}{\emph{\sQuote{Numeric}}}
#'\item{FP}{\emph{\sQuote{Numeric}}}
#'\item{TP}{\emph{\sQuote{Numeric}}}
#'\item{TN}{\emph{\sQuote{Numeric}}}
#'\item{UP}{\emph{\sQuote{Numeric}}}
#'\item{UN}{\emph{\sQuote{Numeric}}}
#'\item{citation_short}{\emph{\sQuote{Numeric}}}
#'\item{include_vradj}{\emph{\sQuote{Logical}}}
#'\item{include_bmat}{\emph{\sQuote{Logical}}}
#'\item{incompVR}{\emph{\sQuote{Logical}}}
#'\item{missing_envelope_bmatrun}{\emph{\sQuote{Logical}}}
#'\item{incomVR_multiplier}{\emph{\sQuote{Logical}}}
#'\item{ztot}{\emph{\sQuote{Numeric}}}
#'\item{zvr}{\emph{\sQuote{Numeric}}}
#'\item{zmatvr}{\emph{\sQuote{Numeric}}}
#'\item{znonmatvr}{\emph{\sQuote{Numeric}}}
#'\item{refyear}{\emph{\sQuote{Numeric}}}
#'\item{rhovr_ref}{\emph{\sQuote{Numeric}}}
#'\item{yvr_ref}{\emph{\sQuote{Numeric}}}
#'\item{ytot_ref}{\emph{\sQuote{Numeric}}}
#'\item{matvr_ref}{\emph{\sQuote{Numeric}}}
#'\item{nonmatvr_ref}{\emph{\sQuote{Numeric}}}
#'\item{short_citation_final}{\emph{\sQuote{Character}}}
#'\item{full_citation_final}{\emph{\sQuote{Character}}}
"bmat_mmr_studies"
bmat_mmr_studies <- function(){
}
# bmat_mmr_studies <- readRDS(here::here("data-raw/observations/fromvrmodel/df_BMATMMR_studies.rds"))
# document_data(bmat_mmr_studies)

#' mics_mmr
#'
#' A dataset containing mmr data from MICS surveys
#'
#' @format A data frame with 49 variables:
#' \describe{
#'   \item{New data as of June/July 2019=1}{\emph{\sQuote{Numeric}} explain the variable here. }
#'   \item{AB 6 March}{\emph{\sQuote{Character}} ...... }
#'   \item{who 5 march 2019}{\emph{\sQuote{Character}} ....... }
#'   \item{response la}{\emph{\sQuote{Character}} .......... }
#'   \item{5 march response2 to LA }{\emph{\sQuote{Character}} ................}
#'   \item{AB comments}{\emph{\sQuote{Character}}................ }
#'   \item{Data before 2000 or mid-year not 2000: 1= yes}{\emph{\sQuote{Numeric}} ............   }
#'   \item{15-19 years; yes=1, no=2 (>2000)}{\emph{\sQuote{Character}} ...... }
#' }
#'
#'
#' @references
#'
#' @source
#'
#'
"mics_mmr"
mics_mmr <- function(){
}

# Use this to read in the data and inspect
# df <- readxl::read_xlsx(here::here("data-raw/observations/mics_MMR database_23 Aug 2019.xlsx"))
# str(df)
# ncol(df)



#' dhs_mmr
#' @format data.frame 155obs.of14variables}
#'\item{iso.code}{\emph{\sQuote{Character}}}
#'\item{country.name}{\emph{\sQuote{Character}}}
#'\item{survey.year}{\emph{\sQuote{Character}}}
#'\item{start.date}{\emph{\sQuote{Numeric}}}
#'\item{end.date}{\emph{\sQuote{Numeric}}}
#'\item{data.type}{\emph{\sQuote{Character}}}
#'\item{source.detail}{\emph{\sQuote{Character}}}
#'\item{definition}{\emph{\sQuote{Character}}}
#'\item{pmdf.input.file}{\emph{\sQuote{Character}}}
#'\item{pmdf}{\emph{\sQuote{Numeric}}}
#'\item{total.deaths.obs}{\emph{\sQuote{Logical}}}
#'\item{se.pmdf.calc}{\emph{\sQuote{Numeric}}}
#'\item{log.pmdf.calc}{\emph{\sQuote{Numeric}}}
#'\item{se.log.pmdf.calc}{\emph{\sQuote{Numeric}}}
"dhs_mmr"
dhs_mmr <- function(){
}

# Use this to read in the data and inspect
# df <- read.csv(here::here("data-raw/observations/DHSinput_April2019.csv"))
# str(df)
# ncol(df)
# document_data(df)
