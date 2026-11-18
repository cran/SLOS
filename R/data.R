#' Sampled ICU admissions data
#'
#' An anonymized sample of 1000 ICU admissions used to illustrate and test the
#' package. It contains the predictors expected by the pre-trained SLOS model,
#' the ICU identifier (`UnitCode`) and the observed, truncated ICU length of
#' stay (`UnitLengthStay_trunc`, in days).
#'
#' @docType data
#' @name sampled_data
#' @aliases SampledData
#' @usage data(SampledData)
#' @format A data frame with 1000 rows (one per admission). Among its columns:
#' \describe{
#'   \item{UnitCode}{ICU identifier.}
#'   \item{UnitLengthStay_trunc}{Observed ICU length of stay, in days,
#'     truncated (outcome).}
#'   \item{...}{Admission-level predictors used by the model.}
#' }
#' @source Anonymized sample prepared by the package authors.
#' @keywords datasets
NULL
