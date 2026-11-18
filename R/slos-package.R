#' SLOS: ICU Length of Stay Prediction and Efficiency Evaluation
#'
#' Tools for predicting ICU length of stay and for assessing ICU efficiency
#' with the standardized length of stay (SLOS) ratio.
#'
#' @section Main functions:
#' \itemize{
#'   \item [SLOS()]: unit-level SLOS, observed vs. predicted plot and funnel plot.
#'   \item [slos_predict_pretrained()]: patient-level predictions with the
#'     pre-trained model.
#'   \item [slos_pre_processing()]: feature screening, train/test split and
#'     multiple imputation.
#'   \item [slos_feature_select_rfe()], [slos_apply_feature_selection()]:
#'     recursive feature elimination.
#'   \item [slos_train_new_model()]: train a new stacked model.
#' }
#'
#' @references
#' Peres et al. (2022) \doi{10.1016/j.accpm.2022.101142}.
#'
#' Peres et al. (2023) \doi{10.1007/s00134-023-07255-w}.
#'
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats na.omit na.pass predict cor sd
#' @importFrom utils head
#' @importFrom caretEnsemble caretList caretStack caretModelSpec
#' @importFrom ranger ranger
"_PACKAGE"
