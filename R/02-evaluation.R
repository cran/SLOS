#' Standardized length of stay (SLOS) per ICU
#'
#' Core function of the package. It predicts the length of stay of every
#' patient, aggregates observed and expected length of stay by ICU, computes
#' each unit's SLOS (observed / expected), draws a funnel plot and builds a
#' plot comparing the sum of observed and predicted length of stay per unit.
#'
#' @param data A data frame containing the test data: the model predictors,
#'   the outcome and the ICU identifier.
#' @param icu_column Name of the column that identifies the ICU, as a string.
#' @param outcome Name of the outcome column (observed ICU length of stay), as
#'   a string.
#' @param model Optional. A model returned by [slos_train_new_model()]. If
#'   `NULL` (default), the pre-trained model is downloaded with
#'   [slos_load_pretrained_model()] (requires an internet connection and a
#'   download of about 1.1 GB).
#' @param show_funnel Logical. If `TRUE` (default), the funnel plot is drawn
#'   on the active graphics device. It can always be drawn again with
#'   `plot(result$funnel_plot)`.
#'
#' @return A list with:
#' \describe{
#'   \item{df_unit_slos}{Data frame with one row per ICU: number of admissions
#'     (`admissoes`), sum of observed length of stay (`soma_los_obs`), sum of
#'     expected length of stay (`soma_los_esp`) and `SLOS`.}
#'   \item{plot_SLOS_obs_prev}{A `ggplot` object comparing the sum of observed
#'     and predicted length of stay per unit.}
#'   \item{funnel_plot}{The funnel plot object created by `ems::funnel()`.}
#' }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("e1071", quietly = TRUE)) {
#'   # Train a small model so that the example runs quickly
#'   dat <- sampled_data[, setdiff(names(sampled_data),
#'                                 c("UnitLengthStay_days", "admissionid"))]
#'   idx <- caret::createDataPartition(dat$UnitLengthStay_trunc,
#'                                     p = 0.8, list = FALSE)
#'   fit <- slos_train_new_model(
#'     dat[idx, ], outcome = "UnitLengthStay_trunc", icu_column = "UnitCode",
#'     cv_folds = 2, verbose_iter = FALSE,
#'     base_ranger_grid = data.frame(.mtry = 5, .splitrule = "variance",
#'                                   .min.node.size = 5),
#'     meta_grid = expand.grid(mtry = 2, min.node.size = 5,
#'                             splitrule = "variance")
#'   )
#'
#'   result <- SLOS(dat[-idx, ], icu_column = "UnitCode",
#'                  outcome = "UnitLengthStay_trunc", model = fit,
#'                  show_funnel = FALSE)
#'   head(result$df_unit_slos)
#'   result$plot_SLOS_obs_prev
#'   plot(result$funnel_plot)
#' }
#' }
#'
#' \dontrun{
#' # With the pre-trained model (downloads about 1.1 GB on first use)
#' result <- SLOS(sampled_data, icu_column = "UnitCode",
#'                outcome = "UnitLengthStay_trunc")
#' }
#' @export
SLOS <- function(data, icu_column, outcome, model = NULL,
                 show_funnel = TRUE) {
  # input checks
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("Input data must contain at least one row.", call. = FALSE)
  }

  required_columns <- c(icu_column, outcome)
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop("Missing required columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  # predictions
  if (is.null(model)) {
    eval_results <- slos_predict_pretrained(data, outcome = outcome)
  } else {
    eval_results <- slos_predict_custom(data, model, outcome = outcome)
  }
  observations <- eval_results$comparison$Observations
  predictions <- eval_results$predictions$predictions

  df_model_pred <- data.frame(observations = observations,
                              predictions = predictions) %>%
    dplyr::mutate(predictions = dplyr::case_when(
      .data$predictions < 0 ~ 0,
      .data$predictions > 21 ~ 21,
      TRUE ~ .data$predictions
    ))

  df_unit_slos <- df_model_pred %>%
    dplyr::bind_cols(dplyr::select(data, dplyr::all_of(icu_column))) %>%
    dplyr::group_by(.data[[icu_column]]) %>%
    dplyr::summarise(
      admissoes = dplyr::n(),
      soma_los_obs = sum(.data$observations),
      soma_los_esp = sum(.data$predictions),
      .groups = "drop"
    ) %>%
    dplyr::mutate(SLOS = .data$soma_los_obs / .data$soma_los_esp) %>%
    stats::na.omit()

  plot_SLOS_obs_prev <- ggplot2::ggplot(df_unit_slos) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$soma_los_esp, y = .data$soma_los_obs),
      color = "gray40"
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = .data$soma_los_esp, y = .data$soma_los_obs),
      method = "loess", formula = y ~ x
    ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::labs(x = "Sum of predicted ICU LoS",
                  y = "Sum of observed ICU LoS",
                  title = "Grouped LoS per Unit (days)") +
    ggplot2::theme_bw()

  # Funnel plot via ems::funnel
  funnel_plot <- ems::funnel(
    unit = as.factor(df_unit_slos[[icu_column]]),
    y = df_unit_slos$SLOS,
    y.type = "SRU",
    o = df_unit_slos$soma_los_obs,
    e = df_unit_slos$soma_los_esp,
    theta = sum(df_unit_slos$soma_los_obs) / sum(df_unit_slos$soma_los_esp),
    n = df_unit_slos$admissoes,
    method = "normal",
    option = "rate",
    plot = FALSE,
    direct = TRUE
  )
  if (isTRUE(show_funnel)) {
    graphics::plot(funnel_plot)
  }

  list(
    df_unit_slos = df_unit_slos,
    plot_SLOS_obs_prev = plot_SLOS_obs_prev,
    funnel_plot = funnel_plot
  )
}
