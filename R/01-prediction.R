# Session-level cache so the pre-trained model is downloaded at most once per
# R session (nothing is ever written to the user's home directory).
.slos_cache <- new.env(parent = emptyenv())

slos_pretrained_url <- function() {
  paste0(
    "https://github.com/igor-peres/ICU-Length-of-Stay-Prediction/",
    "releases/download/v2.1.0/SLOS_model.RData"
  )
}

#' Load the pre-trained SLOS model
#'
#' Downloads the pre-trained SLOS model from the project's GitHub release and
#' returns it. **The file is large (about 1.1 GB)**, so the first call can take a
#' long time. The file is stored in a temporary location and the loaded model
#' is cached for the rest of the R session, so repeated calls do not download
#' it again. Nothing is written outside of [tempdir()].
#'
#' An internet connection is required. If the model cannot be downloaded, an
#' informative error is raised.
#'
#' @param timeout Download time limit, in seconds. Default `3600`.
#' @param cache Logical. If `TRUE` (default), reuse the model already
#'   downloaded in this session, if any.
#'
#' @return The pre-trained model (a `caretStack` object).
#' @examples
#' \dontrun{
#' # Downloads about 1.1 GB
#' model <- slos_load_pretrained_model()
#' class(model)
#' }
#' @export
slos_load_pretrained_model <- function(cache = TRUE, timeout = 3600) {
  if (isTRUE(cache) &&
      exists("SLOS_model", envir = .slos_cache, inherits = FALSE)) {
    return(get("SLOS_model", envir = .slos_cache, inherits = FALSE))
  }

  if (!curl::has_internet()) {
    stop(
      "An internet connection is required to download the pre-trained SLOS ",
      "model. Please connect to the internet and try again, or train your ",
      "own model with slos_train_new_model().",
      call. = FALSE
    )
  }

  message(
    "Downloading the pre-trained SLOS model (about 1.1 GB). ",
    "This may take a long time, depending on your connection."
  )

  url <- slos_pretrained_url()
  temp_file <- tempfile(fileext = ".RData")
  on.exit(unlink(temp_file), add = TRUE)

  response <- tryCatch(
    httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE),
              httr::timeout(timeout)),
    error = function(e) {
      stop(
        "Could not download the pre-trained SLOS model from ", url, ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (httr::http_error(response)) {
    stop(
      "Could not download the pre-trained SLOS model from ", url,
      " (HTTP status ", httr::status_code(response), ").",
      call. = FALSE
    )
  }

  env <- new.env(parent = emptyenv())
  objs <- load(temp_file, envir = env)

  if (!"SLOS_model" %in% objs) {
    stop(
      "Downloaded RData does not contain 'SLOS_model'. Contains: ",
      paste(objs, collapse = ", "),
      call. = FALSE
    )
  }

  assign("SLOS_model", env$SLOS_model, envir = .slos_cache)
  env$SLOS_model
}

#' Predict ICU length of stay with the pre-trained SLOS model
#'
#' Makes patient-level predictions and evaluates RMSE, MAE and R2. If `model`
#' is not provided, the pre-trained model is downloaded with
#' [slos_load_pretrained_model()].
#'
#' @param data A data frame with the predictors used by the model and the
#'   outcome column.
#' @param outcome The outcome's column name, as a string. Default
#'   `"UnitLengthStay_trunc"`.
#' @param model Optional. A fitted `caretStack` model compatible with `data`.
#'   If `NULL`, the pre-trained model is used.
#'
#' @return A list with:
#' \describe{
#'   \item{predictions}{data.frame with one column, `predictions`.}
#'   \item{comparison}{data.frame with columns `Observations` and `pred`.}
#'   \item{RMSE, MAE, R2}{Numeric performance metrics.}
#' }
#' @examples
#' \dontrun{
#' # Downloads the pre-trained model (about 1.1 GB) on first use
#' res <- slos_predict_pretrained(sampled_data,
#'                                outcome = "UnitLengthStay_trunc")
#' res$RMSE
#' }
#' @export
slos_predict_pretrained <- function(data, outcome = "UnitLengthStay_trunc",
                                    model = NULL) {
  if (!(outcome %in% names(data))) {
    stop("Outcome column '", outcome, "' not found in `data`.", call. = FALSE)
  }

  if (is.null(model)) {
    model <- slos_load_pretrained_model()
  }

  preds_obj <- stats::predict(model, newdata = data)
  if (is.list(preds_obj)) {
    # data.frame / list output: use `pred` if present, else the first element
    pred_vec <- if ("pred" %in% names(preds_obj)) preds_obj[["pred"]] else preds_obj[[1]]
  } else {
    pred_vec <- preds_obj
  }

  Observations <- data.frame(Observations = data[[outcome]])
  Predictions  <- data.frame(pred = as.numeric(pred_vec))

  RMSE_value <- MLmetrics::RMSE(Predictions$pred, Observations$Observations)
  MAE_value  <- MLmetrics::MAE(Predictions$pred, Observations$Observations)
  R2_value   <- MLmetrics::R2_Score(Predictions$pred, Observations$Observations)
  comparison <- cbind(Observations, Predictions)

  list(
    predictions = data.frame(predictions = as.numeric(Predictions$pred)),
    comparison  = data.frame(comparison),
    RMSE = RMSE_value, MAE = MAE_value, R2 = R2_value
  )
}

# Internal: predict with a model returned by slos_train_new_model().
#' @noRd
slos_predict_custom <- function(data, model, outcome = "UnitLengthStay_trunc") {

  if (!(outcome %in% names(data))) {
    stop("Outcome column '", outcome, "' not found in data.", call. = FALSE)
  }

  # separate
  y <- data[[outcome]]
  X <- data[, setdiff(names(data), outcome), drop = FALSE]

  # char -> factor
  chr_cols <- names(X)[vapply(X, is.character, logical(1))]
  if (length(chr_cols)) X[chr_cols] <- lapply(X[chr_cols], factor)

  # align factor levels with the ones seen in training
  for (col in names(X)) {
    if (is.factor(X[[col]]) && col %in% names(model$dummyVars$lvls)) {
      X[[col]] <- factor(X[[col]], levels = model$dummyVars$lvls[[col]])
    }
  }

  # dummy encoding
  X_num <- as.data.frame(stats::predict(model$dummyVars, newdata = X))

  # ensure same columns
  missing_cols <- setdiff(model$feature_names, names(X_num))
  if (length(missing_cols)) {
    for (mc in missing_cols) X_num[[mc]] <- 0
  }

  X_num <- X_num[, model$feature_names, drop = FALSE]

  # handle NA
  X_num[is.na(X_num)] <- 0

  # predict
  pred_raw <- stats::predict(model$stacked_model, newdata = X_num)

  if (is.data.frame(pred_raw)) {
    pred <- as.numeric(pred_raw[[1]])
  } else if (is.list(pred_raw)) {
    pred <- as.numeric(unlist(pred_raw))
  } else {
    pred <- as.numeric(pred_raw)
  }

  Observations <- data.frame(Observations = y)
  Predictions  <- data.frame(pred = pred)

  RMSE_value <- MLmetrics::RMSE(Predictions$pred, Observations$Observations)
  MAE_value  <- MLmetrics::MAE(Predictions$pred, Observations$Observations)
  R2_value   <- MLmetrics::R2_Score(Predictions$pred, Observations$Observations)

  list(
    predictions = data.frame(predictions = pred),
    comparison  = cbind(Observations, Predictions),
    RMSE = RMSE_value, MAE = MAE_value, R2 = R2_value
  )
}
