## ----setup, include = FALSE---------------------------------------------------
# The code below needs an internet connection (pre-trained model) and/or several
# minutes of model training, so it is shown but not evaluated when the vignette
# is built.
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----load---------------------------------------------------------------------
# library(SLOS)
# data(SampledData)   # loads `sampled_data`, 1000 anonymized admissions

## ----pretrained---------------------------------------------------------------
# res <- SLOS(
#   data       = sampled_data,
#   icu_column = "UnitCode",
#   outcome    = "UnitLengthStay_trunc"
# )
# 
# head(res$df_unit_slos)       # one row per ICU
# res$plot_SLOS_obs_prev       # observed vs. predicted LoS per ICU
# plot(res$funnel_plot)        # funnel plot

## ----predict------------------------------------------------------------------
# pred <- slos_predict_pretrained(sampled_data, outcome = "UnitLengthStay_trunc")
# pred$RMSE

## ----preprocess---------------------------------------------------------------
# outcome <- "UnitLengthStay_trunc"
# non_predictors <- c("UnitLengthStay_days", "admissionid", "UnitCode")
# 
# pp <- slos_pre_processing(
#   data = sampled_data, outcome = outcome,
#   corr_cutoff_num = 0.8, corr_cutoff_cat = 0.5,
#   do_impute = TRUE, include_outcome = TRUE, m = 1, maxit = 5, seed = 123
# )
# 
# pp$removed                       # what was removed and why
# train_imp <- pp$imputation$train_completed
# test_imp  <- pp$imputation$test_completed

## ----rfe----------------------------------------------------------------------
# res_fs <- slos_feature_select_rfe(
#   data    = train_imp[, setdiff(names(train_imp), non_predictors)],
#   outcome = outcome, method = "treebag", cv_folds = 5, verbose = FALSE
# )
# res_fs$selected_vars

## ----train--------------------------------------------------------------------
# keep_cols <- union(res_fs$selected_vars, outcome)
# 
# fit <- slos_train_new_model(
#   train      = train_imp[, keep_cols],
#   test       = test_imp,
#   outcome    = outcome,
#   icu_column = "UnitCode"
# )
# fit$metrics        # patient-level and ICU-level RMSE, MAE and R2

## ----custom-------------------------------------------------------------------
# res <- SLOS(test_imp, icu_column = "UnitCode", outcome = outcome, model = fit)

## ----summary------------------------------------------------------------------
# slos_values <- res$df_unit_slos$SLOS
# 
# sum(res$df_unit_slos$soma_los_obs) / sum(res$df_unit_slos$soma_los_esp)  # overall
# median(slos_values)
# quantile(slos_values, c(0.25, 0.75))

