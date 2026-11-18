library(SLOS)
set.seed(42)

# Your own data (adjust to your real file/object names)
d <- read.csv("UMCdb_final.csv")

outcome <- "UnitLengthStay_trunc"

non_predictors <- c(
  "UnitLengthStay_days",   # almost the outcome itself (leakage!)
  "admissionid",           # identifier, not a predictor
  "icuid"                  # ICU identifier (only used in the final SLOS step)
)

# PRE-PROCESSING:
#' \itemize{
#'   \item Near-zero variance detection via \code{caret::nearZeroVar} (records \emph{freqRatio} and \emph{percentUnique}).
#'   \item High pairwise correlation among numeric predictors (records the max partner correlation for removed vars).
#'   \item High pairwise association among categorical predictors using Cramér's V (records max partner association).
#'   \item Splits the data into train/test with \code{caret::createDataPartition
#'   \item Performs multiple imputation using \pkg{mice}, returning completed datasets plus diagnostics.
#'   }

pp <- slos_pre_processing(
  data              = d,
  outcome           = outcome,
  remove_nzv        = TRUE,
  nzv_freq_cut      = 50,
  remove_corr_num   = TRUE,
  corr_cutoff_num   = 0.80,
  remove_corr_cat   = TRUE,
  corr_cutoff_cat   = 0.50,
  do_impute         = TRUE,
  p_train           = 0.80,
  include_outcome   = TRUE,
  mincor            = 0.10,
  m                 = 1,
  maxit             = 5,
  method            = "pmm",
  seed              = 123L,
  return_mids       = FALSE,
  return_matrices   = FALSE
)

pp$removed
pp$screening_metrics$nzv_table
pp$screening_metrics$num_removed_table
pp$screening_metrics$cat_removed_table

pp$imputation$imputed_counts$train
pp$imputation$numeric_drift$train
pp$imputation$categorical_drift$train
pp$imputation$missingness_before$test
pp$imputation$missingness_after$test

train_imp <- pp$imputation$train_completed
test_imp  <- pp$imputation$test_completed

df_final_featureselect <- train_imp[, setdiff(names(train_imp), non_predictors)]

# FEATURE SELECTION
# Supports Treebag (`treebag`) and Random Forest (`ranger`), optional normalization (via `caret::preProcess`), and k-fold cross-validation.

res <- slos_feature_select_rfe(
  data = df_final_featureselect,
  outcome = "UnitLengthStay_trunc",
  method = "treebag",
  cv_folds = 5,
  verbose = FALSE
)

print("Selected Columns:")
res$selected_vars
print("Removed Columns:")
setdiff(setdiff(names(train_imp), res$selected_vars), outcome)

# TRAINING

keep_cols <- union(setdiff(res$selected_vars, non_predictors), outcome)

train_fs <- train_imp[, keep_cols, drop = FALSE]
test_fs  <- test_imp[,  keep_cols, drop = FALSE]

fit <- slos_train_new_model(
  train   = train_fs,
  test    = test_imp,
  outcome = "UnitLengthStay_trunc",
  seed    = 998,
  icu_column = "icuid"
)

fit$metrics

# EVALUATING EFFICIENCY

# SLOS() needs the "icuid" column, so we keep it next to the model variables
# in the test set
test_slos <- test_imp[, union(keep_cols, "icuid"), drop = FALSE]

results <- SLOS(test_slos, "icuid", "UnitLengthStay_trunc", fit)

slos_values <- results$df_unit_slos$SLOS

general_slos <- sum(results$df_unit_slos$soma_los_obs) / sum(results$df_unit_slos$soma_los_esp)
general_slos

median_slos <- median(slos_values)
median_slos

q1_slos <- quantile(slos_values, 0.25)
q3_slos <- quantile(slos_values, 0.75)
q1_slos
q3_slos
