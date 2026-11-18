# SLOS 2.0.0

## Breaking changes

* `SLOS()` now takes `icu_column` and `outcome` (and an optional `model`)
  explicitly: `SLOS(data, icu_column, outcome, model = NULL)`.
* `load_SLOSModel()` is replaced by `slos_load_pretrained_model()` and
  `predict_and_evaluate()` by `slos_predict_pretrained()`.

## New features

* Complete workflow for building a new model from local data:
  `slos_pre_processing()` (near-zero variance, correlation and Cramér's V
  screening, train/test split and multiple imputation with \pkg{mice}),
  `slos_feature_select_rfe()` and `slos_apply_feature_selection()` (recursive
  feature elimination), and `slos_train_new_model()` (stacked linear model +
  random forest).
* `SLOS()` accepts a model returned by `slos_train_new_model()`.
* New `show_funnel` argument in `SLOS()`.
* New vignette, "Getting started with SLOS", and example scripts in
  `inst/examples`.

## Other changes

* The pre-trained model is downloaded to a temporary file, cached for the
  R session, and a clear error is raised when there is no internet connection
  or the download fails.
* Package documentation, examples and tests were rewritten.

# SLOS 1.0.1

* Initial CRAN release.
