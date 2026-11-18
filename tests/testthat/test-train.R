test_that("slos_train_new_model validates its input", {
  d <- data.frame(x = 1:10, y = rnorm(10))
  expect_error(slos_train_new_model(as.matrix(d)))
  expect_error(slos_train_new_model(d, outcome = "nope"), "not found")
  d$y <- as.character(d$y)
  expect_error(slos_train_new_model(d, outcome = "y"), "numeric")
})

test_that("slos_train_new_model trains a model usable by SLOS()", {
  skip_on_cran()
  skip_if_not_installed("e1071")
  skip_if_not_installed("ems")
  set.seed(10)
  n <- 240
  d <- data.frame(
    icu = rep(paste0("U", 1:4), each = n / 4),
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n),
    g = factor(sample(c("u", "v"), n, replace = TRUE))
  )
  d$los <- 5 + 2 * d$x1 + d$x2 + (d$g == "u") + rnorm(n, sd = 0.3)
  idx <- seq_len(n) %% 4 != 0

  fit <- slos_train_new_model(
    d[idx, ], outcome = "los", icu_column = "icu", test = d[!idx, ],
    cv_folds = 2, verbose_iter = FALSE,
    base_ranger_grid = data.frame(.mtry = 2, .splitrule = "variance",
                                  .min.node.size = 5),
    meta_grid = expand.grid(mtry = 2, min.node.size = 5,
                            splitrule = "variance"),
    save_path = file.path(tempdir(), "slos_test_model")
  )
  expect_true(file.exists(file.path(tempdir(), "slos_test_model.RData")))
  expect_named(fit$metrics, c("patient", "icu"))
  expect_named(fit$metrics$patient, c("RMSE", "MAE", "R2"))
  expect_equal(length(fit$predictions), sum(!idx))

  res <- SLOS(d[!idx, ], "icu", "los", model = fit, show_funnel = FALSE)
  expect_equal(nrow(res$df_unit_slos), 4)
})

test_that("slos_train_new_model warns when icu_column is not in test", {
  skip_on_cran()
  skip_if_not_installed("e1071")
  set.seed(11)
  d <- data.frame(x1 = rnorm(120), x2 = rnorm(120))
  d$los <- 3 + d$x1 + rnorm(120, sd = 0.2)
  expect_warning(
    slos_train_new_model(
      d[1:90, ], outcome = "los", test = d[91:120, ], icu_column = "icuid",
      cv_folds = 2, verbose_iter = FALSE,
      base_ranger_grid = data.frame(.mtry = 1, .splitrule = "variance",
                                    .min.node.size = 5),
      meta_grid = expand.grid(mtry = 2, min.node.size = 5,
                              splitrule = "variance")
    ),
    "ICU-level metrics"
  )
})
