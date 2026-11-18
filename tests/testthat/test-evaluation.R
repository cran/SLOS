registerS3method("predict", "slos_fake_stack", predict.slos_fake_stack)

test_that("SLOS validates its input", {
  d <- make_icu_data()
  expect_error(SLOS(as.matrix(d), "icu", "los"), "data frame")
  expect_error(SLOS(d[0, ], "icu", "los", model = make_fake_model()),
               "at least one row")
  expect_error(SLOS(d, "icu_missing", "los", model = make_fake_model()),
               "Missing required columns: icu_missing")
  expect_error(SLOS(d, "icu", c("los", "other"), model = make_fake_model()),
               "Missing required columns")
})

test_that("SLOS computes unit-level SLOS with a custom model", {
  skip_if_not_installed("ems")
  d <- make_icu_data()
  res <- SLOS(d, icu_column = "icu", outcome = "los",
              model = make_fake_model(slope = 1), show_funnel = FALSE)

  expect_named(res, c("df_unit_slos", "plot_SLOS_obs_prev", "funnel_plot"))
  expect_s3_class(res$plot_SLOS_obs_prev, "ggplot")

  u <- res$df_unit_slos
  expect_equal(nrow(u), 3)
  expect_equal(u$admissoes, rep(20L, 3))

  # expected LoS is the prediction clamped to [0, 21]
  pred <- pmin(pmax(d$x, 0), 21)
  expect_equal(u$soma_los_obs, as.numeric(tapply(d$los, d$icu, sum)),
               ignore_attr = TRUE)
  expect_equal(u$soma_los_esp, as.numeric(tapply(pred, d$icu, sum)),
               ignore_attr = TRUE)
  expect_equal(u$SLOS, u$soma_los_obs / u$soma_los_esp)
})

test_that("SLOS clamps predictions to the 0-21 day range", {
  skip_if_not_installed("ems")
  d <- make_icu_data()
  d$x <- d$x * 10  # predictions far above 21 days
  res <- SLOS(d, "icu", "los", model = make_fake_model(), show_funnel = FALSE)
  expect_true(all(res$df_unit_slos$soma_los_esp <= 21 * 20))
})
