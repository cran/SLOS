registerS3method("predict", "slos_fake_stack", predict.slos_fake_stack)

test_that("slos_load_pretrained_model fails gracefully without internet", {
  local_mocked_bindings(has_internet = function() FALSE, .package = "curl")
  rm(list = ls(SLOS:::.slos_cache), envir = SLOS:::.slos_cache)
  expect_error(
    slos_load_pretrained_model(cache = FALSE),
    "internet connection"
  )
})

test_that("slos_load_pretrained_model reuses the session cache", {
  assign("SLOS_model", "cached-model", envir = SLOS:::.slos_cache)
  withr_cleanup <- function() rm("SLOS_model", envir = SLOS:::.slos_cache)
  on.exit(withr_cleanup())
  expect_identical(slos_load_pretrained_model(), "cached-model")
})

test_that("slos_predict_pretrained validates the outcome column", {
  expect_error(
    slos_predict_pretrained(data.frame(x = 1:3), outcome = "los",
                            model = make_fake_model()$stacked_model),
    "not found"
  )
})

test_that("slos_predict_pretrained returns predictions and metrics", {
  d <- make_icu_data()
  res <- slos_predict_pretrained(
    d, outcome = "los", model = make_fake_model(slope = 1)$stacked_model
  )
  expect_named(res, c("predictions", "comparison", "RMSE", "MAE", "R2"))
  expect_equal(nrow(res$predictions), nrow(d))
  expect_equal(res$predictions$predictions, d$x)
  expect_equal(res$comparison$Observations, d$los)
  expect_lt(res$RMSE, 1)
})

test_that("slos_predict_custom encodes data like the training data", {
  d <- make_icu_data()
  res <- SLOS:::slos_predict_custom(d, make_fake_model(slope = 2),
                                    outcome = "los")
  expect_equal(res$predictions$predictions, 2 * d$x)
  expect_error(SLOS:::slos_predict_custom(d, make_fake_model(), "nope"),
               "not found")
})
