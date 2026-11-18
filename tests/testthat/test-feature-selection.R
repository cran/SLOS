test_that("slos_coerce_predictors harmonises types", {
  x <- data.frame(
    ch = c("b", "a", NA, "a"),
    lg = c(TRUE, FALSE, TRUE, NA),
    int = 1:4,
    num = c(0.5, 1, 2, 3),
    stringsAsFactors = FALSE
  )
  out <- SLOS:::slos_coerce_predictors(x, verbose = FALSE)

  expect_s3_class(out$x$ch, "factor")
  expect_equal(levels(out$x$ch), c("a", "b"))
  expect_s3_class(out$x$lg, "factor")
  expect_equal(levels(out$x$lg), c("FALSE", "TRUE"))
  expect_type(out$x$int, "double")
  expect_equal(out$levels_map$ch, c("a", "b"))
  expect_message(SLOS:::slos_coerce_predictors(x), "Coercion summary")
})

test_that("slos_align_factor_levels maps unseen levels to NA with a warning", {
  nd <- data.frame(ch = c("a", "z", "b"), stringsAsFactors = FALSE)
  expect_warning(
    out <- SLOS:::slos_align_factor_levels(nd, list(ch = c("a", "b"))),
    "unseen levels"
  )
  expect_equal(levels(out$ch), c("a", "b"))
  expect_true(is.na(out$ch[2]))
  expect_silent(
    SLOS:::slos_align_factor_levels(nd, list(ch = c("a", "b", "z")),
                                    verbose = FALSE)
  )
})

fake_rfe <- function() {
  structure(
    list(selected_vars = c("a", "b"), preproc = NULL, levels_map = NULL),
    class = "slos_rfe_result"
  )
}

test_that("slos_apply_feature_selection keeps the selected columns", {
  nd <- data.frame(b = 1:3, a = 4:6, c = 7:9)
  out <- slos_apply_feature_selection(nd, fake_rfe())
  expect_named(out, c("a", "b"))
  expect_error(slos_apply_feature_selection(nd, list()), "slos_rfe_result")
})

test_that("slos_apply_feature_selection handles missing columns", {
  nd <- data.frame(a = 1:3)
  expect_error(slos_apply_feature_selection(nd, fake_rfe()),
               "Selected columns missing")
  expect_warning(
    out <- slos_apply_feature_selection(nd, fake_rfe(), fill_missing = "zero"),
    "Selected columns missing"
  )
  expect_equal(out$b, c(0, 0, 0))
  expect_warning(
    out <- slos_apply_feature_selection(nd, fake_rfe(), strict = FALSE),
    "Selected columns missing"
  )
  expect_named(out, "a")
})

test_that("slos_feature_select_rfe validates its input", {
  d <- data.frame(x = 1:10, y = rnorm(10))
  skip_if_not_installed("e1071")
  expect_error(slos_feature_select_rfe(), "Provide either")
  expect_error(slos_feature_select_rfe(data = d, outcome = "nope"),
               "not found")
  expect_error(slos_feature_select_rfe(data = d, outcome = c("x", "y")),
               "single string")
  d$y <- as.character(d$y)
  expect_error(slos_feature_select_rfe(data = d, outcome = "y"), "numeric")
})

test_that("slos_feature_select_rfe runs end to end (treebag)", {
  skip_on_cran()
  skip_if_not_installed("ipred")
  skip_if_not_installed("e1071")
  set.seed(1)
  d <- data.frame(
    x1 = rnorm(80), x2 = rnorm(80), x3 = rnorm(80), x4 = rnorm(80)
  )
  d$y <- 3 * d$x1 + d$x2 + rnorm(80, sd = 0.1)
  res <- slos_feature_select_rfe(d, "y", method = "treebag",
                                 sizes = c(2, 3), cv_folds = 2,
                                 verbose = FALSE, seed = 1)
  expect_s3_class(res, "slos_rfe_result")
  expect_true("x1" %in% res$selected_vars)
  expect_named(slos_apply_feature_selection(d, res), res$selected_vars)
})
