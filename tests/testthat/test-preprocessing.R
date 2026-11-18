make_pp_data <- function(n = 120, seed = 2) {
  set.seed(seed)
  x1 <- rnorm(n)
  d <- data.frame(
    x1 = x1,
    x2 = x1 + rnorm(n, sd = 0.01),   # almost a copy of x1 (high correlation)
    x3 = rnorm(n),
    const = 1,                        # zero variance
    f1 = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  d$f2 <- d$f1                        # perfectly associated factor
  d$y <- 2 * d$x1 + d$x3 + rnorm(n, sd = 0.1)
  d
}

test_that("slos_pre_processing validates its input", {
  d <- make_pp_data()
  expect_error(slos_pre_processing(as.matrix(d), "y"))
  expect_error(slos_pre_processing(d, "nope"), "not found")
})

test_that("slos_pre_processing screens features and splits the data", {
  d <- make_pp_data()
  pp <- slos_pre_processing(d, outcome = "y", do_impute = FALSE, seed = 1)

  expect_true("const" %in% pp$removed$nzv)
  expect_length(pp$removed$corr_num, 1)
  expect_true(pp$removed$corr_num %in% c("x1", "x2"))
  expect_length(pp$removed$corr_cat, 1)
  expect_true("y" %in% names(pp$data_screened))
  expect_null(pp$imputation)

  expect_equal(length(pp$split$train_idx) + length(pp$split$test_idx), nrow(d))
  expect_equal(length(intersect(pp$split$train_idx, pp$split$test_idx)), 0)
  expect_s3_class(pp$screening_metrics$nzv_table, "data.frame")
})

test_that("slos_pre_processing is reproducible for a given seed", {
  d <- make_pp_data()
  a <- slos_pre_processing(d, "y", seed = 5)
  b <- slos_pre_processing(d, "y", seed = 5)
  expect_equal(a$split$train_idx, b$split$train_idx)
})

test_that("slos_pre_processing imputes missing values", {
  skip_on_cran()
  d <- make_pp_data()
  set.seed(3)
  d$x3[sample(nrow(d), 15)] <- NA
  pp <- slos_pre_processing(d, "y", do_impute = TRUE, m = 1, maxit = 1,
                            seed = 1)
  expect_equal(sum(is.na(pp$imputation$train_completed)), 0)
  expect_equal(sum(is.na(pp$imputation$test_completed)), 0)
  expect_true(all(c("train", "test") %in% names(pp$imputation$missingness_before)))
})
