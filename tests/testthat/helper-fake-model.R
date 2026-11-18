# A tiny stand-in for a model returned by slos_train_new_model(), so SLOS()
# can be tested offline and without training anything.
predict.slos_fake_stack <- function(object, newdata, ...) {
  object$slope * newdata$x
}

make_fake_model <- function(slope = 1) {
  train <- data.frame(x = c(1, 2, 3, 4))
  stacked <- structure(list(slope = slope), class = "slos_fake_stack")
  list(
    stacked_model = stacked,
    dummyVars     = caret::dummyVars(~ ., data = train, fullRank = TRUE),
    feature_names = "x"
  )
}

make_icu_data <- function(n_per_icu = 20, seed = 1) {
  set.seed(seed)
  icu <- rep(c("A", "B", "C"), each = n_per_icu)
  x <- stats::runif(length(icu), 1, 10)
  data.frame(
    icu = icu,
    x = x,
    los = x + stats::rnorm(length(icu), sd = 0.5),
    stringsAsFactors = FALSE
  )
}
