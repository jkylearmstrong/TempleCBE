long_df <- function() {
  data.frame(
    id = rep(c("s1", "s2", "s3", "s4"), each = 4),
    time = rep(c(15, 0, 5, 120), 4), # deliberately not in time order
    x = c(
      70, 80, 90, 60,   # s1: t0 = 80, t5 = 90, t15 = 70, t120 = 60
      61, 60, 62, 63,   # s2
      5, 5, 5, 5,       # s3 constant
      NA, 0, 4, 8       # s4: baseline 0, missing at t15
    )
  )
}

test_that("each method follows its definition, with values ordered by time", {
  r <- transform_longitudinal_features(long_df(), "id", "time", "x")
  expect_named(r, c("id", "feature", "method", "value", "n_obs"))
  v <- function(id, m) r$value[r$id == id & r$method == m]
  # s1 ordered by time: 80, 90, 70, 60
  expect_equal(v("s1", "pct_change_baseline"), 100 * (60 - 80) / 80)
  expect_equal(v("s1", "variance"), var(c(80, 90, 70, 60)))
  expect_equal(v("s1", "sd"), sd(c(80, 90, 70, 60)))
  # max 90 at t5 comes after min 60 at t120? no: min is later, so the swing is negative
  expect_equal(v("s1", "max_signed_diff"), -(90 - 60))
  # s2 ordered: 60, 62, 61, 63 -> min first then max later: positive
  expect_equal(v("s2", "max_signed_diff"), 63 - 60)
  expect_equal(unique(r$n_obs[r$id == "s1"]), 4L)
})

test_that("constants, zero baselines, missing values and single observations are handled", {
  r <- transform_longitudinal_features(long_df(), "id", "time", "x")
  v <- function(id, m) r$value[r$id == id & r$method == m]
  expect_equal(v("s3", "max_signed_diff"), 0)
  expect_equal(v("s3", "variance"), 0)
  expect_equal(v("s3", "pct_change_baseline"), 0)
  expect_true(is.na(v("s4", "pct_change_baseline"))) # baseline 0
  expect_equal(r$n_obs[r$id == "s4"][1], 3L) # the missing value is dropped
  expect_equal(v("s4", "sd"), sd(c(0, 4, 8)))
  one <- data.frame(id = "a", time = 0, x = 3)
  o <- transform_longitudinal_features(one, "id", "time", "x")
  expect_true(all(is.na(o$value)))
  expect_equal(unique(o$n_obs), 1L)
})

test_that("methods, several features, wide format and id column naming", {
  d <- long_df()
  d$y <- d$x * 2
  r <- transform_longitudinal_features(d, "id", "time", c("x", "y"), methods = c("sd", "variance"))
  expect_equal(nrow(r), 4L * 2L * 2L)
  expect_equal(r$value[r$id == "s1" & r$feature == "y" & r$method == "sd"], 2 * sd(c(80, 90, 70, 60)))
  w <- transform_longitudinal_features(d, "id", "time", c("x", "y"), methods = "sd", format = "wide")
  expect_named(w, c("id", "x__sd", "y__sd"))
  expect_equal(nrow(w), 4L)
  expect_equal(w$id, c("s1", "s2", "s3", "s4"))
})

test_that("input is validated", {
  d <- long_df()
  expect_error(transform_longitudinal_features(d, "id", "time", "x", methods = "median"), "methods")
  expect_error(transform_longitudinal_features(d, "nope", "time", "x"), "id_col")
  expect_error(transform_longitudinal_features(d, "id", "nope", "x"), "time_col")
  expect_error(transform_longitudinal_features(d, "id", "time", "zzz"), "not in")
  expect_error(transform_longitudinal_features(d, "id", "time", "id"), "numeric")
  d$time <- as.character(d$time)
  expect_error(transform_longitudinal_features(d, "id", "time", "x"), "numeric")
  d2 <- long_df()
  d2$id[1] <- NA
  expect_error(transform_longitudinal_features(d2, "id", "time", "x"), "missing")
  expect_error(transform_longitudinal_features(1, "id", "time", "x"), "data frame")
})

test_that("percent change keeps its direction for a negative baseline; id names that would clash are refused", {
  d <- data.frame(id = "a", time = c(0, 1), x = c(-10, -20))
  r <- transform_longitudinal_features(d, "id", "time", "x", methods = "pct_change_baseline")
  expect_equal(r$value, -100) # more negative: went down
  d2 <- data.frame(value = "a", time = c(0, 1), x = c(1, 2))
  expect_error(transform_longitudinal_features(d2, "value", "time", "x"), "must not be named")
})
