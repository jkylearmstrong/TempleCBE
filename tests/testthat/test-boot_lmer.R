boot_fixture <- function(n_id = 20, n_t = 5, slope = 2, seed = 11) {
  withr::local_seed(seed)
  id <- rep(seq_len(n_id), each = n_t)
  time <- rep(seq_len(n_t), times = n_id)
  u <- stats::rnorm(n_id, sd = 1.5)[id]
  data.frame(id = factor(id), time = time, y = 5 + slope * time + u + stats::rnorm(n_id * n_t, sd = 0.5))
}

# ---- boot_p_value() ------------------------------------------------------------------------------

test_that("boot_p_value() doubles the smaller tail", {
  expect_equal(boot_p_value(c(1, 2, 3, 4)), 0) # nothing at or below the null
  expect_equal(boot_p_value(c(-1, 1, 2, 3)), 2 * 0.25)
  expect_equal(boot_p_value(c(-1, -1, 1, 1)), 1)
  expect_equal(boot_p_value(c(0, 2, 2, 2), null_value = 1, epsilon = 0.5), 2 * 0.25)
  # estimates within epsilon of the null count on neither side
  expect_equal(boot_p_value(c(0, 0, 5, 5), epsilon = 1e-5), 0)
  expect_equal(boot_p_value(c(0, 0, 5, 5), epsilon = 0), 2 * 0.5)
  expect_equal(boot_p_value(c(NA, 1, 2)), 0)
  expect_true(is.na(boot_p_value(numeric())))
  expect_true(is.na(boot_p_value(NA_real_)))
})

test_that("boot_p_value() checks its arguments", {
  expect_error(boot_p_value("a"), "must be numeric")
  expect_error(boot_p_value(1, null_value = c(1, 2)), "null_value")
  expect_error(boot_p_value(1, epsilon = -1), "epsilon")
})

# ---- boot_lmer() ---------------------------------------------------------------------------------

test_that("boot_lmer() resamples groups and summarises each fixed effect", {
  skip_if_not_installed("lme4")
  d <- boot_fixture()
  res <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 60, seed = 1)
  expect_s3_class(res, "boot_lmer")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("term", "n", "mean", "sd", "std.error", "median", "lower", "upper", "p.value"))
  expect_identical(res$term, "time") # the intercept is left out by default
  expect_lt(abs(res$mean - 2), 0.2)
  expect_true(res$lower < 2 && 2 < res$upper)
  expect_equal(res$std.error, res$sd / sqrt(res$n))
  expect_lt(res$p.value, 0.05)
  expect_identical(attr(res, "n_boot"), 60L)
  expect_identical(attr(res, "n_failed"), 0L)
  draws <- attr(res, "draws")
  expect_identical(sort(unique(draws$term)), c("(Intercept)", "time"))
  expect_identical(max(draws$boot), 60L)
  expect_identical(nrow(boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 10, include_intercept = TRUE, seed = 1)), 2L)
})

test_that("boot_lmer() draws whole groups: every replicate holds complete groups", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 8)
  # a model whose only fixed effect is the cluster size makes the replicate's row count visible
  d$ones <- 1
  res <- boot_lmer(y ~ 0 + ones + (1 | id), d, group = "id", n_boot = 15, fence = "none", seed = 3)
  draws <- attr(res, "draws")
  expect_true(all(draws$term == "ones"))
  expect_identical(nrow(draws), 15L)
})

test_that("boot_lmer() is reproducible with a seed and leaves the session's random numbers alone", {
  skip_if_not_installed("lme4")
  d <- boot_fixture()
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  a <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 20, seed = 5)
  after <- stats::runif(1)
  b <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 20, seed = 5)
  expect_equal(as.data.frame(a)[names(a)], as.data.frame(b)[names(b)])
  expect_equal(attr(a, "draws"), attr(b, "draws"))
  expect_identical(after, before)
})

test_that("boot_lmer() rescales named predictors to [0, 1] within each resample", {
  skip_if_not_installed("lme4")
  d <- boot_fixture()
  raw <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 30, seed = 2)
  scaled <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 30, normalize = "time", seed = 2)
  # time runs 1..5 in every group, so one unit of the rescaled predictor is 4 units of time
  expect_equal(scaled$mean, raw$mean * 4, tolerance = 1e-6)
})

test_that("boot_lmer() drops outlying estimates of each term by the chosen fence", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 10)
  # two groups with wild responses make some replicates produce extreme slopes
  d$y[d$id == "1"] <- d$y[d$id == "1"] + c(-400, 400, -400, 400, -400)
  none <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 80, fence = "none", seed = 4)
  outer <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 80, fence = "outer", seed = 4)
  inner <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 80, fence = "inner", seed = 4)
  expect_identical(attr(none, "draws")$kept, rep(TRUE, nrow(attr(none, "draws"))))
  expect_lte(outer$n, none$n)
  expect_lte(inner$n, outer$n)
  # the kept flags are exactly the strict fences of calculate_fences(), term by term
  for (res in list(outer = outer, inner = inner)) {
    dr <- attr(res, "draws")
    fn <- attr(res, "fence")
    for (tm in unique(dr$term)) {
      e <- dr$estimate[dr$term == tm]
      f <- calculate_fences(e)
      expect_identical(
        dr$kept[dr$term == tm],
        f[[paste0("lower_", fn, "_fence")]] < e & e < f[[paste0("upper_", fn, "_fence")]]
      )
    }
  }
  # the same draws underlie all three
  expect_equal(attr(none, "draws")$estimate, attr(inner, "draws")$estimate)
})

test_that("boot_lmer() falls back to another formula, or counts the failure", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 8)
  # a model that always fails: the variable does not exist
  expect_warning(
    none <- boot_lmer(y ~ missing_var + (1 | id), d, group = "id", n_boot = 5, seed = 1),
    "No bootstrap replicate could be fitted"
  )
  expect_identical(attr(none, "n_failed"), 5L)
  expect_identical(nrow(none), 0L)

  fb <- boot_lmer(y ~ missing_var + (1 | id), d, group = "id", n_boot = 5,
                  fallback_formula = y ~ time + (1 | id), seed = 1)
  expect_identical(attr(fb, "n_failed"), 0L)
  expect_identical(attr(fb, "n_fallback"), 5L)
  expect_identical(fb$term, "time")
  expect_true(all(attr(fb, "draws")$fallback))
})

test_that("boot_lmer() checks its arguments", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 6)
  expect_error(boot_lmer("y ~ x", d, "id"), "must be a formula")
  expect_error(boot_lmer(y ~ time, d, "id"), "random-effect term")
  expect_error(boot_lmer(y ~ time + (1 | id), d[0, ], "id"), "non-empty data frame")
  expect_error(boot_lmer(y ~ time + (1 | id), d, "nope"), "name of a column")
  expect_error(boot_lmer(y ~ time + (1 | id), d, "id", n_boot = 0), "n_boot")
  expect_error(boot_lmer(y ~ time + (1 | id), d, "id", conf_level = 1), "conf_level")
  expect_error(boot_lmer(y ~ time + (1 | id), d, "id", fence = "wide"), "should be one of")
  expect_error(boot_lmer(y ~ time + (1 | id), d, "id", normalize = "id"), "numeric columns")
  expect_error(boot_lmer(y ~ time + (1 | id), d, "id", fallback_formula = "y ~ 1"), "fallback_formula")
})

test_that("print.boot_lmer() summarises the run", {
  skip_if_not_installed("lme4")
  res <- boot_lmer(y ~ time + (1 | id), boot_fixture(n_id = 8), group = "id", n_boot = 10, seed = 2)
  expect_output(print(res), "Cluster bootstrap of a mixed model: 10 replicate")
})
