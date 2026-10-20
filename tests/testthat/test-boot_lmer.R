boot_fixture <- function(n_id = 20, n_t = 5, slope = 2, seed = 11) {
  withr::local_seed(seed)
  id <- rep(seq_len(n_id), each = n_t)
  time <- rep(seq_len(n_t), times = n_id)
  u <- stats::rnorm(n_id, sd = 1.5)[id]
  data.frame(id = factor(id), time = time, y = 5 + slope * time + u + stats::rnorm(n_id * n_t, sd = 0.5))
}

test_that("boot_lmer() returns one row per fixed effect with sensible intervals", {
  skip_if_not_installed("lme4")
  d <- boot_fixture()
  res <- boot_lmer(y ~ time + (1 | id), d, n_boot = 60, seed = 1)
  expect_s3_class(res, "boot_lmer")
  expect_s3_class(res, "tbl_df")
  expect_identical(res$term, c("(Intercept)", "time"))
  expect_named(res, c("term", "estimate", "std.error", "conf.low", "conf.high", "n_boot"))
  expect_equal(res$estimate, unname(lme4::fixef(attr(res, "fit"))))
  expect_true(all(res$conf.low < res$estimate & res$estimate < res$conf.high))
  expect_true(all(res$std.error > 0))
  expect_lt(abs(res$estimate[2] - 2), 0.2)
  expect_true(res$conf.low[2] < 2 && 2 < res$conf.high[2])
  expect_identical(attr(res, "n_removed"), 0L)
  expect_identical(res$n_boot + attr(res, "n_failed"), rep(60L, 2))
})

test_that("boot_lmer() is reproducible with a seed and leaves the session's random numbers alone", {
  skip_if_not_installed("lme4")
  d <- boot_fixture()
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  a <- boot_lmer(y ~ time + (1 | id), d, n_boot = 30, seed = 5)
  after <- stats::runif(1)
  b <- boot_lmer(y ~ time + (1 | id), d, n_boot = 30, seed = 5)
  expect_equal(as.data.frame(a)[names(a)], as.data.frame(b)[names(b)])
  expect_identical(after, before) # the seed argument did not move the session's stream
})

test_that("boot_lmer() removes outliers of the response by Tukey's fences and reports how many", {
  skip_if_not_installed("lme4")
  d <- boot_fixture()
  d$y[c(3, 40)] <- c(500, -500)
  d$y[7] <- NA
  expect_identical(attr(boot_lmer(y ~ time + (1 | id), d, n_boot = 5, fence = "none", seed = 1), "n_removed"), 0L)
  inner <- boot_lmer(y ~ time + (1 | id), d, n_boot = 5, fence = "inner", seed = 1)
  outer <- boot_lmer(y ~ time + (1 | id), d, n_boot = 5, fence = "outer", seed = 1)
  expect_identical(attr(inner, "n_removed"), 2L)
  expect_identical(attr(outer, "n_removed"), 2L)
  # with the two wild values removed the slope is back near the truth
  expect_lt(abs(inner$estimate[2] - 2), 0.3)
  expect_equal(attr(inner, "fence"), "inner")
})

test_that("boot_lmer() checks its arguments", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 6)
  expect_error(boot_lmer("y ~ x", d), "must be a formula")
  expect_error(boot_lmer(y ~ time, d), "random-effect term")
  expect_error(boot_lmer(y ~ time + (1 | id), d[0, ]), "non-empty data frame")
  expect_error(boot_lmer(y ~ time + (1 | id), d, n_boot = 0), "n_boot")
  expect_error(boot_lmer(y ~ time + (1 | id), d, conf_level = 1), "conf_level")
  expect_error(boot_lmer(y ~ time + (1 | id), d, fence = "wide"), "should be one of")
  dd <- d
  dd$grp <- "a"
  expect_error(boot_lmer(nope ~ time + (1 | id), d, fence = "inner"), "single numeric response")
})

test_that("print.boot_lmer() summarises the run", {
  skip_if_not_installed("lme4")
  res <- boot_lmer(y ~ time + (1 | id), boot_fixture(n_id = 8), n_boot = 10, seed = 2)
  expect_output(print(res), "Bootstrap of a mixed model: 2 fixed effect")
})
