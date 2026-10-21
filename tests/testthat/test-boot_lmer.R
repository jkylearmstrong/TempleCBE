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

test_that("boot_p_value() never exceeds 1", {
  expect_equal(boot_p_value(rep(0, 5), 0, epsilon = 0), 1) # ties count in both tails
  expect_lte(boot_p_value(c(0, 0, 0, 1), 0, epsilon = 0), 1)
})

test_that("boot_p_value() checks its arguments", {
  expect_error(boot_p_value("a"), "must be numeric")
  expect_error(boot_p_value(1, null_value = c(1, 2)), "null_value")
  expect_error(boot_p_value(1, epsilon = -1), "epsilon")
})

# ---- the pieces of boot_lmer() ---------------------------------------------------------------------

test_that(".boot_resample() draws whole groups, with replacement, keeping each group's rows together", {
  d <- data.frame(id = rep(c("a", "b", "c"), times = c(2, 5, 3)), v = seq_len(10))
  rows_of <- split(seq_len(nrow(d)), d$id)
  sizes <- c(a = 2, b = 5, c = 3)
  for (seed in 1:30) {
    set.seed(seed)
    r <- .boot_resample(d, rows_of)
    counts <- table(r$id)
    # every drawn group comes with all its rows, as many times as it was drawn
    for (g in names(counts)) expect_equal(as.integer(counts[[g]]) %% sizes[[g]], 0)
    for (g in names(counts)) {
      expect_identical(sort(unique(r$v[r$id == g])), sort(d$v[d$id == g]))
    }
  }
  # groups are drawn with replacement: some resample repeats a group
  expect_true(any(vapply(1:30, function(seed) {
    set.seed(seed)
    r <- .boot_resample(d, rows_of)
    any(table(r$id) > sizes[names(table(r$id))])
  }, logical(1))))
})

test_that(".boot_resample() rescales within the resample, not on the whole data", {
  # group a has time 1..2, group b time 1..10
  d <- data.frame(id = rep(c("a", "b"), times = c(2, 10)), time = c(1:2, 1:10))
  rows_of <- split(seq_len(nrow(d)), d$id)
  found_narrow <- FALSE
  for (seed in 1:40) {
    set.seed(seed)
    raw <- .boot_resample(d, rows_of)
    set.seed(seed)
    scaled <- .boot_resample(d, rows_of, normalize = "time")
    expect_equal(scaled$time, (raw$time - min(raw$time)) / (max(raw$time) - min(raw$time)))
    expect_equal(range(scaled$time), c(0, 1))
    if (max(raw$time) < 10) found_narrow <- TRUE # a resample without group b: whole-data scaling would differ
  }
  expect_true(found_narrow)
})

test_that(".boot_fence_keep() keeps what lies strictly inside the fences, and the bulk when there is no spread", {
  x <- c(1:9, 100)
  expect_identical(.boot_fence_keep(x, "none"), rep(TRUE, 10))
  expect_identical(.boot_fence_keep(x, "outer"), c(rep(TRUE, 9), FALSE))
  expect_identical(.boot_fence_keep(x, "inner"), c(rep(TRUE, 9), FALSE))
  # an estimate on a fence is not strictly inside it
  edge <- c(0, 0, 0, 0, 1, 1, 1, 1, 2) # q1 = 0, q3 = 1, iqr = 1: inner fences -1.5 and 2.5
  expect_true(all(.boot_fence_keep(edge, "inner")))
  # no spread: the bulk is kept, what is off it is dropped
  expect_identical(.boot_fence_keep(rep(3, 5), "outer"), rep(TRUE, 5))
  expect_identical(.boot_fence_keep(c(rep(3, 8), 10), "outer"), c(rep(TRUE, 8), FALSE))
  expect_identical(.boot_fence_keep(5, "inner"), TRUE)
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
  expect_identical(attr(res, "n_incomparable"), 0L)
  draws <- attr(res, "draws")
  expect_identical(sort(unique(draws$term)), c("(Intercept)", "time"))
  expect_identical(max(draws$boot), 60L)
  expect_identical(nrow(boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 10, include_intercept = TRUE, seed = 1)), 2L)
})

test_that("boot_lmer() summarises the kept estimates and uses std.error as the tolerance of the p-value", {
  skip_if_not_installed("lme4")
  d <- boot_fixture()
  res <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 80, seed = 6)
  dr <- attr(res, "draws")
  kept <- dr$estimate[dr$term == "time" & dr$kept]
  expect_identical(res$n, length(kept))
  expect_equal(res$mean, mean(kept))
  expect_equal(res$median, stats::median(kept))
  expect_equal(res$lower, unname(stats::quantile(kept, 0.025)))
  expect_equal(res$upper, unname(stats::quantile(kept, 0.975)))
  expect_equal(res$p.value, boot_p_value(kept, 0, epsilon = res$std.error))
  # a different tolerance gives a different p-value on data where estimates are near the null
  near_null <- boot_fixture(slope = 0.02, seed = 3)
  r0 <- boot_lmer(y ~ time + (1 | id), near_null, group = "id", n_boot = 80, seed = 6)
  k0 <- attr(r0, "draws")
  k0 <- k0$estimate[k0$term == "time" & k0$kept]
  expect_equal(r0$p.value, boot_p_value(k0, 0, epsilon = r0$std.error))
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
  d <- boot_fixture(n_id = 30)
  # one group with an enormous slope: resamples that draw it (once or twice) or not give very different slopes
  d$y[d$id == "1"] <- d$y[d$id == "1"] + 4000 * d$time[d$id == "1"]
  none <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 150, fence = "none", seed = 4)
  outer <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 150, fence = "outer", seed = 4)
  inner <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 150, fence = "inner", seed = 4)
  expect_identical(attr(none, "draws")$kept, attr(none, "draws")$comparable)
  expect_lte(outer$n, none$n)
  expect_lte(inner$n, outer$n)
  expect_lt(inner$n, none$n) # something was dropped
  # the kept flags are exactly the fences of the draws, term by term
  for (res in list(outer = outer, inner = inner)) {
    dr <- attr(res, "draws")
    for (tm in unique(dr$term)) {
      expect_identical(dr$kept[dr$term == tm], .boot_fence_keep(dr$estimate[dr$term == tm], attr(res, "fence")))
    }
  }
  # the same draws underlie all three
  expect_equal(attr(none, "draws")$estimate, attr(inner, "draws")$estimate)
})

test_that("boot_lmer() does not mix contrasts when a resample lacks a factor level", {
  skip_if_not_installed("lme4")
  withr::local_seed(21)
  # level A occurs only in groups 1 and 2: resamples without them drop the fB column
  levels_of <- list("1" = c("A", "B", "C"), "2" = c("A", "B", "C"), "3" = c("B", "C"), "4" = c("B", "C"),
                    "5" = c("B", "C"), "6" = c("B", "C"), "7" = c("B", "C"), "8" = c("B", "C"))
  eff <- c(A = 0, B = 10, C = 30)
  d <- do.call(rbind, lapply(names(levels_of), function(g) {
    f <- rep(levels_of[[g]], each = 4)
    data.frame(id = g, f = factor(f, levels = c("A", "B", "C")), y = eff[f] + stats::rnorm(length(f), sd = 0.3) + stats::rnorm(1))
  }))
  res <- boot_lmer(y ~ f + (1 | id), d, group = "id", n_boot = 60, seed = 9, fence = "none")
  expect_gt(attr(res, "n_incomparable"), 0L)
  dr <- attr(res, "draws")
  fc <- dr$estimate[dr$term == "fC" & dr$kept]
  # fC is C - A in every kept replicate (30), never C - B (20) from a replicate that lost level A
  expect_gt(min(fc), 27)
  expect_identical(res$n[res$term == "fC"], length(fc))
  incomparable <- dr[!dr$comparable & dr$term == "fC", ]
  if (nrow(incomparable)) expect_lt(mean(incomparable$estimate), 25)
})

test_that("boot_lmer() falls back to another formula, or counts the failure", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 8)
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

test_that("boot_lmer() handles unused factor levels, missing groups and a group that is not the clustering", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 10)
  d$fid <- factor(d$id, levels = c(levels(d$id), "unused1", "unused2"))
  expect_no_error(boot_lmer(y ~ time + (1 | fid), d, group = "fid", n_boot = 5, seed = 1))
  d2 <- d
  d2$id[3] <- NA
  expect_error(boot_lmer(y ~ time + (1 | id), d2, group = "id", n_boot = 5), "missing values")
  d$site <- rep(1:2, length.out = nrow(d))
  expect_warning(
    boot_lmer(y ~ time + (1 | id), d, group = "site", n_boot = 5, seed = 1),
    "not a grouping factor"
  )
})

test_that("boot_lmer() counts singular fits and warns when many replicates fail", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 6)
  res <- boot_lmer(y ~ time + (1 | id), d, group = "id", n_boot = 20, seed = 1)
  expect_true(is.integer(attr(res, "n_singular")))
  expect_true(is.integer(attr(res, "n_nonconverged")))
  # two groups only: a resample that draws one group twice cannot estimate the random effect
  two <- boot_fixture(n_id = 2)
  expect_warning(
    boot_lmer(y ~ time + (1 | id), two, group = "id", n_boot = 40, seed = 1),
    "could not be fitted"
  )
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

test_that(".boot_groups() ignores unused factor levels", {
  d <- data.frame(id = factor(c("a", "a", "b"), levels = c("a", "b", "unused")))
  g <- .boot_groups(d, "id")
  expect_named(g, c("a", "b"))
  expect_identical(unname(lengths(g)), c(2L, 1L))
})

test_that("a grouping term the formula parse cannot read does not stop the run", {
  skip_if_not_installed("lme4")
  d <- boot_fixture(n_id = 8)
  # (lme4 itself may be unable to fit such a term on resampled data; the check is that our own parse does not stop the run)
  expect_no_error(suppressWarnings(boot_lmer(y ~ time + (1 | factor(id)), d, group = "id", n_boot = 5, seed = 1)))
})

test_that("boot_lmer() warns when many replicates are not comparable, and returns terms in model order", {
  skip_if_not_installed("lme4")
  withr::local_seed(5)
  # each group sees one factor level only: most resamples lose a level
  d <- do.call(rbind, lapply(1:3, function(i) {
    data.frame(id = as.character(i), f = factor(c("A", "B", "C")[i], levels = c("A", "B", "C")), y = c(0, 10, 30)[i] + stats::rnorm(6, sd = 0.3))
  }))
  expect_warning(
    res <- boot_lmer(y ~ f + (1 | id), d, group = "id", n_boot = 30, seed = 3, fence = "none"),
    "lack a fixed-effect column"
  )
  expect_gt(attr(res, "n_incomparable"), 0L)
  expect_identical(res$term, c("fB", "fC")) # model order, not the order of the first draw
  expect_output(print(res), "not comparable")
})
