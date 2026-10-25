test_that("tbl_feature_importance_dalex summarises and ranks permutations", {
  fi <- data.frame(
    variable = rep(c("_full_model_", "a", "b", "_baseline_"), each = 4),
    permutation = rep(0:3, 4),
    dropout_loss = c(rep(0.2, 4), 0.30, 0.31, 0.29, 0.33, 0.22, 0.21, 0.23, 0.22, rep(0.5, 4)),
    label = "m"
  )
  tb <- tbl_feature_importance_dalex(fi)
  expect_equal(tb$variable, c("a", "b"))
  expect_equal(tb$rank, 1:2)
  expect_equal(tb$mean[1], mean(c(0.30, 0.31, 0.29, 0.33)))
  expect_equal(tb$dropout_loss[1], 0.30)
  expect_equal(tb$max[2], 0.23)
  expect_equal(nrow(tbl_feature_importance_dalex(fi, keep_reference = TRUE)), 4)
  expect_equal(nrow(tbl_feature_importance_dalex(fi, top_n = 1)), 1)
  expect_equal(tbl_feature_importance_dalex(fi, rank_by = "min", id = list(index = "I1"))$index, c("I1", "I1"))
  expect_error(tbl_feature_importance_dalex(fi[-1]), "lacks")
  expect_error(tbl_feature_importance_dalex(fi, top_n = 0), "top_n")
  expect_error(tbl_feature_importance_dalex(fi, id = list(1)), "named")
  expect_error(tbl_feature_importance_dalex(fi[fi$variable == "a", ][0, ]), "No variables")
})

test_that("batch_cutpoint_survdiff scans cutpoints and bins p-values", {
  skip_if_not_installed("survival")
  set.seed(2)
  n <- 120
  x <- rnorm(n)
  d <- data.frame(time = rexp(n, exp(0.8 * x)), status = rbinom(n, 1, 0.8), x = x,
                  z = rnorm(n), k = 1)
  res <- batch_cutpoint_survdiff(d, "time", "status", c("x", "z", "k"))
  expect_setequal(unique(res$feature), c("x", "z"))
  expect_equal(sum(res$feature == "x"), length(unique(x)) - 1)
  expect_true(all(res$n_low + res$n_high == n))
  expect_true(is.ordered(res$p_type))
  expect_equal(levels(res$p_type)[1], "p >= 0.1")
  expect_true(all(res$p_type[res$p < 0.001] == "p < 0.001"))
  expect_true(all(res$p_type[res$p >= 0.1] == "p >= 0.1"))
  expect_true(all(res$cox_coef[res$feature == "x"] > 0))

  q <- batch_cutpoint_survdiff(d, "time", "status", "x", cutpoints = "quantiles", probs = c(0.25, 0.5))
  expect_equal(nrow(q), 2)
  expect_equal(nrow(batch_cutpoint_survdiff(d, "time", "status", "x", min_n = n)), 0)

  expect_error(batch_cutpoint_survdiff(d, "time", "status", "nope"), "not in")
  expect_error(batch_cutpoint_survdiff(d, "time", "status", character()), "empty")
  d$f <- "a"
  expect_error(batch_cutpoint_survdiff(d, "time", "status", "f"), "Non-numeric")
  expect_error(batch_cutpoint_survdiff(d, "time", "status", "x", p_breaks = c(0.1, 0.05)), "increasing")
  expect_error(batch_cutpoint_survdiff(d, "time", "status", "x", cutpoints = "quantiles", probs = 1), "probs")
  expect_error(batch_cutpoint_survdiff(1, "time", "status", "x"), "data frame")
})

test_that("first_significant_cutpoint follows the Cox direction", {
  x <- tibble::tibble(feature = "x", cutpoint = 1:5, p = c(0.2, 0.04, 0.03, 0.0005, 0.5),
                      cox_coef = 0.3)
  out <- first_significant_cutpoint(x, alpha = c(0.05, 0.001))
  expect_equal(out$cutpoint, c(2, 4))
  expect_equal(out$alpha, c(0.05, 0.001))
  x$cox_coef <- -0.3
  expect_equal(first_significant_cutpoint(x, alpha = 0.05)$cutpoint, 4)
  expect_equal(nrow(first_significant_cutpoint(x, alpha = 1e-6)), 0)
  expect_error(first_significant_cutpoint(x[-1]), "lacks")
  expect_error(first_significant_cutpoint(x, alpha = 0), "alpha")
})

test_that("calc_grouped_summary returns long raw statistics", {
  d <- data.frame(g = c("a", "a", "b", "b", NA), v = c(1, 3, NA, NA, 5), w = c(2, 4, 6, 8, 10))
  s <- calc_grouped_summary(d, by = "g")
  expect_equal(nrow(s), 6)
  a_v <- s[s$g %in% "a" & s$variable == "v", ]
  expect_equal(a_v$mean, 2)
  expect_equal(a_v$sd, stats::sd(c(1, 3)))
  expect_equal(a_v$iqr, a_v$q3 - a_v$q1)
  b_v <- s[s$g %in% "b" & s$variable == "v", ]
  expect_equal(b_v$pct_missing, 100)
  expect_true(is.na(b_v$mean))
  expect_equal(s$n[is.na(s$g) & s$variable == "w"], 1)
  expect_true(is.na(s$sd[is.na(s$g) & s$variable == "w"]))

  o <- calc_grouped_summary(d, vars = "w", stats = c("n", "median"))
  expect_equal(names(o), c("variable", "n", "median"))
  expect_equal(o$median, 6)

  expect_error(calc_grouped_summary(d, vars = "g"), "Non-numeric")
  expect_error(calc_grouped_summary(d, vars = "zz"), "not in")
  expect_error(calc_grouped_summary(d, by = "zz"), "not in")
  expect_error(calc_grouped_summary(d, stats = "foo"), "Unknown")
  expect_error(calc_grouped_summary(d["g"]), "No numeric")
  expect_error(calc_grouped_summary(1), "data frame")
})
