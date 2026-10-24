lung_b <- function() {
  d <- survival::lung
  d$status <- d$status - 1
  d$sex_f <- factor(d$sex, labels = c("M", "F"))
  d$ecog_f <- factor(d$ph.ecog)
  d
}

test_that("batch_cox_single() matches single coxph fits and adjusts across features", {
  d <- lung_b()
  r <- batch_cox_single(d, "time", "status", c("age", "sex_f", "wt.loss"), parallel = FALSE)
  expect_s3_class(r, "cbe_batch_cox")
  expect_named(r, c("feature", "term", "estimate", "conf.low", "conf.high", "std.error", "statistic",
                    "p.value", "p_overall", "p_adj", "n", "n_events", "status"))
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  row <- r[r$feature == "age", ]
  expect_equal(row$estimate, unname(exp(coef(fit))))
  expect_equal(c(row$conf.low, row$conf.high), unname(exp(confint(fit))[1, ]))
  expect_equal(row$p.value, summary(fit)$coefficients[, "Pr(>|z|)"][[1]])
  expect_equal(row$n_events, fit$nevent)
  expect_equal(sort(unique(r$p_adj[!is.na(r$p_adj)])), sort(p.adjust(unique(r$p_overall), "BH")))
  expect_false(is.unsorted(r$p_adj, na.rm = TRUE))
})

test_that("factors give one row per non-reference level and one adjusted p per feature", {
  r <- batch_cox_single(lung_b(), "time", "status", c("ecog_f", "age"), parallel = FALSE)
  e <- r[r$feature == "ecog_f", ]
  expect_equal(nrow(e), 3L)
  expect_equal(e$term, paste0("ecog_f", c(1, 2, 3)))
  expect_equal(length(unique(e$p_adj)), 1L)
})

test_that("exponentiate = FALSE returns coefficients; adjust_method is used", {
  d <- lung_b()
  a <- batch_cox_single(d, "time", "status", "age", exponentiate = FALSE, parallel = FALSE)
  b <- batch_cox_single(d, "time", "status", "age", parallel = FALSE)
  expect_equal(a$estimate, log(b$estimate))
  bonf <- batch_cox_single(d, "time", "status", c("age", "sex_f"), adjust_method = "bonferroni", parallel = FALSE)
  expect_equal(bonf$p_adj, pmin(1, bonf$p_overall * 2))
  none <- batch_cox_single(d, "time", "status", c("age", "sex_f"), adjust_method = "none", parallel = FALSE)
  expect_equal(none$p_adj, none$p_overall)
})

test_that("a failing feature is reported, not fatal, and left out of the adjustment", {
  d <- lung_b()
  d$const <- 1
  r <- batch_cox_single(d, "time", "status", c("age", "const", "sex_f"), parallel = FALSE)
  bad <- r[r$feature == "const", ]
  expect_equal(bad$status, "feature has a single value")
  expect_true(is.na(bad$p_adj))
  good <- r[r$feature != "const", ]
  expect_equal(sort(unique(good$p_adj)), sort(p.adjust(unique(good$p_overall), "BH")))
  expect_equal(r$feature[nrow(r)], "const") # failures last
})

test_that("start-stop data are supported", {
  d <- survival::heart
  r <- batch_cox_single(d, time = "stop", status = "event", features = c("age", "surgery"), time_start = "start", parallel = FALSE)
  fit <- survival::coxph(survival::Surv(start, stop, event) ~ age, data = d)
  expect_equal(r$estimate[r$feature == "age"], unname(exp(coef(fit))))
})

test_that("parallel = TRUE gives the same answer", {
  skip_if_not_installed("furrr")
  d <- lung_b()
  a <- batch_cox_single(d, "time", "status", c("age", "sex_f", "wt.loss"), parallel = FALSE)
  b <- batch_cox_single(d, "time", "status", c("age", "sex_f", "wt.loss"), parallel = TRUE)
  expect_equal(as.data.frame(a), as.data.frame(b))
})

test_that("batch_cox_single() validates its input", {
  d <- lung_b()
  expect_error(batch_cox_single(1, features = "a"), "data frame")
  expect_error(batch_cox_single(d, "nope", "status", "age"), "`time` must name")
  expect_error(batch_cox_single(d, "time", "status", character()), "at least one")
  expect_error(batch_cox_single(d, "time", "status", "zzz"), "not in `data`")
  expect_error(batch_cox_single(d, "time", "status", "age", adjust_method = "x"), "adjust_method")
  expect_error(batch_cox_single(d, "time", "status", c("age", "time")), "must not include")
  expect_error(batch_cox_single(d, "time", "age", "wt.loss"), "0/1")
  expect_error(batch_cox_single(d, "time", "status", "age", conf_level = 2), "conf_level")
})
