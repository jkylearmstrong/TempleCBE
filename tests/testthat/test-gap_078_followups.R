# tbl_fine_gray_single(), glance_imputations(), zero-variance pre-filter of the formula samplers

fg_data <- function(n = 300, seed = 11) {
  set.seed(seed)
  data.frame(
    time = rexp(n, 0.1),
    type = sample(0:2, n, TRUE, prob = c(0.3, 0.4, 0.3)),
    age = rnorm(n, 60, 8),
    arm = sample(c("A", "B", "C"), n, TRUE)
  )
}

test_that("tbl_fine_gray_single matches cmprsk::crr on a numeric predictor", {
  skip_if_not_installed("cmprsk")
  d <- fg_data()
  r <- tbl_fine_gray_single(d, "time", "type", "age")
  ref <- cmprsk::crr(d$time, d$type, cov1 = matrix(d$age, ncol = 1), failcode = 1, cencode = 0)
  expect_equal(r$estimate, exp(unname(ref$coef)))
  expect_equal(r$std.error, unname(sqrt(diag(ref$var))))
  expect_equal(r$conf.low, exp(unname(ref$coef) - qnorm(0.975) * sqrt(ref$var[1, 1])))
  expect_equal(r$n, nrow(d))
  expect_equal(r$n_event, sum(d$type == 1))
  expect_equal(tbl_fine_gray_single(d, "time", "type", "age", exponentiate = FALSE)$estimate,
               unname(ref$coef))
})

test_that("tbl_fine_gray_single gives one row per non-reference level and honours failcode", {
  skip_if_not_installed("cmprsk")
  d <- fg_data()
  r <- tbl_fine_gray_single(d, "time", "type", "arm", failcode = 2)
  expect_equal(r$term, c("armB", "armC"))
  expect_equal(unique(r$n_event), sum(d$type == 2))
  d$age[1:5] <- NA
  expect_equal(tbl_fine_gray_single(d, "time", "type", "age")$n, nrow(d) - 5L)
})

test_that("tbl_fine_gray_single validates its input", {
  skip_if_not_installed("cmprsk")
  d <- fg_data()
  expect_error(tbl_fine_gray_single(1, "time", "type", "age"), "data frame")
  expect_error(tbl_fine_gray_single(d, "nope", "type", "age"), "not a column")
  expect_error(tbl_fine_gray_single(d, "type", "type", "age", failcode = 9), "failcode")
  expect_error(tbl_fine_gray_single(d, "time", "type", "age", failcode = 0), "different")
  expect_error(tbl_fine_gray_single(d, "time", "type", "age", cencode = 7), "cencode")
  expect_error(tbl_fine_gray_single(d, "time", "type", "age", conf_level = 1), "conf_level")
  d$flat <- 1
  expect_error(tbl_fine_gray_single(d, "time", "type", "flat"), "zero variance")
  d$one <- "a"
  expect_error(tbl_fine_gray_single(d, "time", "type", "one"), "fewer than two")
  d$time <- as.character(d$time)
  expect_error(tbl_fine_gray_single(d, "time", "type", "age"), "numeric")
})

test_that("glance_imputations pools means and uses Rubin's rules when a standard error exists", {
  g <- lapply(c(0.70, 0.72, 0.74), function(cc) {
    data.frame(concordance = cc, std.error.concordance = 0.03, brier = cc / 10, df = 3)
  })
  p <- glance_imputations(g)
  expect_equal(p$statistic, c("concordance", "brier", "df"))
  expect_equal(p$estimate, c(0.72, 0.072, 3))
  expect_equal(p$between_sd[3], 0)
  expect_equal(p$min[1], 0.70)
  expect_equal(p$max[1], 0.74)
  ref <- pool_imputations(lapply(g, function(d) data.frame(term = "c", estimate = d$concordance,
                                                           std.error = d$std.error.concordance)))
  expect_equal(p$std.error[1], ref$std.error)
  expect_equal(p$fmi[1], ref$fmi)
  expect_true(all(is.na(p$std.error[-1])))

  stacked <- do.call(rbind, Map(function(d, i) cbind(imputation = i, d), g, 1:3))
  expect_equal(glance_imputations(stacked)$estimate, p$estimate)
})

test_that("glance_imputations validates its input", {
  g <- list(data.frame(a = 1), data.frame(a = 2))
  expect_error(glance_imputations(g[1]), "two imputations")
  expect_error(glance_imputations(data.frame(a = 1:2)), "imputation")
  expect_error(glance_imputations(list(data.frame(a = 1:2), data.frame(a = 1))), "one-row")
  expect_error(glance_imputations(list(data.frame(a = 1), data.frame(b = 1))), "same columns")
  expect_error(glance_imputations(list(data.frame(a = "x"), data.frame(a = "y"))), "No numeric")
  expect_error(glance_imputations(list(data.frame(a = 1), data.frame(a = NA_real_))), "missing")
})

test_that("zero-variance terms are kept out of sampled formulas", {
  d <- data.frame(a = rnorm(20), b = 1, c = rnorm(20), e = c(rep("x", 20)), f = rnorm(20))
  expect_message(
    s <- sample_candidate_formulas(c("a", "b", "c", "e", "f"), n = 5, min_size = 1, max_size = 3,
                                   seed = 1, data = d),
    "b, e"
  )
  expect_false(any(vapply(s$terms, function(t) any(c("b", "e") %in% t), logical(1))))
  expect_equal(attr(s, "degenerate"), c("b", "e"))
  # a required degenerate term stays
  s2 <- suppressMessages(sample_candidate_formulas(c("a", "b", "c"), required = "b", n = 3, seed = 1, data = d))
  expect_true(all(vapply(s2$terms, function(t) "b" %in% t, logical(1))))
  # no data: nothing is checked
  expect_length(attr(sample_candidate_formulas(c("a", "b"), n = 2, seed = 1), "degenerate"), 0L)
  expect_error(sample_candidate_formulas(c("a", "b"), data = 1), "data frame")
})

test_that("sample_weighted_formulas and the adaptive sampler drop zero-variance features", {
  d <- data.frame(a = rnorm(20), b = 1, c = rnorm(20), f = rnorm(20))
  w <- c(a = 1, b = 5, c = 1, f = 1)
  expect_message(s <- sample_weighted_formulas(w, n_samples = 3, min_terms = 2, max_terms = 3, seed = 1, data = d),
                 "Zero-variance")
  expect_false(any(vapply(s$terms, function(t) "b" %in% t, logical(1))))
  expect_error(suppressMessages(sample_weighted_formulas(c(b = 1), data = d)), "zero variance")

  prior <- data.frame(formula = c("y ~ a + b", "y ~ a + c", "y ~ b + c", "y ~ f"), IBS = c(0.1, 0.12, 0.2, 0.3))
  expect_message(out <- sample_candidate_formulas_adaptive(prior, top_quantile = 0.75, n_formulas = 1, seed = 1, data = d),
                 "Zero-variance")
  expect_false("b" %in% attr(out, "adaptive")$pool)
})
