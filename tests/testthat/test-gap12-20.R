test_that("tbl_contingency_2x2 matches fisher.test, chisq.test and hand-computed risks", {
  d <- data.frame(g = rep(c("a", "b"), c(30, 20)),
                  o = c(rep(c("yes", "no"), c(12, 18)), rep(c("yes", "no"), c(4, 16))))
  res <- tbl_contingency_2x2(d, "g", "o", test = "both")
  tab <- table(d$g, d$o)
  expect_equal(res$tests$p_value[1], stats::fisher.test(tab)$p.value)
  expect_equal(res$tests$p_value[2], stats::chisq.test(tab, correct = FALSE)$p.value)
  # first row level (a) is exposed, first column level (no) is the outcome
  a <- 18; b <- 12; c <- 16; dd <- 4
  expect_equal(res$risk$estimate[1], (a * dd) / (b * c))
  expect_equal(res$risk$estimate[2], (a / (a + b)) / (c / (c + dd)))
  expect_equal(res$table$no[1], "18 (60.0%)")
  expect_equal(nrow(res$table), 3L)
  expect_match(res$note, "^n = 50\\. Cells are n \\(row %\\)\\.")
})

test_that("tbl_contingency_2x2 corrects zero cells for risks and validates input", {
  d <- data.frame(g = rep(c("a", "b"), each = 10), o = rep(c("x", "y"), c(10, 10)))
  res <- tbl_contingency_2x2(d, "g", "o")
  expect_true(all(is.finite(res$risk$estimate)))
  expect_null(tbl_contingency_2x2(d, "g", "o", include_risk = FALSE)$risk)
  expect_error(tbl_contingency_2x2(d, "g", "nope"), "not a column")
  d$o3 <- rep(c("p", "q", "r", "p"), 5)
  expect_error(tbl_contingency_2x2(d, "g", "o3"), "exactly two")
})

test_that("pca_equation_latex formats loadings and escapes names", {
  fit <- prcomp(mtcars[, c("mpg", "hp", "wt")], scale. = TRUE)
  s <- pca_equation_latex(fit, 1, digits = 2)
  l <- round(fit$rotation[, 1], 2)
  expect_match(s, "^PC_\\{1\\} = ")
  expect_match(s, sprintf("%+.2f \\\\text\\{mpg\\}", l[["mpg"]]), fixed = FALSE)
  s2 <- pca_equation_latex(fit, "PC2", feature_names = c(hp = "horse_power"), max_terms = 2)
  expect_match(s2, "horse\\\\_power", fixed = FALSE)
  expect_match(s2, "\\\\dots$")
  expect_equal(lengths(regmatches(s2, gregexpr("\\\\text", s2))), 2L)
  expect_error(pca_equation_latex(fit, 4), "between 1 and 3")
  expect_error(pca_equation_latex(fit, 1, feature_names = c("a", "b")), "one entry per feature")
})

test_that("pca_component_cutoff returns the smallest k reaching the threshold", {
  fit <- prcomp(mtcars, scale. = TRUE)
  cum <- cumsum(fit$sdev^2) / sum(fit$sdev^2)
  k <- pca_component_cutoff(fit, 0.9)
  expect_equal(as.integer(k), which(cum >= 0.9)[1])
  expect_equal(as.integer(pca_component_cutoff(fit, 1)), length(cum))
  expect_error(pca_component_cutoff(fit, 0), "in \\(0, 1\\]")
  expect_error(pca_component_cutoff(prcomp(mtcars, scale. = TRUE, rank. = 1), 0.99), "do not reach")
})

test_that("piecewise LMM tunes a knot near the true change point and imputes", {
  skip_if_not_installed("lme4")
  set.seed(42)
  n <- 30
  d <- expand.grid(id = seq_len(n), time = 0:8)
  u0 <- rnorm(n, 0, 1); u1 <- rnorm(n, 0, 0.1)
  d$y <- 10 + u0[d$id] + (1 + u1[d$id]) * d$time - 3 * pmax(d$time - 4, 0) + rnorm(nrow(d), 0, 0.3)
  tuned <- tune_breakpoint_lmm(d, "y", "time", "id", knots = 2:6, metric = "AIC")
  expect_equal(tuned$knot[tuned$selected], 4)
  expect_equal(sum(tuned$selected), 1L)
  expect_true(all(c("AIC", "BIC", "RMSE", "status") %in% names(tuned)))

  truth <- d$y[c(10, 100, 200)]
  d$y[c(10, 100, 200)] <- NA
  imp <- impute_piecewise_lmm(d, "y", "time", "id", knot = 4)
  expect_equal(which(imp$y_was_imputed), c(10, 100, 200))
  expect_false(anyNA(imp$y_imputed))
  expect_lt(max(abs(imp$y_imputed[c(10, 100, 200)] - truth)), 2)
  expect_equal(attr(imp, "knot"), 4)
  expect_equal(imp$y_imputed[-c(10, 100, 200)], d$y[-c(10, 100, 200)])
  expect_error(tune_breakpoint_lmm(d, "nope", "time", "id"), "not a column")
})

test_that("classify_quadrants numbers quadrants and puts cut values on the upper side", {
  d <- data.frame(x = c(1, -1, -1, 1, 0, NA), y = c(1, 1, -1, -1, 0, 1))
  q <- classify_quadrants(d, "x", "y")
  expect_equal(as.character(q$quadrant), c("I", "II", "III", "IV", "I", NA))
  expect_equal(levels(q$quadrant), c("I", "II", "III", "IV"))
  expect_error(classify_quadrants(d, "x", "y", labels = c("a", "a", "b", "c")), "four distinct")
  expect_error(classify_quadrants(d, "x", "zz"), "not a column")
})

test_that("quadrant tables count strata and quadrant_n reads them", {
  d <- classify_quadrants(mtcars, "wt", "mpg", x_cut = 3.2, y_cut = 20)
  tq <- tbl_quadrant_distribution(d, strat_vars = c("am", "vs"))
  expect_equal(quadrant_n(tq, "I", "Total"), sum(d$quadrant == "I", na.rm = TRUE))
  d$st <- ifelse(d$am == 1, "manual", "auto")
  tq2 <- tbl_quadrant_distribution(d, strat_vars = "st")
  expect_equal(quadrant_n(tq2, "II", "manual"), sum(d$quadrant == "II" & d$am == 1))
  expect_equal(sum(tq2$n[tq2$variable == "st"]), nrow(d))
  expect_error(quadrant_n(tq, "I", "0"), "several variables")
  expect_error(quadrant_n(tq2, "V", "manual"), "Unknown quadrant")
  expect_error(quadrant_n(tq2, "I", "ghost"), "must be one of")
})

test_that("tbl_paired_assay_agreement matches hand-computed bias, limits, CCC and missingness", {
  x <- c(1, 2, 3, 4, 5, NA)
  y <- c(1.5, 2.1, 3.9, 4.0, 6.0, 7)
  d <- data.frame(id = c(1, 1, 2, 2, 3, 3), x = x, y = y)
  res <- tbl_paired_assay_agreement(d, data.frame(parameter = "p", method_1 = "x", method_2 = "y"),
                                    id_col = "id", digits = NULL)
  diff <- (x - y)[1:5]
  expect_equal(res$n_pairs, 5L)
  expect_equal(res$n_subjects, 3L)
  expect_equal(res$bias, mean(diff))
  expect_equal(res$sd_diff, sd(diff))
  expect_equal(res$loa_upper, mean(diff) + 1.96 * sd(diff))
  xx <- x[1:5]; yy <- y[1:5]
  ccc <- 2 * mean((xx - mean(xx)) * (yy - mean(yy))) /
    (mean((xx - mean(xx))^2) + mean((yy - mean(yy))^2) + (mean(xx) - mean(yy))^2)
  expect_equal(res$ccc, ccc)
  expect_equal(res$pct_miss_1, 100 / 6)
  expect_equal(res$pct_miss_2, 0)
  sub <- tbl_paired_assay_agreement(d, data.frame(parameter = "p", method_1 = "x", method_2 = "y"),
                                    metrics = c("bias", "ccc"))
  expect_equal(names(sub), c("parameter", "method_1", "method_2", "n_pairs", "bias", "ccc"))
  expect_error(tbl_paired_assay_agreement(d, data.frame(parameter = "p", method_1 = "x", method_2 = "zz")), "not a column")
  expect_error(tbl_paired_assay_agreement(d, data.frame(a = 1)), "must have columns")
})

test_that("tbl_correlated_pairs lists pairs at or above the cutoff with labels and caption", {
  set.seed(3)
  a <- rnorm(60)
  d <- data.frame(a = a, b = a + rnorm(60, 0, 0.1), c = rnorm(60), d = rnorm(60))
  res <- tbl_correlated_pairs(d, cutoff = 0.9, format = "tibble",
                              metadata = data.frame(columns = "a", labels = "Alpha"))
  expect_equal(nrow(res), 1L)
  expect_equal(res$label_1, "Alpha")
  expect_equal(res$label_2, "b")
  expect_equal(res$correlation, cor(d$a, d$b))
  expect_equal(attr(res, "caption"),
               "2 of the 4 features (50.0%) are at least 90% correlated with another feature (pearson).")
  none <- tbl_correlated_pairs(d, c("c", "d"), cutoff = 0.99, format = "tibble")
  expect_equal(nrow(none), 0L)
  expect_equal(attr(none, "n_involved"), 0L)
  skip_if_not_installed("knitr")
  expect_s3_class(tbl_correlated_pairs(d, cutoff = 0.9), "knitr_kable")
  expect_error(tbl_correlated_pairs(d, cutoff = 1.5, format = "tibble"), "in \\(0, 1\\]")
  expect_error(tbl_correlated_pairs(d, "a", format = "tibble"), "at least two")
})

test_that("inspect_rank_deficiency finds aliased lm terms and stays silent for full rank", {
  d <- data.frame(y = c(1, 2, 3, 5, 4, 6), a = 1:6, b = 2 * (1:6))
  expect_message(r <- inspect_rank_deficiency(lm(y ~ a + b, d)), "Rank-deficient")
  expect_true(r$rank_deficient)
  expect_equal(r$dropped, "b")
  expect_equal(r$rank, 2L)
  expect_silent(ok <- inspect_rank_deficiency(lm(y ~ a, d)))
  expect_false(ok$rank_deficient)
  expect_null(ok$message)
  expect_silent(inspect_rank_deficiency(lm(y ~ a + b, d), verbose = FALSE))
  expect_error(inspect_rank_deficiency("x"), "merMod, lm or glm")
})

test_that("inspect_rank_deficiency reads dropped columns of a mixed model", {
  skip_if_not_installed("lme4")
  set.seed(5)
  d <- data.frame(id = factor(rep(1:12, each = 4)), a = factor(rep(c("p", "q"), 24)),
                  b = factor(rep(c("u", "v"), each = 24)))
  d$y <- rnorm(48)
  d <- d[!(d$a == "q" & d$b == "v"), ] # empty cell: a * b has an unobserved combination
  fit <- suppressMessages(suppressWarnings(lme4::lmer(y ~ a * b + (1 | id), d)))
  r <- inspect_rank_deficiency(fit, verbose = FALSE)
  expect_true(r$rank_deficient)
  expect_length(r$dropped, 1L)
  expect_equal(r$n_columns - length(r$dropped), r$rank)
})

test_that("plot_imputation_hazard_stability filters Tukey outer fences", {
  set.seed(1)
  orig <- data.frame(term = rep(c("age", "bmi"), each = 100), estimate = rnorm(200, 0.2, 0.1))
  imp <- data.frame(term = rep(c("age", "bmi"), each = 100), estimate = rnorm(200, 0.25, 0.1))
  imp$estimate[1] <- 50 # optimisation artefact
  p <- plot_imputation_hazard_stability(orig, imp)
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 399L)
  expect_equal(attr(p, "n_removed")$estimate, 1L)
  p2 <- plot_imputation_hazard_stability(orig, imp, filter_fences = FALSE, exponentiate = FALSE)
  expect_equal(nrow(p2$data), 400L)
  expect_error(plot_imputation_hazard_stability(orig, data.frame(x = 1)), "must be a data frame")
})

test_that("tbl_tuning_by_quantile tiers by metric and counts term retention", {
  tr <- data.frame(IBS = c(0.10, 0.11, 0.20, 0.21), alpha = c(1, 0.5, 0.2, 0.1),
                   lambda = c(0.1, 0.2, 0.3, 0.4),
                   formula = c("~ a + b", "~ a", "~ b", "~ b + c"))
  s <- tbl_tuning_by_quantile(tr, n_tiles = 2)
  expect_equal(s$n_models, c(2L, 2L))
  expect_equal(s$metric_mean, c(0.105, 0.205))
  expect_equal(s$mean_lambda, c(0.15, 0.35))
  tm <- tbl_tuning_by_quantile(tr, n_tiles = 2, what = "terms")
  expect_equal(tm$retention[tm$tier == 1 & tm$term == "a"], 1)
  expect_equal(tm$retention[tm$tier == 2 & tm$term == "c"], 0.5)
  hi <- tbl_tuning_by_quantile(tr, n_tiles = 2, lower_is_better = FALSE)
  expect_equal(hi$metric_mean, c(0.205, 0.105))
  expect_error(tbl_tuning_by_quantile(tr, n_tiles = 10), "Fewer candidates")
  expect_error(tbl_tuning_by_quantile(tr, n_tiles = 1), "at least 2")
  expect_error(tbl_tuning_by_quantile(tr[, 1:3], n_tiles = 2, what = "terms"), "formula")
})
