# Downstream requests for 0.7.10: extensions of existing functions and the new plot/table helpers.

surv_data <- function(n = 150, seed = 1) {
  set.seed(seed)
  x <- rnorm(n)
  data.frame(
    time = rexp(n, 0.1 * exp(0.8 * x)),
    status = rbinom(n, 1, 0.8),
    marker = x,
    grp = sample(c("A", "B"), n, replace = TRUE)
  )
}

tune_table <- function(seed = 1) {
  set.seed(seed)
  grid <- expand.grid(inner_resamples_splits = paste0("Fold", 1:4), alpha = c(0.1, 0.5, 1),
                      lambda = c(0.01, 0.05, 0.1), stringsAsFactors = FALSE)
  grid$IBS <- 0.15 + 0.03 * grid$alpha + 0.2 * grid$lambda + rnorm(nrow(grid), 0, 0.005)
  out <- merge(grid, data.frame(term = c("age", "bmi", "(Intercept)"), stringsAsFactors = FALSE))
  out$estimate <- rnorm(nrow(out), 0.3, 0.1)
  out
}

# ---- plot_cox_forest ----

test_that("plot_cox_forest colors and shapes by p-value tier and takes the logical shortcuts", {
  td <- data.frame(index_label = c("a", "b", "c", "d"), estimate = c(1.1, 3, 0.2, 1.5),
                   conf.low = c(0.9, 2, 0.1, 0.8), conf.high = c(1.3, 4.5, 0.4, 2.8),
                   p.value = c(0.4, 0.0001, 0.004, NA))
  p <- plot_cox_forest(td, p_col = "p.value", color_by = "p_tier")
  b <- ggplot2::ggplot_build(p)
  tier <- as.character(p$data$.tier)
  expect_equal(tier[match(c("a", "b", "c", "d"), p$data$.label)],
               c("p \u2265 0.05", "p < 0.001", "p < 0.01", "p \u2265 0.05"))
  expect_true(all(c("colour", "shape") %in% names(b$data[[3]])))
  expect_length(unique(b$data[[3]]$shape), 3L)

  p2 <- plot_cox_forest(td, p_col = "p.value", color_by_sig = TRUE, order_by_magnitude = TRUE, exponentiate = FALSE)
  expect_true(".tier" %in% names(p2$data))
  # sorted by |log HR|, largest first: c (log 0.2), b (log 3), d, a
  expect_equal(p2$data$.label, c("c", "b", "d", "a"))
  expect_equal(p2$layers[[1]]$data$xintercept %||% p2$layers[[1]]$aes_params$xintercept, 0)

  p3 <- plot_cox_forest(td, p_col = "p.value", color_by = "significance", color_by_sig = FALSE)
  expect_false(".sig" %in% names(p3$data))
  expect_error(plot_cox_forest(td, color_by = "p_tier"), "requires `p_col`")
  expect_error(plot_cox_forest(td, p_col = "p.value", color_by = "p_tier", p_breaks = c(0.05, 0.01)), "p_breaks")
  expect_error(plot_cox_forest(td, color_by_sig = "yes"), "TRUE, FALSE or NULL")
})

# ---- plot_survival_km ----

test_that("plot_survival_km takes time_breaks and counting-process data", {
  d <- surv_data()
  p <- plot_survival_km(d, "time", "status", group_col = "grp", time_breaks = c(0, 10, 20), risk_table = TRUE)
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$layout$panel_params[[1]]$x$breaks[!is.na(b$layout$panel_params[[1]]$x$breaks)], c(0, 10, 20))
  expect_error(plot_survival_km(d, "time", "status", time_breaks = c(10, 5)), "increasing")

  # each subject split into two intervals at t = 2: the same curve as the unsplit data
  long <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    if (r$time <= 2) return(cbind(r, start = 0, stop = r$time, ev = r$status))
    rbind(cbind(r, start = 0, stop = 2, ev = 0), cbind(r, start = 2, stop = r$time, ev = r$status))
  }))
  p_cp <- plot_survival_km(long, "stop", "ev", group_col = "grp", start_col = "start", p_value = TRUE)
  p_plain <- plot_survival_km(d, "time", "status", group_col = "grp")
  est <- function(p) {
    x <- ggplot2::ggplot_build(p)$data[[1]]
    x <- x[order(x$group, x$x), ]
    tapply(x$y, x$group, min)
  }
  expect_equal(unname(est(p_cp)), unname(est(p_plain)), tolerance = 1e-8)
  labs <- unname(unlist(lapply(p_cp$layers, function(l) l$aes_params$label)))
  expect_match(labs, "^p [=<] ")
  # with fixed groups the Cox score test is the log-rank test
  sd <- survival::survdiff(survival::Surv(time, status) ~ grp, data = d)
  expect_equal(labs, paste0("p = ", formatC(sd$pvalue, format = "f", digits = 3)))
  expect_error(plot_survival_km(long, "stop", "ev", start_col = "nope"), "not a column")
})

# ---- cutpoints ----

test_that("km_cutpoint_search reports the selection criteria", {
  d <- surv_data()
  res <- km_cutpoint_search(d, "time", "status", "marker")
  cr <- res$criteria
  expect_s3_class(cr, "tbl_df")
  expect_true(all(c("median", "max_logrank", "lowest_p") %in% cr$criterion))
  med <- cr[cr$criterion == "median", ]
  expect_equal(med$cutpoint, stats::median(d$marker))
  expect_equal(med$n_low + med$n_high, nrow(d))
  best <- cr[cr$criterion == "max_logrank", ]
  expect_equal(best$cutpoint, res$optimal_cutpoint)
  expect_equal(best$p_value, res$optimal_p)
  expect_equal(cr$p_value[cr$criterion == "lowest_p"], min(res$candidates$p_value))
  # the marker raises the hazard, so the first significant cutpoint is the lowest candidate below alpha
  expect_gt(res$cox_coef, 0)
  first <- cr[cr$criterion == "first_p<0.05", ]
  sig <- res$candidates[res$candidates$p_value < 0.05, ]
  expect_equal(first$cutpoint, min(sig$cutpoint))
  expect_equal(cr$neg_log10_p, -log10(pmax(cr$p_value, 1e-16)))

  # the median row agrees with a direct log-rank test
  d$hi <- d$marker > stats::median(d$marker)
  sd <- survival::survdiff(survival::Surv(time, status) ~ hi, data = d)
  expect_equal(med$chisq, sd$chisq)

  mid <- km_cutpoint_search(d, "time", "status", "marker", cutpoints = "midpoints", alpha = NULL)
  expect_gt(nrow(mid$candidates), nrow(res$candidates))
  expect_false(any(grepl("^first_p", mid$criteria$criterion)))
  expect_error(km_cutpoint_search(d, "time", "status", "marker", alpha = 2), "alpha")
})

test_that("a protective marker is scanned from the highest cutpoint", {
  d <- surv_data()
  d$marker <- -d$marker
  res <- km_cutpoint_search(d, "time", "status", "marker")
  expect_lt(res$cox_coef, 0)
  first <- res$criteria[res$criteria$criterion == "first_p<0.05", ]
  sig <- res$candidates[res$candidates$p_value < 0.05, ]
  expect_equal(first$cutpoint, max(sig$cutpoint))
})

test_that("the maxstat criterion matches maxstat when it is installed", {
  skip_if_not_installed("maxstat")
  d <- surv_data()
  res <- km_cutpoint_search(d, "time", "status", "marker")
  mt <- maxstat::maxstat.test(survival::Surv(time, status) ~ marker, data = d, smethod = "LogRank",
                              pmethod = "none", minprop = 0.1, maxprop = 0.9)
  expect_equal(res$criteria$cutpoint[res$criteria$criterion == "maxstat"], unname(mt$estimate))
})

test_that("cutpoint plots work for one result, a list and time-varying data", {
  d <- surv_data()
  res <- km_cutpoint_search(d, "time", "status", "marker")
  expect_s3_class(plot_km_cutpoints(res), "ggplot")
  expect_s3_class(plot_km_cutpoints(res, alpha = NULL), "ggplot")
  bar <- plot_cutpoint_bar(res)
  expect_equal(nrow(bar$data), nrow(res$criteria))
  expect_s3_class(plot_cutpoint_bar(res, metric = "hr"), "ggplot")
  expect_s3_class(plot_cutpoint_scatter(res), "ggplot")
  d$z <- rnorm(nrow(d))
  two <- list(marker = res, z = km_cutpoint_search(d, "time", "status", "z"))
  both <- plot_cutpoint_bar(two)
  expect_equal(levels(both$data$feature), c("marker", "z"))
  expect_error(plot_cutpoint_bar(list(1)), "km_cutpoint_search")
  expect_error(plot_cutpoint_bar(data.frame(a = 1)), "lacks column")

  km <- plot_km_cutpoint(res, criterion = "median", risk_table = FALSE)
  expect_match(km$labels$subtitle, "median cutpoint")
  expect_s3_class(plot_km_cutpoint(res, criterion = 0.25), "ggplot")
  expect_error(plot_km_cutpoint(res, criterion = "nope"), "not in")

  long <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    r$id <- i
    if (r$time <= 2) return(cbind(r, start = 0, stop = r$time, ev = r$status))
    rbind(cbind(r, start = 0, stop = 2, ev = 0), cbind(r, start = 2, stop = r$time, ev = r$status))
  }))
  tv <- km_cutpoint_time_varying(long, "start", "stop", "ev", "marker", "id")
  expect_true(all(c("median", "max_logrank") %in% tv$criteria$criterion))
  expect_false("maxstat" %in% tv$criteria$criterion)
  expect_equal(tv$start_col, "start")
  expect_s3_class(plot_km_cutpoint(tv, risk_table = FALSE), "ggplot")
})

# ---- tuning diagnostics ----

test_that("plot_tune_coefficients shows the grid and its best share", {
  tune <- tune_table()
  p <- plot_tune_coefficients(tune, top_frac = 0.2)
  expect_false("(Intercept)" %in% p$data$term)
  cut <- stats::quantile(unique(tune$IBS), 0.2, names = FALSE)
  best <- p$data[p$data$.set != "All models", ]
  expect_true(all(best$IBS <= cut))
  expect_equal(nrow(best), sum(tune$IBS <= cut & tune$term != "(Intercept)"))
  expect_equal(nrow(plot_tune_coefficients(tune, top_frac = NULL, terms = "age")$data), 36L)
  expect_error(plot_tune_coefficients(tune, top_frac = 1.5), "top_frac")
  expect_error(plot_tune_coefficients(tune, terms = "zzz"), "not in")
  expect_error(plot_tune_coefficients(tune[, c("term", "IBS")]), "estimate")
})

test_that("plot_tune_grid and plot_tune_ibs_density build", {
  skip_if_not_installed("patchwork")
  tune <- tune_table()
  g <- plot_tune_grid(tune, title = "Grid")
  expect_s3_class(g, "patchwork")
  expect_length(g$patches$plots, 3L)
  tune$lambda[1] <- 0
  expect_s3_class(plot_tune_grid(tune), "patchwork")

  d <- plot_tune_ibs_density(tune_table())
  expect_equal(nrow(d$data), 36L)
  expect_s3_class(plot_tune_ibs_density(tune_table(), facet = TRUE, log_ibs = TRUE), "ggplot")
  expect_s3_class(plot_tune_ibs_density(data.frame(IBS = runif(10))), "ggplot")
  expect_error(plot_tune_ibs_density(data.frame(x = 1)), "IBS")
})

test_that("tbl_tune_mixed_model returns the fixed effects of the lmer fit", {
  skip_if_not_installed("lme4")
  tune <- tune_table()
  tab <- tbl_tune_mixed_model(tune)
  expect_equal(tab$term, c("(Intercept)", "alpha", "log10_lambda", "alpha:log10_lambda"))
  fits <- unique(tune[c("inner_resamples_splits", "alpha", "lambda", "IBS")])
  fits$log10_lambda <- log10(fits$lambda)
  ref <- suppressMessages(lme4::lmer(IBS ~ alpha * log10_lambda + (1 | inner_resamples_splits), data = fits))
  expect_equal(tab$estimate, unname(lme4::fixef(ref)), tolerance = 1e-8)
  expect_equal(tab$conf.high - tab$conf.low, 2 * stats::qnorm(0.975) * tab$std.error, tolerance = 1e-10)
  expect_true(tab$p.value[tab$term == "alpha"] < 0.05)
  expect_s4_class(attr(tab, "model"), "merMod")

  add <- tbl_tune_mixed_model(tune, interaction = FALSE, log_ibs = TRUE)
  expect_equal(nrow(add), 3L)
  one <- tune[tune$inner_resamples_splits == "Fold1", ]
  expect_equal(attr(tbl_tune_mixed_model(one), "method"), "lm")
  expect_error(tbl_tune_mixed_model(tune[tune$alpha == 0.5, ]), "at least two values")
})

# ---- paired comparisons ----

paired_data <- function() {
  set.seed(2)
  d <- data.frame(id = rep(1:30, 3), time = rep(c(0, 6, 12), each = 30), original = rnorm(90, 50, 8))
  d$imputed <- d$original + rnorm(90, 0.5, 2)
  d
}

test_that("paired_diff_tests agrees with t.test and wilcox.test", {
  d <- paired_data()
  res <- paired_diff_tests(d, "original", "imputed")
  tt <- stats::t.test(d$imputed, d$original, paired = TRUE)
  expect_equal(res$mean_diff, unname(tt$estimate))
  expect_equal(res$p_t, tt$p.value)
  expect_equal(c(res$conf.low, res$conf.high), as.numeric(tt$conf.int))
  expect_equal(res$p_wilcoxon, stats::wilcox.test(d$imputed, d$original, paired = TRUE)$p.value)

  by <- paired_diff_tests(d, "original", "imputed", group_col = "time")
  expect_equal(by$group, c(0, 6, 12))
  expect_equal(sum(by$n), 90L)
  d6 <- d[d$time == 6, ]
  expect_equal(by$p_t[2], stats::t.test(d6$imputed - d6$original)$p.value)

  same <- data.frame(a = 1:5, b = 1:5)
  z <- paired_diff_tests(same, "a", "b")
  expect_equal(z$mean_diff, 0)
  expect_true(is.na(z$p_t) && is.na(z$p_wilcoxon))
  expect_error(paired_diff_tests(d, "original", "nope"), "not in `data`")
  expect_error(paired_diff_tests(data.frame(a = "x", b = 1), "a", "b"), "numeric")
})

test_that("the paired plots build and drop incomplete pairs", {
  d <- paired_data()
  d$imputed[1] <- NA
  h <- paired_diff_histogram(d, "original", "imputed")
  expect_equal(nrow(h$data), 89L)
  expect_s3_class(paired_diff_histogram(d, "original", "imputed", group_col = "time"), "ggplot")
  r <- paired_diff_ribbon_by_group(d, "original", "imputed", group_col = "time")
  expect_equal(r$data$x, c(0, 6, 12))
  v <- (d$imputed - d$original)[d$time == 6]
  expect_equal(r$data$center[2], mean(v))
  expect_equal(r$data$hi[2] - r$data$lo[2], 2 * stats::qt(0.975, 29) * stats::sd(v) / sqrt(30))
  q <- paired_diff_ribbon_by_group(d, "original", "imputed", group_col = "time", ribbon = "quantile")
  expect_equal(q$data$center[2], stats::median(v))
  d$visit <- factor(paste0("V", d$time), levels = c("V0", "V6", "V12"))
  f <- paired_diff_ribbon_by_group(d, "original", "imputed", group_col = "visit", ribbon = "sd")
  expect_equal(f$data$x, 1:3)
  expect_s3_class(paired_diff_ridgeline(d, "original", "imputed", group_col = "visit"), "ggplot")
  expect_error(paired_diff_ribbon_by_group(d, "original", "imputed"), "group_col")
})

test_that("compare_paired_correlation reports the fitted line", {
  d <- paired_data()
  p <- compare_paired_correlation(d, "original", "imputed")
  fit <- stats::lm(imputed ~ original, data = d)
  expect_equal(unname(stats::coef(attr(p, "model"))), unname(stats::coef(fit)))
  expect_match(p$labels$caption, "Pearson r = 0\\.9")
  expect_equal(unname(attr(p, "cor_test")$estimate), stats::cor(d$original, d$imputed))
  skip_if_not_installed("lme4")
  set.seed(3)
  d$imputed <- d$imputed + rep(rnorm(30, 0, 3), 3)
  m <- compare_paired_correlation(d, "original", "imputed", id_col = "id")
  expect_s4_class(attr(m, "model"), "merMod")
  expect_match(m$labels$caption, "mixed model")
})

test_that("compare_distributions_ks attaches the KS test", {
  skip_if_not_installed("patchwork")
  set.seed(4)
  x <- rnorm(80)
  y <- c(rnorm(60, 0.8), NA)
  p <- compare_distributions_ks(x, y, title = "x")
  expect_s3_class(p, "patchwork")
  ks <- stats::ks.test(x, y[!is.na(y)])
  expect_equal(attr(p, "ks")$p.value, ks$p.value)
  expect_match(p$patches$annotation$caption, "n = 80 and 60")
  expect_error(compare_distributions_ks(1, 1:3), "at least two")
  expect_error(compare_distributions_ks(x, y, labels = c("a", "a")), "different")
})

# ---- labels, glossary, contrasts ----

test_that("pretty_name looks names up in a dictionary in three forms", {
  dict <- data.frame(
    term = c("liv_rl", "thb", "age", "ph"),
    short = c("LIV (%RL)", "tHb", "Age", NA),
    full = c("Lung injury volume", "Total hemoglobin (g/dL)", NA, "Arterial pH"),
    unit = c("% right lung", "g/dL", "years", NA),
    domain = c("Imaging", "Blood", "Demo", "Blood"),
    stringsAsFactors = FALSE
  )
  x <- c("liv_rl", "thb", "age", "ph", "heart_rate", NA)
  expect_equal(pretty_name(x, dict = dict), c("LIV (%RL)", "tHb", "Age", "Arterial pH", "Heart Rate", NA))
  expect_equal(pretty_name(x, dict = dict, form = "full"),
               c("Lung injury volume", "Total hemoglobin (g/dL)", "Age", "Arterial pH", "Heart Rate", NA))
  # the unit is not repeated when the label already ends with it
  expect_equal(pretty_name(x, dict = dict, form = "unit"),
               c("Lung injury volume (% right lung)", "Total hemoglobin (g/dL)", "Age (years)", "Arterial pH",
                 "Heart Rate", NA))
  expect_equal(pretty_name(c("a", "body_mass"), dict = c(a = "Alpha")), c("Alpha", "Body Mass"))
  expect_equal(pretty_name("patient_bmi"), "Patient BMI")
  expect_error(pretty_name("a", dict = data.frame(term = c("a", "a"), short = c("x", "y"))), "duplicated")
  expect_error(pretty_name("a", dict = data.frame(term = "a", other = "x")), "short-label")

  g <- tbl_abbreviations_glossary(dict)
  expect_equal(g$Abbreviation, c("tHb", "LIV (%RL)"))
  expect_equal(names(g), c("Abbreviation", "Definition", "Unit", "Domain"))
  all_rows <- tbl_abbreviations_glossary(dict, only_abbreviations = FALSE, include_term = TRUE)
  expect_equal(nrow(all_rows), 4L)
  expect_equal(tbl_abbreviations_glossary(dict, domains = "Imaging")$Abbreviation, "LIV (%RL)")
  expect_error(tbl_abbreviations_glossary(dict, domains = "Nope"), "not in `dict`")
})

test_that("format_contrast_labels names the reference level", {
  d <- data.frame(head_shape = factor(c("Flathead", "Hemis", "Round top")), orientation = c("Spine", "Chestwall", "Spine"),
                  smoker = c(TRUE, FALSE, TRUE), age = c(40, 50, 60), ag = factor(c("e", "f", "e")))
  out <- format_contrast_labels(
    c("head_shapeHemis", "head_shape_Round.top", "orientation_Spine", "smokerTRUE", "age", "head_shapeHemis:age", NA),
    data = d
  )
  expect_equal(out, c("Head Shape: Hemis (relative to Flathead)", "Head Shape: Round top (relative to Flathead)",
                      "Orientation: Spine (relative to Chestwall)", "Smoker: TRUE (relative to FALSE)", "age",
                      "Head Shape: Hemis (relative to Flathead) x age", NA))
  # `age` is not a level of the factor `ag`
  expect_equal(format_contrast_labels("age", data = d), "age")
  ref <- format_contrast_labels(c("armB", "sex_M"), reference = list(arm = c("A", "B")), labels = c(arm = "Treatment arm"))
  expect_equal(ref, c("Treatment arm: B (relative to A)", "sex_M"))
  attr(d$head_shape, "label") <- "Implant head"
  expect_equal(format_contrast_labels("head_shapeHemis", data = d, template = "{variable} = {level} vs {reference}"),
               "Implant head = Hemis vs Flathead")
  expect_error(format_contrast_labels("a"), "`data` or `reference`")
  expect_error(format_contrast_labels("a", reference = list(a = "x")), "at least one other level")

  # agrees with the coefficient names of a fitted model
  fit <- stats::lm(mpg ~ factor(cyl) + wt, data = transform(mtcars, cyl = factor(cyl)))
  m <- stats::lm(mpg ~ cyl + wt, data = transform(mtcars, cyl = factor(cyl)))
  expect_equal(format_contrast_labels(names(stats::coef(m)), data = transform(mtcars, cyl = factor(cyl))),
               c("(Intercept)", "Cyl: 6 (relative to 4)", "Cyl: 8 (relative to 4)", "wt"))
})

test_that("sanitize_model_table fixes pipes, long decimals and DF headers", {
  tab <- data.frame(
    term = c("sd__(Intercept) (1 &#124; subject_id)", "x &124; y"),
    contrast = factor(c("time40.4573170731707", "time = 40.4573170731707, dose2.5")),
    NumDF = c(1, 2), DenDF = c(38.2123456, 40.1), t.ratio = c(1.23456789, 2),
    stringsAsFactors = FALSE
  )
  attr(tab, "mesg") <- "Results are averaged at age = 61.2857142857143"
  out <- sanitize_model_table(tab)
  expect_equal(out$term, c("sd__(Intercept) (1 | subject_id)", "x | y"))
  expect_equal(as.character(out$contrast), c("time = 40.5", "time = 40.5, dose2.5"))
  expect_equal(names(out), c("term", "contrast", "Num DF", "Den DF", "t ratio"))
  expect_equal(out$`Den DF`, tab$DenDF)
  expect_equal(attr(out, "mesg"), "Results are averaged at age = 61.3")
  expect_match(attr(out, "footnotes"), "numerator degrees of freedom")
  expect_equal(sanitize_model_table("x1 = 2.123456 and y = -0.98765", digits = 2), "x1 = 2.12 and y = -0.99")
  expect_equal(sanitize_model_table("group A - group B, p = 0.012"), "group A - group B, p = 0.012")
  expect_equal(names(sanitize_model_table(tab, rename_df = FALSE))[3:4], c("NumDF", "DenDF"))

  d <- data.frame(y = rnorm(10), a = 1:10)
  d$b <- 2 * d$a
  fit <- stats::lm(y ~ a + b, data = d)
  with_note <- sanitize_model_table(data.frame(term = "a", estimate = 1), model = fit)
  expect_match(attr(with_note, "footnotes"), "rank deficiency.*: b\\.")
  expect_length(attr(sanitize_model_table(data.frame(term = "a")), "footnotes"), 0L)
  expect_error(sanitize_model_table(1:3), "data frame or a character")
})

test_that("excel_col_letters and rename_to_excel", {
  expect_equal(excel_col_letters(c(1, 26, 27, 52, 53, 702, 703, NA)), c("A", "Z", "AA", "AZ", "BA", "ZZ", "AAA", NA))
  expect_equal(excel_col_letters(16384), "XFD")
  expect_error(excel_col_letters(0), "positive whole")
  expect_error(excel_col_letters(1.5), "positive whole")
  df <- rename_to_excel(data.frame(id = 1, `Blood gas` = 2, check.names = FALSE))
  expect_equal(names(df), c("A", "B"))
  expect_equal(attr(df, "original_names"), c("id", "Blood gas"))
})

# ---- PCA accessors ----

test_that("pca accessors format for prose and give loading ranges", {
  fit <- stats::prcomp(mtcars[, 1:5], scale. = TRUE)
  expect_equal(pca_loading(fit, "mpg", 1, digits = 2), formatC(fit$rotation["mpg", 1], format = "f", digits = 2))
  expect_type(pca_loading(fit, "mpg", 1), "double")
  expect_equal(pca_variance_pct(fit, 1:2, digits = 1),
               formatC(100 * fit$sdev[1:2]^2 / sum(fit$sdev^2), format = "f", digits = 1))
  v <- fit$rotation[c("cyl", "disp", "hp"), 1]
  expect_equal(pca_loading_range(fit, c("cyl", "disp", "hp"), 1),
               paste(formatC(range(v), format = "f", digits = 2), collapse = " to "))
  expect_equal(pca_loading_range(fit, c("mpg", "cyl"), "PC1", digits = NULL, absolute = TRUE),
               range(abs(fit$rotation[c("mpg", "cyl"), 1])))
  expect_error(pca_loading_range(fit, c("mpg", "zzz"), 1), "not in the PCA")
  expect_error(pca_loading(fit, "mpg", 1, digits = -1), "digits")
})

# ---- fit_lmer_safely ----

test_that("fit_lmer_safely reports convergence and uses the fallback formula", {
  skip_if_not_installed("lme4")
  set.seed(5)
  d <- data.frame(id = rep(1:12, each = 5), x = rep(1:5, 12))
  d$y <- 1 + 0.5 * d$x + rep(rnorm(12, 0, 2), each = 5) + rnorm(60)
  ok <- fit_lmer_safely(y ~ x + (1 | id), data = d)
  expect_equal(ok$method, "lmer")
  expect_true(ok$converged)
  expect_equal(ok$used_fallback, "none")
  expect_equal(ok$optimizer, "default")

  # no between-subject variance: the random intercept is singular
  d$z <- rnorm(60)
  expect_warning(sing <- fit_lmer_safely(z ~ x + (x | id), data = d, fallback_formula = z ~ x), "singular")
  expect_equal(sing$method, "lm")
  expect_false(sing$converged)
  expect_true(sing$is_singular)
  expect_equal(sing$used_fallback, "fallback_formula")
  kept <- fit_lmer_safely(z ~ x + (x | id), data = d, singular_ok = TRUE, warn_on_fallback = FALSE)
  expect_equal(kept$method, "lmer")
  expect_true(kept$is_singular)

  # random slopes fail to fit here (as many random effects as observations); the random intercept does
  two <- d[d$x <= 2, ]
  expect_warning(
    fb <- fit_lmer_safely(y ~ x + (x | id), data = two, fallback_formula = y ~ x + (1 | id)),
    "fallback_formula"
  )
  expect_equal(fb$method, "lmer")
  expect_equal(fb$used_fallback, "fallback_formula")
  expect_false(fb$converged)
  expect_true(any(grepl("failed", fb$messages)))
  expect_equal(deparse(fb$formula), "y ~ x + (1 | id)")

  plain <- fit_lmer_safely(y ~ x, data = d)
  expect_equal(plain$method, "lm")
  expect_equal(plain$used_fallback, "none")
  expect_error(fit_lmer_safely(y ~ x, data = d, fallback_formula = "y ~ 1"), "formula")
})

# ---- composite index ----

test_that("compute_composite_index applies polarities and min-max scaling", {
  df <- data.frame(a = c(1, 2, 3, 4, 5), b = c(50, 40, 30, 20, 10), k = 7)
  out <- compute_composite_index(df, cols = c("a", "b"), method = "min_max", polarities = c(1, -1))
  # a rises and b falls by the same share of their range, so with b reversed both give the same score
  expect_equal(out$composite_index, (0:4) / 4 - 0.5)
  raw <- compute_composite_index(df, cols = c("a", "b"), method = "min_max", center = FALSE)
  expect_equal(raw$composite_index, rep(0.5, 5))
  named <- compute_composite_index(df, cols = c("a", "b"), method = "min_max", polarities = c(b = -1, a = 1))
  expect_equal(named$composite_index, out$composite_index)
  expect_equal(compute_composite_index(df, cols = c("a", "k"), method = "min_max", center = FALSE)$composite_index,
               (0:4) / 8)
  z <- compute_composite_index(df, cols = c("a", "b"), polarities = c(1, -1))
  expect_equal(z$composite_index, as.numeric(scale(df$a)))
  expect_error(compute_composite_index(df, c("a", "b"), method = "pca", polarities = c(1, -1)), "pca")
  expect_error(compute_composite_index(df, c("a", "b"), polarities = c(1, 2)), "\\+1 or -1")
  expect_error(compute_composite_index(df, c("a", "b"), polarities = 1), "length")

  tab <- index_variable_table(out, cols = c("a", "b"), polarities = c(1, -1), weights = c(0.5, 0.5))
  expect_equal(names(tab), c("variable", "polarity", "weight", "mean", "sd", "min", "max", "r_with_index", "p_value"))
  expect_equal(tab$min, c(1, 10))
  expect_equal(tab$max, c(5, 50))
  expect_equal(sign(tab$r_with_index), c(1, -1))
})

# ---- RSE with grouped baselines ----

test_that("compare_imputation_rse takes a grouped null baseline", {
  set.seed(6)
  cells <- data.frame(variable = "x", time = rep(c(0, 6), each = 20), truth = c(rnorm(20, 10), rnorm(20, 20)))
  res <- rbind(transform(cells, method = "good", imputed = truth + rnorm(40, 0, 0.5)),
               transform(cells, method = "flat", imputed = 15))
  overall <- compare_imputation_rse(res)
  grouped <- compare_imputation_rse(res, group_col = "time")
  gm <- stats::ave(cells$truth, cells$time)
  expect_equal(unique(grouped$null_rmse), sqrt(mean((cells$truth - gm)^2)))
  expect_equal(unique(overall$null_rmse), sqrt(mean((cells$truth - mean(cells$truth))^2)))
  expect_equal(grouped$rmse, overall$rmse)
  # the time means explain most of the spread, so the grouped baseline is the harder one
  expect_true(all(grouped$rse > overall$rse))
  expect_gt(grouped$rse[grouped$method == "flat"], 1)
  med <- compare_imputation_rse(res, null_method = "median", group_col = "time")
  gmed <- stats::ave(cells$truth, cells$time, FUN = stats::median)
  expect_equal(unique(med$null_rmse), sqrt(mean((cells$truth - gmed)^2)))
  expect_error(compare_imputation_rse(res, group_col = "time", baseline = c(x = 1)), "not both")
  expect_error(compare_imputation_rse(res, group_col = "nope"), "group_col")
})

# ---- permutation importance ----

test_that("summarize_dalex_importance and the box plot accept permutation results", {
  fi <- data.frame(
    variable = rep(c("_full_model_", "a", "b", "_baseline_"), each = 4),
    permutation = rep(0:3, 4),
    dropout_loss = c(rep(0.2, 4), 0.31, 0.30, 0.29, 0.34, 0.22, 0.21, 0.23, 0.22, rep(0.5, 4)),
    label = "model"
  )
  s <- summarize_dalex_importance(fi)
  expect_equal(s$variable, c("a", "b"))
  expect_equal(s$mean, c(mean(c(0.30, 0.29, 0.34)), mean(c(0.21, 0.23, 0.22))))
  expect_equal(s$importance, s$mean - 0.2)
  expect_equal(s$min, c(0.29, 0.21))
  expect_equal(s$max, c(0.34, 0.23))
  expect_equal(nrow(attr(s, "permutations")), 6L)
  box <- plot_permutation_importance(s, type = "boxplot")
  expect_equal(nrow(box$data), 6L)
  expect_equal(levels(box$data$variable), c("b", "a"))
  expect_s3_class(plot_permutation_importance(fi, type = "boxplot", top_n = 1), "ggplot")
  expect_s3_class(plot_permutation_importance(fi), "ggplot")
  expect_equal(tbl_permutation_importance(s)$variable, c("a", "b"))
  expect_error(plot_permutation_importance(data.frame(variable = "a", importance = 1), type = "boxplot"), "per-permutation")

  # survex layout: one column per variable, a row per evaluation time and permutation
  sv <- list(result = data.frame(
    `_times_` = rep(c(1, 2), 3), `_full_model_` = 0.1, a = c(0.2, 0.4, 0.2, 0.2, 0.4, 0.4),
    b = 0.12, `_baseline_` = 0.5, `_permutation_` = rep(0:2, each = 2), label = "m", check.names = FALSE
  ))
  class(sv) <- c("surv_feature_importance", "list")
  ss <- summarize_dalex_importance(sv)
  expect_equal(ss$variable, c("a", "b"))
  expect_equal(ss$mean[1], mean(c(0.2, 0.4)))
  expect_equal(ss$importance, c(0.3 - 0.1, 0.12 - 0.1))
})

test_that("DALEX model_parts output is summarized", {
  skip_if_not_installed("DALEX")
  fit <- stats::lm(mpg ~ wt + hp + qsec, data = mtcars)
  ex <- DALEX::explain(fit, data = mtcars[c("wt", "hp", "qsec")], y = mtcars$mpg, verbose = FALSE)
  set.seed(1)
  mp <- DALEX::model_parts(ex, B = 5)
  s <- summarize_dalex_importance(mp)
  expect_setequal(s$variable, c("wt", "hp", "qsec"))
  expect_equal(s$variable[1], "wt")
  expect_true(all(s$min <= s$median & s$median <= s$max))
  expect_s3_class(plot_permutation_importance(mp, type = "boxplot"), "ggplot")
})

# ---- rank heatmap ----

test_that("plot_rank_heatmap can fill by another column or a Brewer palette", {
  d <- data.frame(item = rep(c("a", "b"), 2), fold = rep(c("F1", "F2"), each = 2), rank = c(1, 2, 2, 1),
                  est = c(0.5, -0.2, 0.1, -0.4))
  p <- plot_rank_heatmap(d, "item", "fold", "rank", fill_col = "est")
  b <- ggplot2::ggplot_build(p)
  expect_setequal(b$data[[2]]$label, c(1, 2))
  expect_equal(p$scales$get_scales("fill")$name, "est")
  expect_s3_class(plot_rank_heatmap(d, "item", "fold", "rank", palette = "Blues"), "ggplot")
  expect_error(plot_rank_heatmap(d, "item", "fold", "rank", fill_col = "item"), "numeric")
})

# ---- workflow-set imputation tuner ----

test_that("tune_impute_workflows ranks workflows and fills the missing values", {
  for (pkg in c("workflowsets", "tune", "rsample", "parsnip", "workflows", "yardstick", "recipes")) {
    skip_if_not_installed(pkg)
  }
  d <- mtcars[c("mpg", "wt", "hp", "qsec")]
  d$mpg[c(3, 11, 20)] <- NA
  rec <- recipes::recipe(mpg ~ ., data = d[!is.na(d$mpg), ])
  res <- suppressWarnings(tune_impute_workflows(
    d, "mpg", recipes = list(all = rec, wt_only = recipes::recipe(mpg ~ wt, data = d[!is.na(d$mpg), ])),
    folds = 3, race = FALSE, seed = 11
  ))
  expect_s3_class(res, "cbe_impute_tune")
  expect_equal(nrow(res$ranking), 2L)
  expect_setequal(res$ranking$wflow_id, c("all_lm", "wt_only_lm"))
  expect_false(is.unsorted(res$ranking$rmse))
  expect_equal(nrow(res$predictions), 29L)
  expect_setequal(res$predictions$.row, which(!is.na(d$mpg)))
  expect_equal(res$predictions$observed, d$mpg[res$predictions$.row])
  expect_equal(res$rmse, sqrt(mean((res$predictions$observed - res$predictions$imputed)^2)))
  expect_lt(res$rse, 1)
  expect_equal(res$n_imputed, 3L)
  expect_false(anyNA(res$data$mpg))
  expect_equal(res$data$mpg[!is.na(d$mpg)], d$mpg[!is.na(d$mpg)])
  expect_equal(res$data$mpg[c(3, 11, 20)], stats::predict(res$fit, new_data = d[c(3, 11, 20), ])$.pred)

  tab <- tbl_tune_best_model(res)
  expect_equal(tab$selected, c(TRUE, FALSE))
  expect_equal(tab$rse, round((res$ranking$rmse / res$null_rmse)^2, 3))
  expect_s3_class(plot_tune_grid_rmse(res), "ggplot")
  expect_s3_class(plot_imputed_vs_observed(res$predictions, type = "boxplot"), "ggplot")
  expect_error(tune_impute_workflows(d, "nope"), "one column")
  expect_error(tune_impute_workflows(transform(d, mpg = as.character(mpg)), "mpg"), "numeric")
  expect_error(plot_tune_grid_rmse(list()), "tune_impute_workflows")
})
