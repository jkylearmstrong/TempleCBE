test_that("plot_cox_forest generates a valid ggplot object", {
  test_df <- data.frame(
    index_label = c("Age", "Biomarker A", "Biomarker B"),
    estimate    = c(1.2, 0.8, 1.5),
    conf.low    = c(1.05, 0.65, 1.1),
    conf.high   = c(1.38, 0.98, 2.05),
    stringsAsFactors = FALSE
  )

  p <- plot_cox_forest(test_df, title = "Hazard Ratios")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_survival works with cbe_cox object", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_survival(res, data = lung)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_marginal works with cbe_cox object and continuous predictor", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_marginal(res, data = lung, status_col = "status")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_survival and plot_cox_marginal error when feature is missing with raw coxph", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = lung)

  expect_error(plot_cox_survival(fit, data = lung), "Must specify 'feature'")
  expect_error(plot_cox_marginal(fit, data = lung), "Must specify 'feature'")
})

# --- plot_cox_forest: scale / color_by / order_by -------------------------

test_that("plot_cox_forest scale = 'log_hr' log-transforms and uses a 0 reference line", {
  test_df <- data.frame(
    index_label = c("Age", "Biomarker A", "Biomarker B"),
    estimate    = c(1.2, 0.8, 1.5),
    conf.low    = c(1.05, 0.65, 1.1),
    conf.high   = c(1.38, 0.98, 2.05),
    stringsAsFactors = FALSE
  )

  p <- plot_cox_forest(test_df, scale = "log_hr")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "log(Hazard Ratio)")

  built <- ggplot2::layer_data(p, 2) # geom_point layer
  expect_equal(sort(built$x), sort(log(test_df$estimate)))
})

test_that("plot_cox_forest color_by = 'significance' colors points by p-value threshold", {
  test_df <- data.frame(
    index_label = c("Age", "Biomarker A", "Biomarker B"),
    estimate    = c(1.2, 0.8, 1.5),
    conf.low    = c(1.05, 0.65, 1.1),
    conf.high   = c(1.38, 0.98, 2.05),
    p.value     = c(0.01, 0.5, 0.03),
    stringsAsFactors = FALSE
  )

  p <- plot_cox_forest(test_df, p_col = "p.value", color_by = "significance")
  expect_s3_class(p, "ggplot")

  built <- ggplot2::layer_data(p, 2)
  expect_length(unique(built$colour), 2)
})

test_that("plot_cox_forest color_by = 'significance' without p_col errors clearly", {
  test_df <- data.frame(index_label = "Age", estimate = 1.2, conf.low = 1.0, conf.high = 1.4)
  expect_error(plot_cox_forest(test_df, color_by = "significance"), "requires `p_col`")
})

test_that("plot_cox_forest order_by = 'pvalue' without p_col errors clearly", {
  test_df <- data.frame(index_label = "Age", estimate = 1.2, conf.low = 1.0, conf.high = 1.4)
  expect_error(plot_cox_forest(test_df, order_by = "pvalue"), "requires `p_col`")
})

test_that("plot_cox_forest order_by = 'magnitude' orders rows by descending abs(log(estimate))", {
  test_df <- data.frame(
    index_label = c("Small", "Large", "Medium"),
    estimate    = c(1.05, 2.5, 0.6),
    conf.low    = c(0.9, 2.0, 0.4),
    conf.high   = c(1.2, 3.0, 0.8),
    stringsAsFactors = FALSE
  )

  p <- plot_cox_forest(test_df, order_by = "magnitude")
  expect_s3_class(p, "ggplot")

  built <- ggplot2::ggplot_build(p)
  y_labels <- levels(built$plot$data$.y)
  # magnitude order (largest abs(log(estimate)) first); y-axis factor levels are
  # reversed so the plot draws top-to-bottom in that order
  expect_equal(rev(y_labels), c("Large", "Medium", "Small"))
})

test_that("plot_cox_forest order_by = 'pvalue' sorts ascending by p-value", {
  test_df <- data.frame(
    index_label = c("A", "B", "C"),
    estimate    = c(1.1, 1.2, 1.3),
    conf.low    = c(0.9, 1.0, 1.1),
    conf.high   = c(1.3, 1.4, 1.5),
    p.value     = c(0.2, 0.01, 0.5),
    stringsAsFactors = FALSE
  )

  p <- plot_cox_forest(test_df, p_col = "p.value", order_by = "pvalue")
  expect_s3_class(p, "ggplot")
})

# --- plot_cox_forest_multi --------------------------------------------------

test_that("plot_cox_forest_multi renders a faceted forest plot from a cbe_cox_multi object", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))

  res <- cox_multi(lung, outcome = "outcome", features = c("age", "sex_f"))
  p <- plot_cox_forest_multi(res)

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetGrid")
})

test_that("plot_cox_forest_multi errors on non-cbe_cox_multi input", {
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age,
    data = stats::na.omit(survival::lung[, c("time", "status", "age")])
  )
  expect_error(plot_cox_forest_multi(fit), "cbe_cox_multi")
})

# --- plot_cox_survival: overlay_km -----------------------------------------

test_that("plot_cox_survival overlay_km = TRUE adds KM step curves alongside Cox-predicted curves", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  lung$sex_f <- factor(lung$sex, levels = c(1, 2), labels = c("Male", "Female"))
  res <- cox_single(lung, outcome = "outcome", feature = "sex_f")

  p <- plot_cox_survival(res, data = lung, overlay_km = TRUE)
  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 3) # KM step + Cox line + Cox point

  built <- ggplot2::ggplot_build(p)
  expect_length(built$data, 3)
})

# --- plot_cox_marginal: scale ------------------------------------------------

test_that("plot_cox_marginal scale = 'prob' (default) still returns a valid ggplot", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_marginal(res, data = lung, scale = "prob")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_marginal scale = 'hr' plots the relative hazard curve", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_marginal(res, data = lung, scale = "hr")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "Relative Hazard (centered)")
})

test_that("plot_cox_marginal scale = 'log_hr' plots the centered linear predictor", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_marginal(res, data = lung, scale = "log_hr")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "log(Relative Hazard) (centered)")
})

# --- plot_cox_forest: exponentiate / log_scale / sig_level ----------------

forest_df <- data.frame(
  index_label = c("Age", "Biomarker A", "Biomarker B"),
  estimate    = c(1.2, 0.8, 1.5),
  conf.low    = c(1.05, 0.65, 1.1),
  conf.high   = c(1.38, 0.98, 2.05),
  p.value     = c(0.01, 0.2, 0.06),
  stringsAsFactors = FALSE
)

test_that("exponentiate = FALSE shows coefficients with a reference line at 0 and transforms the CI too", {
  p <- plot_cox_forest(forest_df, exponentiate = FALSE)
  expect_equal(p$labels$x, "log(Hazard Ratio)")
  expect_equal(sort(ggplot2::layer_data(p, 3)$x), sort(log(forest_df$estimate)))
  eb <- ggplot2::layer_data(p, 2)
  expect_equal(sort(eb$xmin), sort(log(forest_df$conf.low)))
  expect_equal(sort(eb$xmax), sort(log(forest_df$conf.high)))
  expect_equal(p$layers[[1]]$data$xintercept, 0)
})

test_that("default and exponentiate = TRUE keep hazard ratios with a reference line at 1", {
  for (p in list(plot_cox_forest(forest_df), plot_cox_forest(forest_df, exponentiate = TRUE))) {
    expect_equal(sort(ggplot2::layer_data(p, 3)$x), sort(forest_df$estimate))
    expect_equal(p$layers[[1]]$data$xintercept, 1)
  }
})

test_that("log_scale = TRUE puts hazard ratios on a log axis with the reference line at 1", {
  p <- plot_cox_forest(forest_df, log_scale = TRUE)
  expect_equal(p$layers[[1]]$data$xintercept, 1)
  expect_match(p$labels$x, "log scale")
  sc <- p$scales$get_scales("x")
  expect_equal(sc$trans$name, "log-10")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("argument combinations are validated", {
  expect_error(plot_cox_forest(forest_df, exponentiate = FALSE, log_scale = TRUE), "already on the log scale")
  expect_error(plot_cox_forest(forest_df, scale = "hr", exponentiate = FALSE), "disagree")
  expect_error(plot_cox_forest(forest_df, scale = "log_hr", exponentiate = TRUE), "disagree")
  expect_error(plot_cox_forest(forest_df, scale = "log_hr", log_scale = TRUE), "already on the log scale")
  expect_error(plot_cox_forest(forest_df, exponentiate = "yes"), "TRUE, FALSE or NULL")
  expect_error(plot_cox_forest(forest_df, log_scale = NA), "TRUE or FALSE")
  expect_error(plot_cox_forest(forest_df, sig_level = 2), "between 0 and 1")
  bad <- forest_df
  bad$conf.low[1] <- 0
  expect_error(plot_cox_forest(bad, log_scale = TRUE), "positive")
  # consistent with the older argument
  expect_no_error(plot_cox_forest(forest_df, scale = "log_hr", exponentiate = FALSE))
})

test_that("sig_level sets the significance cut-off and its legend text", {
  p <- plot_cox_forest(forest_df, p_col = "p.value", color_by = "significance", sig_level = 0.1)
  d <- ggplot2::layer_data(p, 3)
  expect_equal(length(unique(d$colour)), 2L)
  expect_equal(sum(d$colour == "#9D2235"), 2L) # p = 0.01 and 0.06
  expect_true("Significant (p < 0.1)" %in% p$data$.sig)
  d05 <- ggplot2::layer_data(plot_cox_forest(forest_df, p_col = "p.value", color_by = "significance"), 3)
  expect_equal(sum(d05$colour == "#9D2235"), 1L)
})

test_that("plot_cox_forest_multi accepts the new display arguments", {
  skip_if_not_installed("survival")
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  m <- cox_multi(lung, outcome = "outcome", features = c("age", "sex"))
  expect_s3_class(plot_cox_forest_multi(m, log_scale = TRUE), "ggplot")
  p0 <- plot_cox_forest_multi(m, exponentiate = FALSE)
  expect_equal(p0$layers[[1]]$data$xintercept, 0)
  expect_error(plot_cox_forest_multi(m, exponentiate = FALSE, log_scale = TRUE), "already on the log scale")
})
