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
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_survival(res, data = lung)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_marginal works with cbe_cox object and continuous predictor", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

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

  res <- cbe_cox_multi(lung, outcome = "outcome", features = c("age", "sex_f"))
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
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "sex_f")

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
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_marginal(res, data = lung, scale = "prob")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cox_marginal scale = 'hr' plots the relative hazard curve", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_marginal(res, data = lung, scale = "hr")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "Relative Hazard (centered)")
})

test_that("plot_cox_marginal scale = 'log_hr' plots the centered linear predictor", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  lung$outcome <- survival::Surv(lung$time, lung$status)
  res <- cbe_cox_single(lung, outcome = "outcome", feature = "age")

  p <- plot_cox_marginal(res, data = lung, scale = "log_hr")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "log(Relative Hazard) (centered)")
})
