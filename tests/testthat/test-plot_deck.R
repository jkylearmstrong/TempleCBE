test_that("plot_survival_km returns a ggplot object", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex")])
  p <- plot_survival_km(lung, time_col = "time", status_col = "status", group_col = "sex")
  expect_s3_class(p, "ggplot")
})

test_that("plot_dynamic_trajectory returns a ggplot object", {
  traj_df <- data.frame(
    subject_id = rep(1:10, each = 3),
    time       = rep(c(0, 10, 20), 10),
    val        = rnorm(30, mean = 50, sd = 5),
    outcome    = rep(c("Survived", "Died"), each = 15),
    stringsAsFactors = FALSE
  )

  p <- plot_dynamic_trajectory(traj_df, value_col = "val", time_col = "time", group_col = "outcome")
  expect_s3_class(p, "ggplot")
})

test_that("plot_group_comparison returns a ggplot object", {
  comp_df <- data.frame(
    subject_id = 1:20,
    val        = rnorm(20, mean = 50, sd = 5),
    cohort     = rep(c("Survived", "Died"), each = 10),
    stringsAsFactors = FALSE
  )

  p <- plot_group_comparison(comp_df, value_col = "val", group_col = "cohort")
  expect_s3_class(p, "ggplot")
})

test_that("plot_missingness returns a ggplot object", {
  miss_df <- data.frame(
    parameter = c("MAP", "MAP", "Pulse", "Pulse"),
    pct       = c(0.1, 0.2, 0.05, 0.15),
    method    = c("OSM3/Spin", "ABL", "OSM3/Spin", "ABL"),
    stringsAsFactors = FALSE
  )

  p <- plot_missingness(miss_df)
  expect_s3_class(p, "ggplot")
})

test_that("table_two_by_two constructs valid contingency table", {
  df <- data.frame(
    Treatment = c("Drug", "Drug", "Placebo", "Placebo", "Placebo", "Drug"),
    Response  = c("Yes", "No", "No", "No", "Yes", "Yes"),
    stringsAsFactors = FALSE
  )

  res <- table_two_by_two(df, row_var = "Treatment", col_var = "Response")
  expect_type(res, "list")
  expect_named(res, c("table", "p_value", "note"))
  expect_s3_class(res$table, "tbl_df")
  expect_type(res$p_value, "double")
  expect_type(res$note, "character")
})
