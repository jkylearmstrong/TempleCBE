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
  expect_true(all(c("table", "p_value", "estimate", "conf.int", "method", "midp", "has_zero", "note") %in% names(res)))
  expect_s3_class(res$table, "tbl_df")
  expect_type(res$p_value, "double")
  expect_type(res$note, "character")
  expect_false(res$has_zero)
})

test_that("table_two_by_two automatically defaults to mid-p when a zero-cell is present", {
  df_zero <- data.frame(
    Treatment = c(rep("Drug", 10), rep("Placebo", 10)),
    Response  = c(rep("Yes", 6), rep("No", 4), rep("Yes", 0), rep("No", 10))
  )

  res_zero <- table_two_by_two(df_zero, row_var = "Treatment", col_var = "Response")
  expect_true(res_zero$has_zero)
  expect_true(res_zero$midp)
  expect_match(res_zero$method, "mid-p")
  expect_match(res_zero$note, "zero-cell detected")
  expect_true(res_zero$p_value < 0.05)
})


test_that("plot_survival_km() adds a log-rank p-value for pval = TRUE and p_value = TRUE alike", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex")])
  lung$sex <- factor(lung$sex, labels = c("M", "F"))
  base <- plot_survival_km(lung, "time", "status", "sex")
  p1 <- plot_survival_km(lung, "time", "status", "sex", pval = TRUE, caption = "my caption")
  p2 <- plot_survival_km(lung, "time", "status", "sex", p_value = TRUE)
  expect_length(p1$layers, length(base$layers) + 1L)
  expect_length(p2$layers, length(base$layers) + 1L)
  lbl <- p1$layers[[length(p1$layers)]]$aes_params$label
  expect_match(lbl, "^p[=<]")
  # the caption stays the user's
  expect_equal(p1$labels$caption, "my caption")
  # pval wins over p_value
  p3 <- plot_survival_km(lung, "time", "status", "sex", p_value = TRUE, pval = FALSE)
  expect_length(p3$layers, length(base$layers))
})

test_that("plot_survival_km() warns when a p-value cannot be computed", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex")])
  expect_warning(p <- plot_survival_km(lung, "time", "status", pval = TRUE), "at least two groups")
  expect_s3_class(p, "ggplot")
  one <- lung[lung$sex == 1, ]
  expect_warning(plot_survival_km(one, "time", "status", "sex", pval = TRUE), "at least two groups")
})

test_that("plot_survival_km(risk_table = TRUE) draws the curve and a risk table together", {
  skip_if_not_installed("patchwork")
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex")])
  lung$sex <- factor(lung$sex, labels = c("M", "F"))
  p <- plot_survival_km(lung, "time", "status", "sex", risk_table = TRUE, pval = TRUE)
  expect_s3_class(p, "ggsurvfit")
  built <- ggsurvfit::ggsurvfit_build(p)
  expect_s3_class(built, "patchwork")
  expect_length(built$patches$plots, 2L)
  # the risk table counts the same strata as the curve
  tbl <- ggsurvfit::tidy_survfit(ggsurvfit::survfit2(survival::Surv(time, status) ~ sex, data = lung))
  expect_setequal(unique(as.character(tbl$strata)), c("M", "F"))
})

traj_data <- function(groups) {
  n <- length(groups)
  data.frame(
    id = rep(seq_len(n * 4), each = 3),
    time = rep(c(0, 10, 20), n * 4),
    val = seq_len(n * 12) / 7,
    grp = rep(rep(groups, each = 4), each = 3),
    stringsAsFactors = FALSE
  )
}
line_cols <- function(p) {
  d <- ggplot2::layer_data(p, 2)
  as.character(vapply(split(d$colour, d$group), function(x) unique(x)[1], character(1)))
}

test_that("plot_dynamic_trajectory() still works unchanged for Survived/Died", {
  d <- traj_data(c("Survived", "Died"))
  p <- plot_dynamic_trajectory(d, "val", "time", "grp")
  expect_s3_class(p, "ggplot")
  expect_equal(unname(line_cols(p)), c("#9D2235", "#6F6F6F")) # Died, Survived (alphabetical, as before)
})

test_that("plot_dynamic_trajectory() takes more than two groups without extra arguments", {
  d <- traj_data(c("A", "B", "C", "D"))
  p <- plot_dynamic_trajectory(d, "val", "time", "grp")
  expect_equal(unname(line_cols(p)), unname(cbe_palette[1:4]))
})

test_that("group_levels fixes order, legend and color assignment (ISS tertiles)", {
  d <- traj_data(c("T3", "T1", "T2"))
  p <- plot_dynamic_trajectory(d, "val", "time", "grp", group_levels = c("T1", "T2", "T3"),
                               palette = c("#111111", "#222222", "#333333"))
  expect_equal(levels(p$data$grp), c("T1", "T2", "T3"))
  expect_equal(unname(line_cols(p)), c("#111111", "#222222", "#333333"))
  expect_error(plot_dynamic_trajectory(d, "val", "time", "grp", group_levels = c("T1", "T2")), "does not list group")
  expect_error(plot_dynamic_trajectory(d, "val", "time", "grp", group_levels = c("T1", "T1", "T2", "T3")), "duplicates")
})

test_that("a factor keeps its level order, dropping unused levels", {
  d <- traj_data(c("hi", "lo", "mid"))
  d$grp <- factor(d$grp, levels = c("lo", "mid", "hi", "unused"))
  p <- plot_dynamic_trajectory(d, "val", "time", "grp")
  expect_equal(levels(p$data$grp), c("lo", "mid", "hi"))
})

test_that("a named palette keeps each group's color when only some groups are plotted", {
  pal <- c(A = "#AA0000", B = "#00BB00", C = "#0000CC")
  all3 <- plot_dynamic_trajectory(traj_data(c("A", "B", "C")), "val", "time", "grp", palette = pal)
  two <- plot_dynamic_trajectory(traj_data(c("A", "C")), "val", "time", "grp", palette = pal)
  expect_equal(unname(line_cols(all3)), c("#AA0000", "#00BB00", "#0000CC"))
  expect_equal(unname(line_cols(two)), c("#AA0000", "#0000CC"))
})

test_that("palette problems are errors, not recycled or missing colors", {
  d <- traj_data(c("A", "B", "C"))
  expect_error(plot_dynamic_trajectory(d, "val", "time", "grp", palette = c("#111111", "#222222")), "2 colors for 3 groups")
  expect_error(plot_dynamic_trajectory(d, "val", "time", "grp", palette = c(A = "red", B = "blue")), "no color for group.*C")
  expect_error(plot_dynamic_trajectory(d, "val", "time", "nope"), "not a column")
  many <- traj_data(letters[1:8])
  expect_error(plot_dynamic_trajectory(many, "val", "time", "grp"), "palette has")
})

test_that("colors = NULL and explicit colors behave as before", {
  d <- traj_data(c("Survived", "Died"))
  expect_equal(unname(line_cols(plot_dynamic_trajectory(d, "val", "time", "grp", colors = NULL))), unname(cbe_palette[1:2]))
  expect_equal(unname(line_cols(plot_dynamic_trajectory(d, "val", "time", "grp", colors = c("red", "blue")))), c("red", "blue"))
})

test_that("plot_survival_km() p-value works when group_col is a formula expression", {
  lung <- stats::na.omit(survival::lung[, c("time", "status", "sex", "ph.ecog")])
  base <- plot_survival_km(lung, "time", "status", "sex + ph.ecog")
  p <- plot_survival_km(lung, "time", "status", "sex + ph.ecog", pval = TRUE)
  expect_length(p$layers, length(base$layers) + 1L)
})

test_that("numeric group labels sort as numbers in plot_dynamic_trajectory()", {
  d <- traj_data(c("1", "2", "10"))
  d$grp <- as.numeric(d$grp)
  expect_equal(levels(plot_dynamic_trajectory(d, "val", "time", "grp")$data$grp), c("1", "2", "10"))
})
