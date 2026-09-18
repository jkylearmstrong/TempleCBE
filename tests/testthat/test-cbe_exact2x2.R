test_that("cbe_exact2x2 automatically triggers mid-p adjustment when a zero-cell exists", {
  tab_zero <- matrix(c(0, 10, 5, 15), nrow = 2,
                     dimnames = list(c("Treated", "Control"), c("Event", "No Event")))

  res <- cbe_exact2x2(tab_zero)
  expect_s3_class(res, "tbl_df")
  expect_true(res$has_zero)
  expect_true(res$midp)
  expect_match(res$method, "mid-p")
  expect_type(res$p.value, "double")
  expect_true(res$p.value > 0 && res$p.value <= 1)

  # Check equivalence to direct exact2x2::exact2x2 with midp = TRUE
  direct <- exact2x2::exact2x2(tab_zero, midp = TRUE)
  expect_equal(res$p.value, unname(direct$p.value))
  expect_equal(res$estimate, unname(direct$estimate))
})

test_that("cbe_exact2x2 defaults to standard Central Fisher's exact test when no zero-cell exists", {
  tab_nonzero <- matrix(c(14, 6, 4, 16), nrow = 2,
                        dimnames = list(c("Treated", "Control"), c("Event", "No Event")))

  res <- cbe_exact2x2(tab_nonzero)
  expect_false(res$has_zero)
  expect_false(res$midp)
  expect_match(res$method, "Central Fisher's Exact Test")
  expect_false(grepl("mid-p", res$method))

  direct <- exact2x2::exact2x2(tab_nonzero, midp = FALSE)
  expect_equal(res$p.value, unname(direct$p.value))
  expect_equal(res$estimate, unname(direct$estimate))
})

test_that("cbe_exact2x2 respects explicit midp overrides", {
  tab_zero <- matrix(c(0, 10, 5, 15), nrow = 2)
  # Override with midp = FALSE even with 0 cell
  res_override <- cbe_exact2x2(tab_zero, midp = FALSE)
  expect_false(res_override$midp)
  expect_false(grepl("mid-p", res_override$method))

  tab_nonzero <- matrix(c(14, 6, 4, 16), nrow = 2)
  # Override with midp = TRUE even without 0 cell
  res_midp <- cbe_exact2x2(tab_nonzero, midp = TRUE)
  expect_true(res_midp$midp)
  expect_match(res_midp$method, "mid-p")
})

test_that("cbe_exact2x2 accepts vector and gtsummary (data, variable, by) inputs", {
  df <- data.frame(
    trt = factor(c("A", "A", "A", "A", "B", "B", "B", "B")),
    out = factor(c("Yes", "Yes", "No", "No", "No", "No", "No", "No"))
  )

  res_vec <- cbe_exact2x2(df$out, df$trt)
  res_df <- cbe_exact2x2(data = df, variable = "out", by = "trt")

  expect_equal(res_vec$p.value, res_df$p.value)
  expect_equal(res_vec$estimate, res_df$estimate)
  expect_true(res_vec$has_zero)
  expect_true(res_vec$midp)
})

test_that("cbe_exact2x2 rejects non-2x2 tables with clear message", {
  tab_3x2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  expect_error(cbe_exact2x2(tab_3x2), "requires a 2x2 contingency table")
})

test_that("cbe_exact2x2_ci formats odds ratios and confidence intervals", {
  tab <- matrix(c(10, 5, 2, 15), nrow = 2)
  ci_str <- cbe_exact2x2_ci(tab, digits = 1)
  expect_type(ci_str, "character")
  expect_match(ci_str, "^[0-9.]+ \\([0-9.]+, [0-9.]+\\)$")
})

test_that("cbe_test_categorical dispatches 2x2 and RxC tables according to CBE hierarchy", {
  df <- data.frame(
    group = factor(rep(c("A", "B"), each = 25)),
    bin_zero = factor(c(rep("Yes", 6), rep("No", 19), rep("Yes", 0), rep("No", 25))),
    bin_nonzero = factor(c(rep("Yes", 12), rep("No", 13), rep("Yes", 5), rep("No", 20))),
    multi_sparse = factor(c(rep("C1", 1), rep("C2", 2), rep("C3", 22), rep("C1", 2), rep("C2", 1), rep("C3", 22))),
    multi_dense = factor(c(rep("C1", 8), rep("C2", 9), rep("C3", 8), rep("C1", 9), rep("C2", 8), rep("C3", 8)))
  )

  # 2x2 with zero cell -> Central Fisher mid-p
  res_bz <- cbe_test_categorical(df, "bin_zero", "group")
  expect_match(res_bz$method, "mid-p")
  expect_true(res_bz$p.value < 0.05)

  # 2x2 without zero cell -> Central Fisher standard
  res_bnz <- cbe_test_categorical(df, "bin_nonzero", "group")
  expect_match(res_bnz$method, "Central Fisher's Exact Test")
  expect_false(grepl("mid-p", res_bnz$method))

  # RxC with sparse counts -> Fisher's exact test (expected < 5)
  res_ms <- cbe_test_categorical(df, "multi_sparse", "group")
  expect_match(res_ms$method, "Fisher's exact test")

  # RxC with dense counts -> Pearson's Chi-squared test
  res_md <- cbe_test_categorical(df, "multi_dense", "group")
  expect_match(res_md$method, "Pearson's Chi-squared test")
})

test_that("cbe_test_categorical integrates seamlessly with gtsummary", {
  skip_if_not_installed("gtsummary")

  df <- data.frame(
    group = factor(rep(c("A", "B"), each = 20)),
    bin_var = factor(c(rep("Yes", 5), rep("No", 15), rep("Yes", 0), rep("No", 20))),
    cat_var = factor(c(rep("X", 7), rep("Y", 7), rep("Z", 6), rep("X", 6), rep("Y", 7), rep("Z", 7)))
  )

  tbl <- gtsummary::tbl_summary(df, by = group) |>
    gtsummary::add_p(test = gtsummary::all_categorical() ~ cbe_test_categorical)

  expect_s3_class(tbl, "gtsummary")
  df_tbl <- as.data.frame(tbl)
  expect_true(any(grepl("p-value", colnames(df_tbl), ignore.case = TRUE)))
})

test_that("cbe_contingency_plot renders balloon, bar, mosaic, heatmap, and square plots", {
  df <- data.frame(
    Arm = factor(c(rep("A", 15), rep("B", 15))),
    Result = factor(c(rep("Pass", 12), rep("Fail", 3), rep("Pass", 5), rep("Fail", 10)))
  )

  # Balloon
  p_balloon <- cbe_contingency_plot(df, "Arm", "Result", type = "balloon")
  expect_s3_class(p_balloon, "ggplot")

  # Bar chart: fill, dodge, stack
  p_bar_fill <- cbe_contingency_plot(df, "Arm", "Result", type = "bar", bar_position = "fill")
  expect_s3_class(p_bar_fill, "ggplot")

  p_bar_dodge <- cbe_bar_plot(df, "Arm", "Result", bar_position = "dodge")
  expect_s3_class(p_bar_dodge, "ggplot")

  p_bar_stack <- cbe_bar_plot(df, "Arm", "Result", bar_position = "stack")
  expect_s3_class(p_bar_stack, "ggplot")

  # Mosaic plot
  p_mosaic <- cbe_mosaic_plot(df, "Arm", "Result")
  expect_s3_class(p_mosaic, "ggplot")

  # Heatmap
  p_heat <- cbe_heatmap_plot(df, "Arm", "Result")
  expect_s3_class(p_heat, "ggplot")

  # Square plot
  p_square <- cbe_square_plot(df, "Arm", "Result")
  expect_s3_class(p_square, "ggplot")

  # corrplot returns table invisibly
  res_corr <- cbe_contingency_plot(df, "Arm", "Result", type = "corrplot")
  expect_true(is.table(res_corr) || is.matrix(res_corr))
})

test_that("cbe_pairwise_combos generates all pairwise combinations", {
  df <- data.frame(
    var_a = factor(c("1", "2")),
    var_b = factor(c("X", "Y")),
    var_c = factor(c("M", "N")),
    num_x = c(10, 20)
  )

  combos <- cbe_pairwise_combos(df)
  expect_s3_class(combos, "tbl_df")
  expect_equal(nrow(combos), 3) # 3 choose 2 = 3
  expect_setequal(names(combos), c("id", "var1", "var2", "label1", "label2", "comparison_label"))
})

test_that("plot_categorical_associations computes and plots matrix", {
  df <- data.frame(
    A = factor(rep(c("1", "2"), 15)),
    B = factor(rep(c("X", "Y", "Z"), 10)),
    C = factor(rep(c("M", "N"), each = 15))
  )

  mat <- plot_categorical_associations(df, method = "cramer_v")
  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(3, 3))
  expect_equal(unname(diag(mat)), c(1, 1, 1))
})

test_that("cbe_four_quadrant_report generates 4-quadrant report card and plot", {
  tab <- matrix(c(14, 6, 4, 16), nrow = 2,
                dimnames = list(c("Yes", "No"), c("Drug", "Placebo")))
  rep <- cbe_four_quadrant_report(tab, label1 = "Response", label2 = "Treatment")

  expect_type(rep, "list")
  expect_named(rep, c("quadrants", "p_value", "p_formatted", "test_method", "compact_report", "text_card", "plot"))
  expect_s3_class(rep$quadrants, "tbl_df")
  expect_equal(tolower(rep$quadrants$quadrant), c("q1", "q2", "q3", "q4"))
  expect_equal(rep$quadrants$count, c(14, 4, 6, 16))
  expect_type(rep$p_formatted, "character")
  expect_match(rep$p_formatted, "p ")
  expect_match(rep$compact_report, "q1: n = 14")
  expect_match(rep$compact_report, "p = ")
  expect_match(rep$text_card, "q1:", ignore.case = TRUE)
  expect_match(rep$text_card, "q4:", ignore.case = TRUE)
  expect_s3_class(rep$plot, "ggplot")
})

test_that("pformat formats p-values cleanly", {
  expect_equal(pformat(0.0423), "p = 0.042")
  expect_equal(pformat(0.00004), "p < 0.001")
  expect_equal(pformat(0.852, add_p = FALSE), "0.852")
  expect_equal(cbe_pformat(0.05), "p = 0.050")
})

test_that("cbe_contingency_plot and cbe_four_quadrant_report support test = 'chisq'", {
  tab <- matrix(c(14, 6, 4, 16), nrow = 2,
                dimnames = list(c("Yes", "No"), c("Drug", "Placebo")))

  rep_chi <- cbe_four_quadrant_report(tab, test = "chisq")
  expect_match(rep_chi$test_method, "Pearson's Chi-squared test")
  expect_match(rep_chi$compact_report, "p = ")
  expect_match(rep_chi$text_card, "Pearson's Chi-squared test")

  p_chi <- cbe_contingency_plot(tab, type = "square", test = "chisq")
  expect_s3_class(p_chi, "ggplot")

  df <- data.frame(
    group = factor(rep(c("A", "B"), each = 25)),
    status = factor(c(rep("Yes", 12), rep("No", 13), rep("Yes", 5), rep("No", 20)))
  )
  res_chi <- cbe_test_categorical(df, "status", "group", test = "chisq")
  expect_equal(res_chi$method, "Pearson's Chi-squared test")
  expect_true(is.numeric(res_chi$p.value))
})

test_that("cbe_test_categorical integrates with gtsummary tbl_summary add_p", {
  skip_if_not_installed("gtsummary")
  suppressPackageStartupMessages(library(gtsummary))

  # Test drop-in custom test with gtsummary
  tbl <- trial |>
    tbl_summary(by = trt, include = c(response, death, grade)) |>
    add_p(test = all_categorical() ~ cbe_test_categorical)

  expect_s3_class(tbl, "gtsummary")
  df_tbl <- as.data.frame(tbl)
  expect_true("p.value" %in% names(df_tbl) || any(grepl("p-value", names(df_tbl), ignore.case = TRUE)))
})

test_that("cbe_test_categorical supports custom B and simulate.p.value", {
  df <- data.frame(
    feature = factor(c(rep("A", 10), rep("B", 10), rep("C", 10))),
    outcome = factor(c(rep("Yes", 5), rep("No", 5), rep("Yes", 2), rep("No", 8), rep("Yes", 8), rep("No", 2)))
  )

  # Custom B passed directly
  res_b <- cbe_test_categorical(df, "feature", "outcome", test = "fisher", B = 5000L, simulate.p.value = TRUE)
  expect_equal(res_b$method, "Fisher's exact test (simulated)")
  expect_true(is.numeric(res_b$p.value))
  expect_gte(res_b$p.value, 0)
  expect_lte(res_b$p.value, 1)
})

test_that("cbe_test_categorical performs batch testing across multiple variables", {
  df <- data.frame(
    var1 = factor(rep(c("Low", "High"), 20)),
    var2 = factor(rep(c("Stage I", "Stage II"), each = 20)),
    group = factor(rep(c("Arm A", "Arm B"), 20))
  )

  # Explicit vector of variables
  res_batch <- cbe_test_categorical(df, variable = c("var1", "var2"), by = "group")
  expect_s3_class(res_batch, "tbl_df")
  expect_equal(nrow(res_batch), 2L)
  expect_equal(res_batch$variable, c("var1", "var2"))
  expect_true(all(c("p.value", "p.formatted", "method") %in% names(res_batch)))

  # Auto-discovery with variable = NULL
  res_auto <- cbe_test_categorical(df, variable = NULL, by = "group")
  expect_s3_class(res_auto, "tbl_df")
  expect_equal(nrow(res_auto), 2L)
})

test_that("cbe_test_categorical supports parallel execution via furrr", {
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  df <- data.frame(
    var1 = factor(rep(c("A", "B"), 25)),
    var2 = factor(rep(c("X", "Y"), each = 25)),
    group = factor(rep(c("Ctrl", "Treat"), 25))
  )

  future::plan(future::sequential) # safe testing plan
  res_par <- cbe_test_categorical(df, variable = c("var1", "var2"), by = "group", parallel = TRUE)
  expect_s3_class(res_par, "tbl_df")
  expect_equal(nrow(res_par), 2L)

  # Parallel simulated Fisher chunking test on RxC table
  df_rxc <- data.frame(
    stage = factor(rep(c("I", "II", "III"), c(10, 15, 25))),
    group = factor(rep(c("Ctrl", "Treat"), 25))
  )
  res_chunk <- cbe_test_categorical(df_rxc, variable = "stage", by = "group", test = "fisher",
                                    simulate.p.value = TRUE, B = 10000L, parallel = TRUE, n_chunks = 2L)
  expect_true(is.numeric(res_chunk$p.value))
  expect_gte(res_chunk$p.value, 0)
  expect_lte(res_chunk$p.value, 1)
})

test_that("cbe_test_categorical supports formula interface", {
  df <- data.frame(
    response = factor(rep(c("Yes", "No"), 20)),
    grade = factor(rep(c("I", "II"), each = 20)),
    trt = factor(rep(c("A", "B"), 20))
  )

  # Single variable formula
  res_fmla <- cbe_test_categorical(df, response ~ trt)
  expect_true(is.numeric(res_fmla$p.value))
  expect_match(res_fmla$method, "Central Fisher", ignore.case = TRUE)

  # Multi-variable formula
  res_fmla_multi <- cbe_test_categorical(df, response + grade ~ trt)
  expect_s3_class(res_fmla_multi, "tbl_df")
  expect_equal(nrow(res_fmla_multi), 2L)
  expect_equal(res_fmla_multi$variable, c("response", "grade"))
})


