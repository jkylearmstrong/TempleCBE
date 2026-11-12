# Tests for the fresh-review (2026-11-11) data/utilities findings, items
# 26, 30 and 52-84. Each block names the item it covers.

test_that("52: tbl_contingency_2x2 marginal totals use n for percent = 'total'", {
  res <- tbl_contingency_2x2(mtcars, "am", "vs", percent = "total")
  expect_equal(res$table$Total[1:2], c("19 (59.4%)", "13 (40.6%)"))
  expect_equal(as.character(res$table[3, 2:3]), c("18 (56.2%)", "14 (43.8%)"))
  expect_equal(res$table[3, 4][[1]], "32 (100.0%)")
  # The row/col modes keep their correct totals.
  expect_equal(tbl_contingency_2x2(mtcars, "am", "vs", percent = "row")$table$Total[1:2],
               c("19 (100.0%)", "13 (100.0%)"))
  expect_equal(as.character(tbl_contingency_2x2(mtcars, "am", "vs", percent = "col")$table[3, 2:3]),
               c("18 (100.0%)", "14 (100.0%)"))
})

test_that("53: cbe_labels() normalizes the templecbe.label_map option", {
  withr::with_options(list(templecbe.label_map = c(age = "Age (years)")), {
    cbe_labels(reset = TRUE)
    m <- cbe_labels()
    expect_s3_class(m, "cbe_label_map")
    expect_equal(m$term, "age")
    expect_equal(m$short, "Age (years)")
    expect_equal(resolve_cbe_labels("age"), "Age (years)")
    expect_equal(tbl_label_map()$short, "Age (years)")
  })
  cbe_labels(reset = TRUE)
})

test_that("54: hierarchy check does not split transformed interactions on ':'", {
  cand <- suppressWarnings(
    sample_candidate_formulas(c("log(a:b)", "a", "b"), n = 10, seed = 1)
  )
  expect_true(any(vapply(cand$terms, function(t) "log(a:b)" %in% t, logical(1))))
})

test_that("55: an empty vector in exclude is rejected up front", {
  expect_error(
    sample_candidate_formulas(c("a", "b"), n = 2, exclude = list(character(0)), seed = 1),
    "must not contain empty"
  )
  expect_error(
    sample_candidate_formulas(c("a", "b"), n = 2, exclude = list(c("a", NA)), seed = 1),
    "must not contain empty"
  )
})

test_that("56: update_deprecated_calls() handles non-UTF-8 (latin-1) files", {
  f <- tempfile(fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  lines <- c("x <- TempleCBE::sd.error(c(1, 2, 3))", "# caf\u00e9")
  writeLines(iconv(lines, from = "UTF-8", to = "latin1"), f, useBytes = TRUE)
  expect_equal(nrow(find_deprecated_calls(f)), 1L)
  update_deprecated_calls(f, dry_run = FALSE)
  expect_true(any(grepl("std_error", readLines(f, warn = FALSE), fixed = TRUE)))
})

test_that("57: pool_imputations() rejects non-finite values and duplicate term rows", {
  fits_inf <- list(
    data.frame(term = "x", estimate = Inf, std.error = 0.1),
    data.frame(term = "x", estimate = 1, std.error = 0.1)
  )
  expect_error(pool_imputations(fits_inf), "non-finite")
  fits_dup <- list(
    data.frame(term = c("x", "x"), estimate = c(1, 99), std.error = c(0.1, 0.1)),
    data.frame(term = "x", estimate = 2, std.error = 0.1)
  )
  expect_error(pool_imputations(fits_dup), "duplicated rows")
})

test_that("58: simulate_section_data() stops on more than one ID variable", {
  mapping <- demo_cbe_mapping()
  mapping <- dplyr::bind_rows(
    mapping,
    tibble::tibble(INDEX = "Demo", old = "Subject2", new = "id2", X_var = FALSE,
                   Y_var = FALSE, ID_var = TRUE, Time_var = FALSE,
                   duplicate_of = NA_character_, duplicate_action = NA_character_)
  )
  expect_error(simulate_section_data(mapping, "Demo"), "more than one `ID_var`")
})

test_that("59: as_cbe_label_map() stops when no term column is present", {
  expect_error(
    as_cbe_label_map(data.frame(label = c("age", "bmi"), value = c("Age", "BMI"))),
    "must have a term column"
  )
})

test_that("60: compute_composite_index() validates cols and weights", {
  df <- data.frame(a = 1:3, b = letters[1:3], c = 4:6)
  expect_error(compute_composite_index(df, cols = c("a", "b")), "must be numeric")
  expect_error(compute_composite_index(df, cols = character(0)), "non-empty")
  expect_error(compute_composite_index(df, cols = c("a", "a")), "duplicated")
  expect_error(compute_composite_index(df, cols = "a", weights = c(NA_real_)), "finite")
  expect_error(compute_composite_index(df, cols = "a", weights = c(Inf)), "finite")
  df$composite_index <- 99
  expect_warning(compute_composite_index(df, cols = "a"), "already exists")
  expect_error(
    index_variable_table(df, cols = "b", index_col = "composite_index"),
    "must be numeric"
  )
})

test_that("61: read_excel_multiple_headers() validates sep/trim/clean_names/n_header_rows", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  writexl::write_xlsx(data.frame(a = 1, b = 2), f)
  expect_error(read_excel_multiple_headers(f, sep = NA), "sep")
  expect_error(read_excel_multiple_headers(f, clean_names = "yes"), "clean_names")
  expect_error(read_excel_multiple_headers(f, trim = "yes"), "trim")
  expect_error(read_excel_multiple_headers(f, n_header_rows = Inf), "n_header_rows")
})

test_that("62: read_positional_excel_headers() does not convert overflow numbers to Inf", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  sheet <- data.frame(
    V1 = c("Group", "var", "1e999", "2"),
    V2 = c("Group", "var2", "3", "4"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(sheet, f, col_names = FALSE)
  out <- read_positional_excel_headers(f, header_rows = 1:2)
  expect_true(is.character(out$group_var))
  expect_equal(out$group_var, c("1e999", "2"))
})

test_that("63: read_raw_table() validates header_rows and rejects .xls", {
  d <- tempfile()
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "t.csv")
  writeLines(c("a,b", "1,4", "2,5", "3,6"), f)
  expect_error(read_raw_table(f, header_rows = 0), "at least 1")
  expect_error(read_raw_table(f, header_rows = 5), "larger than the number of rows")
  expect_error(read_raw_table(f, header_rows = NA), "at least 1")
  xls <- file.path(d, "t.xls")
  writeLines(c("a,b", "1,4"), xls)
  expect_error(read_raw_table(xls), "must be a .csv or .xlsx")
  ok <- read_raw_table(f, header_rows = 1)
  expect_equal(nrow(ok), 3L)
})

test_that("64: find_section_file() matches the index literally and names a missing directory", {
  d <- tempfile()
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines(c("a", "1"), file.path(d, "Demo (1).csv"))
  expect_equal(basename(find_section_file(d, "Demo (1)")), "Demo (1).csv")
  expect_error(find_section_file(file.path(d, "nope"), "Demo"), "not an existing directory")
})

test_that("65: validate_column_mapping() rejects NA role flags and NA names", {
  mapping <- demo_cbe_mapping()
  mapping$Y_var[1] <- NA
  expect_error(validate_column_mapping(mapping), "must not contain NA")
  mapping <- demo_cbe_mapping()
  mapping$new[1] <- NA_character_
  expect_error(validate_column_mapping(mapping), "must not contain NA")
})

test_that("66: a whitespace-only secret key stops instead of keying the HMAC", {
  expect_error(resolve_hmac_key("   "), "whitespace-only")
  expect_error(resolve_hmac_key(""), "No secret key")
})

test_that("67: anonymizer seed / n_chars / max_tries are validated", {
  expect_error(generate_last_names(2, seed = c(1, 2)), "single whole number")
  expect_error(generate_last_names(2, max_tries = 0), "at least 1")
  expect_error(generate_pseudonym_token(n = 2, seed = NA), "single whole number")
  expect_warning(generate_pseudonym_token(n = 1, n_chars = 100), "exceeds the 64")
})

test_that("68: a mapping file whose mappings is not a JSON object stops", {
  skip_if_not_installed("jsonlite")
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  writeLines('{"mappings": ["PI_x"]}', f)
  expect_error(
    anonymize_pi("Smith", method = "token", secrets_path = f),
    "not a JSON object"
  )
})

test_that("69: sample_candidate_formulas() requires whole n/sizes and rejects '1'", {
  expect_error(sample_candidate_formulas(c("a", "b"), n = 1.5, seed = 1), "positive whole number")
  expect_error(sample_candidate_formulas(c("a", "b"), n = 2, max_size = 2.9, seed = 1), "whole number")
  expect_error(sample_candidate_formulas(c("1", "a"), n = 2, seed = 1), "intercept")
})

test_that("70: as_tuning_results() validates alpha/IBS/domain and warns on dropped metric rows", {
  expect_error(as_tuning_results(data.frame(alpha = 2, IBS = 0.1)), "between 0 and 1")
  expect_error(as_tuning_results(data.frame(alpha = 0.5, IBS = -0.5)), "between 0 and 1")
  expect_error(as_tuning_results(data.frame(alpha = 0.5, IBS = Inf)), "finite")
  expect_error(as_tuning_results(data.frame(alpha = 0.5, IBS = "abc")), "not numbers")
  expect_error(
    as_tuning_results(data.frame(alpha = 0.5, IBS = 0.1), domain = c("A", "B")),
    "single string"
  )
  tm <- data.frame(.metric = c("roc_auc", "brier_survival_integrated"),
                   mean = c(0.7, 0.2), mixture = c(0.5, 0.1))
  expect_warning(res <- as_tuning_results(tm), "Dropping 1 row")
  expect_equal(nrow(res), 1L)
  # A `brier_score` source is not stamped as the integrated Brier metric.
  res2 <- suppressWarnings(as_tuning_results(data.frame(brier_score = 0.1)))
  expect_equal(unique(res2$.metric), "brier")
})

test_that("71: pool_imputations() drops unused levels of a stacked imputation column", {
  stacked <- data.frame(
    imputation = factor(c(1, 1, 2, 2), levels = c("1", "2", "3")),
    term = rep(c("x", "z"), 2),
    estimate = c(1, 0.5, 2, 0.6),
    std.error = rep(0.1, 4)
  )
  out <- pool_imputations(stacked)
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out$m), 2L)
})

test_that("72: transform_longitudinal_features() handles 0 rows, duplicate features and '.col'", {
  long <- data.frame(id = rep(1:2, each = 2), time = rep(c(0, 5), 2),
                     hr = c(80, 90, 60, 61))
  empty <- long[0, ]
  out_long <- transform_longitudinal_features(empty, "id", "time", "hr")
  expect_equal(nrow(out_long), 0L)
  expect_equal(names(out_long), c("id", "feature", "method", "value", "n_obs"))
  out_wide <- transform_longitudinal_features(empty, "id", "time", "hr", format = "wide")
  expect_equal(nrow(out_wide), 0L)
  expect_true(all(c("id", "hr__pct_change_baseline") %in% names(out_wide)))
  expect_warning(
    w <- transform_longitudinal_features(long, "id", "time", c("hr", "hr"), format = "wide"),
    "Duplicate feature"
  )
  expect_true(is.numeric(w$hr__variance))
  d <- long
  names(d)[1] <- ".col"
  expect_error(transform_longitudinal_features(d, ".col", "time", "hr"), "must not be named")
})

test_that("73: glm_single() rejects outcome == feature; tbl_coef_forest() labels the level", {
  expect_error(glm_single(mtcars, "mpg", "mpg"), "different columns")
  res <- glm_single(mtcars, "mpg", "wt", conf_level = 0.90)
  tbl <- tbl_coef_forest(res, conf_level = 0.90)
  expect_true(any(grepl("90% CI", names(tbl), fixed = TRUE)))
  expect_false(any(grepl("95% CI", names(tbl), fixed = TRUE)))
  expect_equal(names(tbl_coef_forest(res))[2], "Estimate [95% CI]")
})

test_that("74: summary_table() and std_error() validate their inputs", {
  expect_error(summary_table(data.frame(x = letters[1:3]), x), "must be a numeric")
  expect_error(summary_table(mtcars, mpg, table.output = NA), "single logical")
  expect_error(summary_table(mtcars, mpg, na.rm = NA), "single logical")
  expect_error(std_error(letters[1:3]), "must be a numeric")
  expect_error(std_error(1:3, na.rm = NA), "single logical")
  expect_equal(std_error(1:3), stats::sd(1:3) / sqrt(3))
})

test_that("75: paired comparisons validate conf_level, guard non-finite diffs and check both variables", {
  d <- data.frame(a = rnorm(10), b = rnorm(10), g = rep(1:2, each = 5))
  expect_error(paired_diff_ribbon_by_group(d, "a", "b", group_col = "g", conf_level = 2),
               "between 0 and 1")
  d_inf <- data.frame(a = c(Inf, 1, 2, 3), b = c(Inf, 2, 3, 4))
  expect_warning(res <- paired_diff_tests(d_inf, "a", "b"), "non-finite difference")
  expect_equal(res$n, 3L)
  d_const <- data.frame(a = 1:5, b = rep(7, 5))
  expect_error(compare_paired_correlation(d_const, "a", "b"), "two distinct values")
})

test_that("26+76: step_lencode_* validate penalty, mixture and outcome", {
  skip_if_not_installed("recipes")
  rec <- recipes::recipe(time + status ~ ., data = data.frame(time = 1:3, status = 0:2, g = factor(c("a", "b", "a"))))
  expect_error(step_lencode_coxnet(rec, g, penalty = c(0.05, 0.1)), "single non-negative")
  expect_error(step_lencode_coxnet(rec, g, penalty = -1), "single non-negative")
  expect_error(step_lencode_coxnet(rec, g, mixture = 2), "between 0 and 1")
  expect_error(
    step_lencode_coxnet(rec, g, outcome = c("time", "status", "x", "y")),
    "must name 2 or 3 columns"
  )
  expect_error(step_lencode_joint_model(rec, g, penalty = -1), "single non-negative")
  expect_error(
    step_lencode_joint_model(rec, g, outcome = c("time", "status", "x", "y")),
    "must name 2 or 3 columns"
  )
})

test_that("77: excel_colnames() escapes the replacement and validates max_char", {
  expect_equal(excel_colnames(c("a/b", "c?d"), replacement = "."), c("a.b", "c.d"))
  expect_error(excel_colnames("a", max_char = NA), "max_char")
  expect_error(excel_colnames("a", max_char = "31"), "max_char")
  expect_equal(excel_colnames(c("abcdefghij", "abcdefghiX"), max_char = 5),
               c("abcde", "abcde_1"))
  # A non-positive limit is ignored (pre-existing documented behaviour).
  expect_equal(excel_colnames(strrep("x", 40), max_char = 0), strrep("x", 40))
})

test_that("78: label maps reject duplicate terms; formula labels split at top-level '+'", {
  expect_error(
    as_cbe_label_map(data.frame(term = c("a", "a"), short = c("A", "A2"), full = c("A", "A2"))),
    "duplicate term"
  )
  m <- data.frame(term = "c", short = "Cee", full = "Cee full")
  expect_equal(
    resolve_cbe_formula_labels("log(a+b)+c", label_map = m),
    "log(a+b) + Cee"
  )
})

test_that("79: run_pca() returns 0 proportions for zero total variance", {
  out <- run_pca(data.frame(a = rep(1, 5), b = rep(2, 5)), scale = FALSE)
  expect_true(all(out$proportion == 0))
  expect_true(all(out$variance_pct == 0))
  expect_error(run_pca(data.frame(a = rep(1, 5), b = 1:5), scale = TRUE), "constant column")
})

test_that("80: pairwise-categorical plots reject empty tables and bad inputs", {
  tab_na <- table(factor(c(NA, NA), levels = c("a", "b")), factor(c(NA, NA), levels = c("x", "y")))
  expect_error(cbe_contingency_plot(tab_na, type = "mosaic"), "no observations")
  tab0 <- matrix(0L, 2, 2, dimnames = list(c("a", "b"), c("x", "y")))
  expect_error(cbe_contingency_plot(tab0, type = "square"), "no observations")
  df <- data.frame(age = 1:4, sex = factor(c("F", "M", "F", "M")),
                   grp = factor(c("A", "A", "B", "B")))
  expect_error(pairwise_combos(df, cols = c("age", "sex", "grp")), "not categorical")
  labels_df <- data.frame(variable = c("sex", "sex"), label = c("Sex", "Sex2"))
  expect_error(pairwise_combos(df, labels_df = labels_df), "duplicate `variable`")
  expect_error(
    cbe_contingency_plot(df, "sex", "grp", size_range = c(22, 6)),
    "size_range"
  )
})

test_that("81: scan_data_io() validates max_depth", {
  expect_error(scan_data_io(tempdir(), max_depth = NA), "max_depth")
  expect_error(scan_data_io(tempdir(), max_depth = -1), "max_depth")
  expect_error(scan_data_io(tempdir(), max_depth = 2.5), "max_depth")
})

test_that("82: copy_data_manifest() stops when a copy fails", {
  src <- tempfile()
  dir.create(src)
  dst <- tempfile()
  dir.create(dst)
  on.exit({ unlink(src, recursive = TRUE); unlink(dst, recursive = TRUE) }, add = TRUE)
  writeLines("x", file.path(src, "a.csv"))
  # The destination parent is a file, not a directory, so file.copy() fails.
  writeLines("blocker", file.path(dst, "blocker"))
  manifest <- data.frame(file = "blocker/a.csv", source = "a.csv", stringsAsFactors = FALSE)
  expect_error(
    copy_data_manifest(dir = dst, manifest = manifest, project_root = src),
    "Failed to copy"
  )
})

test_that("83: tune_impute_workflows() validates seed, folds, race and grid up front", {
  expect_error(tune_impute_workflows(mtcars, "mpg", seed = "x"), "single whole number")
  expect_error(tune_impute_workflows(mtcars, "mpg", folds = 2.5), "whole number")
  expect_error(tune_impute_workflows(mtcars, "mpg", race = "yes"), "TRUE or FALSE")
  expect_error(tune_impute_workflows(mtcars, "mpg", grid = 0), "whole number")
  expect_error(
    tune_impute_workflows(mtcars, "mpg", grid = data.frame()),
    "at least one row"
  )
})

test_that("84: the percent = 'total' Total column is asserted (item 52 regression)", {
  res <- tbl_contingency_2x2(mtcars, "am", "vs", percent = "total", include_total = TRUE)
  expect_false(any(grepl("100.0%", res$table$Total[1:2], fixed = TRUE)))
})

test_that(".dict_lookup does not double-append a bare unit (item 78)", {
  dict <- data.frame(
    term = c("sbp", "dbp"),
    short = c("SBP", "DBP"),
    full = c("Systolic BP mmHg", "Diastolic BP (mmHg)"),
    unit = c("mmHg", "mmHg"),
    stringsAsFactors = FALSE
  )
  expect_equal(.dict_lookup("sbp", dict, "unit"), "Systolic BP mmHg")
  expect_equal(.dict_lookup("dbp", dict, "unit"), "Diastolic BP (mmHg)")
  # A label with no unit at all still gets it appended.
  dict2 <- data.frame(
    term = "hr", short = "HR", full = "Heart rate", unit = "bpm",
    stringsAsFactors = FALSE
  )
  expect_equal(.dict_lookup("hr", dict2, "unit"), "Heart rate (bpm)")
})
