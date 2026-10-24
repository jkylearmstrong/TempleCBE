# Edge cases of small helpers that analysis code calls (0.7.3 coverage triage).

test_that("pretty_name() handles empty, NA, camelCase and acronym input", {
  expect_equal(pretty_name(NULL), character())
  expect_equal(pretty_name(character()), character())
  expect_equal(pretty_name("patient_bmi"), "Patient BMI")
  expect_equal(pretty_name("followUpTime"), "Follow Up Time")
  expect_equal(pretty_name("hr.ratio-sd"), "HR Ratio SD")
  expect_equal(pretty_name("  xx__yy  "), "Xx Yy")
  expect_equal(pretty_name("my_gene", acronyms = character()), "My Gene")
  expect_equal(pretty_name(c("auc", NA)), c("AUC", NA))
  expect_equal(pretty_name(1:2), c("1", "2"))
})

test_that("excel_colnames() removes forbidden characters, truncates and fills empty names", {
  expect_equal(excel_colnames(NULL), character())
  expect_equal(excel_colnames("Patient/Subject [ID]"), "Patient_Subject _ID_")
  expect_equal(excel_colnames("a:b*c?d"), "a_b_c_d")
  long <- strrep("x", 40)
  expect_equal(nchar(excel_colnames(long, max_char = 31)), 31L)
  expect_equal(excel_colnames(long, max_char = 0), long) # non-positive limit is ignored
  expect_equal(excel_colnames(c("ok", "", NA, "///")), c("ok", "Col_2", "Col_3", "_"))
  expect_equal(excel_colnames("a/b", replacement = ""), "ab")
  expect_equal(excel_colnames("a/b", replacement = " "), "a b")
  # truncation can leave a trailing space; the result is still non-empty
  expect_true(all(nzchar(excel_colnames(c("   ", "a"), max_char = 1))))
})

test_that("copy_var_labels() copies chosen attributes only for shared columns", {
  from <- data.frame(age = 50, bmi = 24, only_from = 1)
  attr(from$age, "label") <- "Age (years)"
  attr(from$age, "format.sas") <- "BEST12."
  attr(from$bmi, "label") <- "Body mass index"
  to <- data.frame(age = 60, bmi = 28, only_to = 2)
  out <- copy_var_labels(from, to)
  expect_equal(attr(out$age, "label"), "Age (years)")
  expect_equal(attr(out$age, "format.sas"), "BEST12.")
  expect_null(attr(out$only_to, "label"))
  out2 <- copy_var_labels(from, to, attrs = "label")
  expect_null(attr(out2$age, "format.sas"))
  expect_equal(attr(out2$bmi, "label"), "Body mass index")
  # nothing in common: unchanged
  expect_identical(copy_var_labels(from["only_from"], to["only_to"]), to["only_to"])
  expect_error(copy_var_labels(1, to), "data frames")
  expect_error(copy_var_labels(from, list(a = 1)), "data frames")
})

test_that("summary_table() copes with an all-NA column and with na.rm", {
  d <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 3, NA, NA), y = c(NA_real_, NA, NA, NA))
  by_g <- dplyr::group_by(d, g)
  res <- expect_no_warning(summary_table(by_g, x, na.rm = TRUE))
  expect_equal(res$N, c(2L, 0L))
  expect_equal(res$Mean[1], 2)
  expect_true(is.na(res$min[2]) && is.na(res$Max[2]))
  all_na <- expect_no_warning(summary_table(d, y, na.rm = TRUE))
  expect_equal(all_na$N, 0L)
  expect_true(is.na(all_na$min))
  expect_equal(all_na$variable, "y")
  # without na.rm the missing values propagate, and N counts every row
  raw <- summary_table(d, x)
  expect_equal(raw$N, 4L)
  expect_true(is.na(raw$Mean))
  tab <- summary_table(d, x, na.rm = TRUE, table.output = TRUE)
  expect_true(all(c("Mean (SD)", "Range", "variable") %in% names(tab)))
})

test_that("min_max_norm() maps a constant column to zero and keeps NA", {
  expect_equal(min_max_norm(c(5, 5, 5)), c(0, 0, 0))
  expect_equal(min_max_norm(c(1, NA, 3)), c(0, NA, 1))
  expect_true(all(is.na(min_max_norm(c(NA_real_, NA)))))
  out <- min_max_norm(data.frame(k = c(2, 2), x = c(0, 10), s = c("a", "b")))
  expect_equal(names(out), c("k", "x"))
  expect_equal(out$k, c(0, 0))
  expect_equal(out$x, c(0, 1))
  expect_error(min_max_norm(letters), "numeric")
})

test_that("as_tuning_results() maps synonyms, reads RDS files and has summary() and tidy() methods", {
  old <- data.frame(mixture = c(0.1, 0.5, 0.9), mean_ibs = c(0.2, 0.1, 0.15), lambdas = c(0.01, 0.02, 0.03),
                    formulas = c("a", "b", "c"), splits = 1:3, fold = 1)
  res <- as_tuning_results(old, domain = "icu")
  expect_s3_class(res, "cbe_tuning_results")
  expect_equal(res$alpha, c(0.1, 0.5, 0.9))
  expect_equal(res$IBS, c(0.2, 0.1, 0.15))
  expect_equal(res$lambda, c(0.01, 0.02, 0.03))
  expect_equal(res$formula, c("a", "b", "c"))
  expect_equal(res$inner_resamples_splits, c("1", "2", "3"))
  expect_equal(res$outer_fold, rep("1", 3))
  expect_equal(unique(res$domain), "icu")

  s <- summary(res)
  expect_s3_class(s, "summary.cbe_tuning_results")
  expect_equal(s$n_rows, 3L)
  expect_equal(s$mean_ibs, mean(c(0.2, 0.1, 0.15)))
  expect_equal(s$median_ibs, 0.15)
  expect_equal(s$alphas, c(0.1, 0.5, 0.9))
  td <- generics::tidy(res)
  expect_false(inherits(td, "cbe_tuning_results"))
  expect_equal(nrow(td), 3L)

  f <- tempfile(fileext = ".rds")
  saveRDS(data.frame(alpha = 0.3, ibs = 0.11), f)
  from_file <- as_tuning_results(f)
  expect_equal(from_file$IBS, 0.11)
  writeBin(as.raw(1:5), f)
  expect_error(as_tuning_results(f), "Failed to read")
})

test_that("as_tuning_results() picks the Brier rows of a tidymodels metric table and rejects unusable input", {
  tm <- data.frame(.metric = c("roc_auc", "brier_survival_integrated", "brier_survival_integrated"),
                   mean = c(0.7, 0.2, 0.3), mixture = c(0.5, 0.1, 0.9))
  res <- as_tuning_results(tm)
  expect_equal(nrow(res), 2L)
  expect_equal(res$IBS, c(0.2, 0.3))
  # alpha defaults to 1 when absent
  expect_equal(as_tuning_results(data.frame(brier_score = 0.1))$alpha, 1)
  expect_error(as_tuning_results(data.frame(x = 1)), "Missing required column")
  expect_error(as_tuning_results(1:3), "must be a data frame")
  expect_error(as_tuning_results("no/such/file.rds"), "must be a data frame")
})
