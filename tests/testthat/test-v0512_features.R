testthat::test_that("clean_names_keep_labels works on both character vectors and data frames", {
  # Character vector input (Issue #12)
  raw_names <- c("First Name", "Last Name!", "Age (Years)", "Patient  ID")
  cleaned <- clean_names_keep_labels(raw_names)
  testthat::expect_type(cleaned, "character")
  testthat::expect_equal(cleaned, c("first_name", "last_name", "age_years", "patient_id"))

  # Data frame input
  df <- data.frame(`First Name` = "Alice", `Age (Years)` = 30, check.names = FALSE)
  df_cleaned <- clean_names_keep_labels(df)
  testthat::expect_s3_class(df_cleaned, "data.frame")
  testthat::expect_named(df_cleaned, c("first_name", "age_years"))
  testthat::expect_equal(as.character(labelled::var_label(df_cleaned$first_name)), "First Name")
  testthat::expect_equal(as.character(labelled::var_label(df_cleaned$age_years)), "Age (Years)")
})

testthat::test_that("read_excel_multiple_headers with clean_names = TRUE runs cleanly without error", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  # Create a mock multi-header excel workbook using openxlsx
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", x = matrix(c(
    "Demographics", "Demographics", "Vitals",
    "Age", "Sex", "Blood Pressure",
    "25", "F", "120/80",
    "32", "M", "118/75"
  ), nrow = 4, byrow = TRUE), colNames = FALSE)
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)

  res <- read_excel_multiple_headers(
    path = tmp,
    n_header_rows = 2,
    clean_names = TRUE
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_equal(ncol(res), 3L)
  testthat::expect_true(all(grepl("^[a-z0-9_]+$", colnames(res))))
})

testthat::test_that("diff_deliverables paragraph pairing normalizes curly quotes and whitespace", {
  # Text with curly quotes, non-breaking space, em-dash vs plain ASCII
  old_para <- c(
    "# Introduction",
    "The patient\u2019s \u201cprimary\u201d outcome was evaluated\u00a0\u2014 cleanly.",
    "Section concluded."
  )
  new_para <- c(
    "# Introduction",
    "The patient's \"primary\" outcome was evaluated - cleanly.",
    "Section concluded."
  )

  res <- pair_paragraphs_by_similarity(old_para, new_para, time_budget_sec = 5.0)
  testthat::expect_equal(res$n_edited, 0L)
  testthat::expect_equal(res$n_added, 0L)
  testthat::expect_equal(res$n_removed, 0L)
  testthat::expect_equal(res$n_unpaired, 0L)
  testthat::expect_false(res$budget_exceeded)
  testthat::expect_match(res$diff_summary, "\\+0 / -0 paragraph")
})

testthat::test_that("diff_deliverables handles time budget gracefully with unpaired categorization", {
  # Force time budget to 0 to simulate time limit exceeded
  old_paras <- c(
    "# Section 1",
    "Alpha baseline observation number 1.",
    "Alpha baseline observation number 2.",
    "Alpha baseline observation number 3."
  )
  new_paras <- c(
    "# Section 1",
    "Beta follow-up finding number 1.",
    "Beta follow-up finding number 2.",
    "Beta follow-up finding number 3."
  )

  res <- pair_paragraphs_by_similarity(old_paras, new_paras, time_budget_sec = -1.0)
  testthat::expect_true(res$budget_exceeded)
  testthat::expect_gt(res$n_unpaired, 0L)
  testthat::expect_equal(res$n_added, 0L)
  testthat::expect_equal(res$n_removed, 0L)
  testthat::expect_match(res$diff_summary, "comparison budget exceeded")
  testthat::expect_true("unpaired" %in% colnames(res$section_summary))
})

testthat::test_that("tune_alpha_by_split calibrate parameter functions correctly", {
  # Check argument definition exists and defaults to TRUE
  args <- formals(tune_alpha_by_split)
  testthat::expect_true("calibrate" %in% names(args))
  testthat::expect_identical(args$calibrate, TRUE)

  # Check summarize_tune_results also exposes calibrate
  args_sum <- formals(summarize_tune_results)
  testthat::expect_true("calibrate" %in% names(args_sum))
  testthat::expect_identical(args_sum$calibrate, TRUE)
})

testthat::test_that("core functions run cleanly under lifecycle_verbosity error", {
  withr::local_options(lifecycle_verbosity = "error")

  # clean_names_keep_labels
  raw_v <- c("Col A", "Col B")
  testthat::expect_no_error(clean_names_keep_labels(raw_v))

  df <- data.frame(a = 1, b = 2)
  testthat::expect_no_error(clean_names_keep_labels(df))

  # count_na / features_percent_miss
  testthat::expect_no_error(count_na(c(1, NA, 3)))
  testthat::expect_no_error(features_percent_miss(data.frame(x = c(1, NA, 2), y = c(NA, NA, 1))))
})
