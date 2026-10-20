test_that("v0.6.0: Package version and metadata fallbacks are synchronized", {
  ver <- as.character(utils::packageVersion("TempleCBE"))
  expect_equal(ver, "0.6.0")

  news_candidates <- c(
    system.file("NEWS.md", package = "TempleCBE"),
    file.path(system.file(package = "TempleCBE"), "NEWS.md"),
    tryCatch(testthat::test_path("../../NEWS.md"), error = function(e) "")
  )
  found_news <- news_candidates[file.exists(news_candidates) & nzchar(news_candidates)]
  if (length(found_news) > 0L) {
    news_lines <- readLines(found_news[1], n = 5)
    expect_true(any(grepl("^# TempleCBE 0\\.6\\.0", news_lines)))
  }
})

test_that("v0.6.0: Polymorphic clean_names_keep_labels accepts character vectors and data frames", {
  # Character vector input
  raw_cols <- c("Patient ID", "Systolic BP (mmHg)", "Date of Birth")
  cleaned_cols <- clean_names_keep_labels(raw_cols)
  expect_equal(cleaned_cols, c("patient_id", "systolic_bp_mm_hg", "date_of_birth"))

  # Data frame input
  df <- data.frame(
    "Patient ID" = 1:2,
    "Blood Sugar" = c(95, 110),
    check.names = FALSE
  )
  cleaned_df <- clean_names_keep_labels(df)
  expect_named(cleaned_df, c("patient_id", "blood_sugar"))
})

test_that("v0.6.0: Smart quotes and unicode characters normalize cleanly in paragraph pairing", {
  p_old <- c(
    "# Introduction",
    "The patient\u2019s \u201cbaseline\u201d status\u00a0\u2014 cleanly evaluated.",
    "Conclusion."
  )
  p_new <- c(
    "# Introduction",
    "The patient's \"baseline\" status - cleanly evaluated.",
    "Conclusion."
  )

  res <- pair_paragraphs_by_similarity(p_old, p_new, time_budget_sec = 5.0)
  expect_equal(res$n_edited, 0L)
  expect_equal(res$n_added, 0L)
  expect_equal(res$n_removed, 0L)
  expect_equal(res$n_unpaired, 0L)
  expect_false(res$budget_exceeded)
})

test_that("v0.6.0: Deprecation migration scanner operates reliably with single-pass regex", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c(
    "# Sample analytical script",
    "x <- TempleCBE::sd.error(c(1, 2, 3))",
    "y <- clean_names(data.frame(a = 1))",
    "janitor::clean_names(data.frame(b = 2))"
  ), tmp)

  findings <- find_deprecated_calls(tmp)
  expect_s3_class(findings, "tbl_df")
  expect_equal(nrow(findings), 2L)
  expect_true("sd.error" %in% findings$old_name)
  expect_true("clean_names" %in% findings$old_name)
  expect_false(any(findings$call_type == "external_namespace"))
})
